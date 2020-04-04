package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.concurrent.TimeUnit

import concurrent.duration.{DurationLong, FiniteDuration}
import com.malliina.http.{FullUrl, OkClient, OkHttpResponse, ResponseException}
import com.malliina.mapbox.MapboxClient.log
import com.malliina.values.{AccessToken, Username}
import com.typesafe.config.ConfigFactory
import okhttp3._
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, Json, Reads, Writes}

import scala.concurrent.{Future, TimeoutException}

object MapboxClient {
  private val log = LoggerFactory.getLogger(getClass)

  val confFile = Paths.get(sys.props("user.home")).resolve(".mapbox/mapbox.conf")

  def apply(token: AccessToken) = new MapboxClient(token)

  def fromConf(): MapboxClient = {
    val conf = ConfigFactory.parseFile(confFile.toFile).resolve()
    val tokenStr = sys.env.getOrElse("MAPBOX_SECRET_TOKEN", conf.getString("mapbox.studio.token"))
    apply(AccessToken(tokenStr))
  }

  def mapboxHttpClient(): OkClient = OkClient(
    new OkHttpClient.Builder()
      .connectionSpecs(util.Arrays.asList(ConnectionSpec.MODERN_TLS, ConnectionSpec.COMPATIBLE_TLS))
      .connectTimeout(30, TimeUnit.SECONDS)
      .readTimeout(1800, TimeUnit.SECONDS)
      .writeTimeout(1800, TimeUnit.SECONDS)
      .build()
  )
}

class MapboxClient(token: AccessToken, val username: Username = Username("malliina")) {
  val textPlain = MediaType.parse("text/plain")
  val svgXml = MediaType.parse("image/svg+xml")

  val http = MapboxClient.mapboxHttpClient()
  val geo = GeoUtils(http)
  import http.exec

  def apiUrl(path: String, params: Map[String, String] = Map.empty): FullUrl =
    FullUrl
      .https("api.mapbox.com", path)
      .withQuery("access_token" -> token.token)
      .withQuery(params.toList: _*)

  def style(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id")
  def styleTyped(id: StyleId) = get[UpdateStyle](s"/styles/v1/$username/$id")
  def styles = get[JsValue](s"/styles/v1/$username")
  def stylesTyped = get[Seq[Style]](s"/styles/v1/$username")
  def createStyleTyped(style: UpdateStyle): Future[FullStyle] = createStyle(Json.toJson(style))
  def createStyle(style: JsValue) = http.postJsonAs[FullStyle](apiUrl(s"/styles/v1/$username"), style).map { r =>
    log.info(s"Created style '${r.id}'.")
    r
  }
  def deleteStyle(id: StyleId) = deleteUrl(apiUrl(s"/styles/v1/$username/$id"))

  def sources = get[JsValue](s"/tilesets/v1/sources/$username")

  def tilesets = get[JsValue](s"/tilesets/v1/$username")

  def makeRecipe(geoJsons: Seq[SourceLayerFile]): Future[Recipe] =
    Concurrent
      .traverseSlowly(geoJsons, parallelism = 1) { file =>
        val name = FilenameUtils.removeExtension(file.file.getFileName.toString)
        createTilesetSource(TilesetSourceId(name), file.file).map { response =>
          log.info(s"Created tileset source '${response.id}' from '${file.file}'.")
          file.sourceLayerId -> LayerObject(response.id)
        }
      }
      .map(pairs => Recipe(pairs.toMap))

  def updateSource(source: SourceId, tileset: TilesetId, to: StyleId): Future[JsValue] =
    for {
      s <- styleTyped(to)
      r <- updateStyle(to, s.withSource(source, tileset))
    } yield {
      log.info(s"Updated source '$source' with tileset '$tileset' in style '$to'.")
      r
    }

  def updateLayer(style: StyleId, newLayers: Seq[LayerSpec], retries: Int = 1): Future[JsValue] = {
    transformStyle(style)(_.withLayers(newLayers)).recoverWith {
      case re: ResponseException =>
        val code = re.response.code
        if (re.response.code == 422 && retries > 0) {
          log.info(s"Style update returned $code, retrying soon...")
          Concurrent.scheduleIn(10.seconds) {
            updateLayer(style, newLayers, retries - 1)
          }
        } else {
          Future.failed(re)
        }
    }
  }

  def installSourceAndLayers(
    style: StyleId,
    tilesetId: TilesetId,
    sourceId: SourceId,
    newLayers: Seq[LayerSpec],
    draft: Boolean
  ): Future[JsValue] =
    transformStyle(style) { old =>
      val json = Json.toJson(newLayers)
      log.info(s"Installing source '$sourceId' with tileset '$tilesetId' and layers '$json' to style '$style'.")
      old.withSource(sourceId, tilesetId).withLayers(newLayers).copy(draft = draft)
    }

  def removeLayer(layer: LayerId, from: StyleId): Future[JsValue] =
    transformStyle(from)(_.withoutLayer(layer)).map { r =>
      log.info(s"Removed layer '$layer' from style '$from'.")
      r
    }

  def transformStyle(style: StyleId)(f: UpdateStyle => UpdateStyle): Future[JsValue] =
    for {
      old <- styleTyped(style)
      updated <- updateStyle(style, f(old))
    } yield {
      updated
    }

  def updateStyle(style: StyleId, updated: UpdateStyle): Future[JsValue] =
    updateStyleJson(style, Json.toJson(updated)).map { r =>
      val path = Paths.get(s"style-${System.currentTimeMillis()}.json")
      Files.write(
        path,
        Json.prettyPrint(r).getBytes(StandardCharsets.UTF_8)
      )
      log.info(s"Updated style '$style'. Wrote to '${path.toAbsolutePath}'.")
      r
    }

  def updateStyleJson(style: StyleId, updated: JsValue): Future[JsValue] = {
    log.info(s"Updating style '$style'...")
    patch[JsValue, JsValue](apiUrl(s"/styles/v1/$username/$style"), updated)
  }

  def tilesetSources() = get[JsValue](s"/tilesets/v1/sources/$username")

  def delete(id: TilesetSourceId) = deleteUrl(apiUrl(s"/tilesets/v1/sources/$username/$id"))

  def deleteUrl(url: FullUrl) = {
    val req = new Request.Builder().url(url.url).delete().build()
    http.execute(req).map { r =>
      val noQuery = url.url.takeWhile(_ != '?')
      log.info(s"Deleted '$noQuery'.")
      r
    }
  }

  def createTilesetSource(id: TilesetSourceId, geoJson: Path): Future[TilesetSourceCreated] = {
    val tempFile = Files.createTempFile(s"${geoJson.getFileName}", ".ld")
    FeatureCollection.lineDelimit(geoJson, tempFile)
    val url = apiUrl(s"/tilesets/v1/sources/$username/$id")
    log.info(s"Created temporary tileset source file '$tempFile'. Uploading to '$url'...")
    val body = new MultipartBody.Builder()
      .setType(MultipartBody.FORM)
      .addFormDataPart("file", "tileset.json.ld", RequestBody.create(tempFile.toFile, textPlain))
      .build()
    post[TilesetSourceCreated](url, body).map { r =>
      log.info(s"Created tileset source '$id' from '$tempFile'.")
      r
    }
  }

  def createTileset(recipe: Recipe, assetPrefix: String): Future[TilesetId] = {
    val tilesetName = TilesetName.random(assetPrefix)
    val tilesetId = TilesetId(username, tilesetName)
    createTileset(tilesetId, TilesetSpec(tilesetName, recipe)).map { _ =>
      log.debug(s"Created tileset '$tilesetId' with name '$tilesetName'.")
      tilesetId
    }
  }

  def createTileset(id: TilesetId, spec: TilesetSpec): Future[JsValue] = {
    val payload = Json.toJson(spec)
    http
      .postJsonAs[JsValue](apiUrl(s"/tilesets/v1/$id"), payload)
      .recoverWith {
        case re: ResponseException =>
          val res = re.response
          log.error(
            s"Failed to create tileset '$id' from '$payload'. Posted '$payload'. Code ${res.code}. Body '${res.asString}'.",
            re
          )
          Future.failed(re)
      }
      .map { r =>
        log.info(s"Created tileset '$id' named '${spec.name}' with ${spec.recipe.layers.size} layers.")
        r
      }
  }

  def deleteTileset(id: TilesetId): Future[OkHttpResponse] = deleteUrl(apiUrl(s"/tilesets/v1/$id"))

  def recipe(id: TilesetId) = get[RecipeResponse](s"/tilesets/v1/$id/recipe")

  def updateRecipe(id: TilesetId, recipe: Recipe): Future[OkHttpResponse] =
    patchEmpty(apiUrl(s"/tilesets/v1/$id/recipe"), recipe).map { r =>
      log.info(s"Updated recipe of tileset '$id'.")
      r
    }

  def tilesetStatus(id: TilesetId) = get[TilesetStatus](s"/tilesets/v1/$id/status")
  def tilesetJobStatus(tid: TilesetId, jid: JobId) = get[JobStatusResponse](s"/tilesets/v1/$tid/jobs/$jid")

  def publishAndAwait(id: TilesetId) = startPublishJob(id).flatMap { res =>
    awaitCompletion(id, res.jobId, 30.minutes)
  }

  def awaitCompletion(tileset: TilesetId, job: JobId, timeout: FiniteDuration): Future[JobStatusResponse] = {
    val start = System.currentTimeMillis()
    tilesetJobStatus(tileset, job).flatMap { res =>
      val status = res.stage
      if (status.isCompleted) {
        log.info(s"Job '$job' of tileset '$tileset' completed.")
        Future.successful(res)
      } else if (timeout.toMillis > 0) {
        log.debug(s"Status of '$job' is '$status', awaiting...")
        Concurrent.scheduleIn(10.seconds) {
          val now = System.currentTimeMillis()
          awaitCompletion(tileset, job, timeout - (now - start).millis)
        }
      } else {
        Future.failed(
          new TimeoutException(
            s"Timed out awaiting completion of job '$job' for tileset '$tileset'. Last known status was '$status'."
          )
        )
      }
    }
  }

  def startPublishJob(id: TilesetId): Future[PublishResponse] =
    post[PublishResponse](apiUrl(s"/tilesets/v1/$id/publish"), RequestBody.create("", textPlain)).map { r =>
      log.info(s"Started publishing tileset '$id'. Got job ID '${r.jobId}'...")
      r
    }

  def sprite(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id/sprite")
  def sprite(id: StyleId, to: Path) = http.download(apiUrl(s"/styles/v1/$username/$id/sprite@2x.png"), to)

  /** Images used in a style must be added to its sprite.
    */
  def addImage(icon: IconName, svg: Path, to: StyleId) =
    put[JsValue](apiUrl(s"/styles/v1/$username/$to/sprite/$icon"), RequestBody.create(svg.toFile, svgXml)).map { r =>
      log.info(s"Added icon '$icon' from '$svg' to style '$to'.")
      r
    }

  def get[R: Reads](path: String) = http.getAs[R](apiUrl(path))

  def put[R: Reads](url: FullUrl, body: RequestBody) = {
    val req = new Request.Builder().url(url.url).put(body).build()
    http.execute(req).flatMap { r =>
      http.parse[R](r, url)
    }
  }

  def post[R: Reads](url: FullUrl, body: RequestBody, retries: Int = 1): Future[R] =
    http
      .post(url, body, Map.empty)
      .flatMap { r =>
        http.parse[R](r, url)
      }
      .recoverWith {
        case re: ResponseException =>
          if (retries > 0) {
            val times = if (retries == 1) "time" else "times"
            log.warn(s"Failed to POST to '$url'. Retrying $retries $times...")
            post(url, body, retries - 1)
          } else {
            Future.failed(re)
          }
      }

  def patchEmpty[W: Writes](url: FullUrl, w: W): Future[OkHttpResponse] = {
    val body = RequestBody.create(Json.stringify(Json.toJson(w)), OkClient.jsonMediaType)
    val req = new Request.Builder().url(url.url).patch(body).build()
    http.execute(req)
  }

  def patch[W: Writes, R: Reads](url: FullUrl, w: W): Future[R] =
    patchEmpty(url, w).flatMap { r =>
      http.parse[R](r, url)
    }

  def close(): Unit = http.close()
}
