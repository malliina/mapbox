package com.malliina.mapbox

import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.concurrent.TimeUnit

import com.malliina.http.{FullUrl, OkClient, OkHttpResponse}
import com.malliina.mapbox.MapboxClient.log
import com.malliina.values.{AccessToken, Username}
import com.typesafe.config.ConfigFactory
import okhttp3._
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, Json, Reads, Writes}

import scala.concurrent.Future

object MapboxClient {
  private val log = LoggerFactory.getLogger(getClass)

  val confFile = Paths.get(sys.props("user.home")).resolve(".mapbox/mapbox.conf")

  def apply(token: AccessToken) = new MapboxClient(token)

  def fromConf() = {
    val conf = ConfigFactory.parseFile(confFile.toFile).resolve()
    val token = AccessToken(conf.getString("mapbox.studio.token"))
    apply(token)
  }
}

class MapboxClient(token: AccessToken) {
  val username = Username("malliina")

  val textPlain = MediaType.parse("text/plain")
  val svgXml = MediaType.parse("image/svg+xml")

  val http = OkClient(
    new OkHttpClient.Builder()
      .connectionSpecs(util.Arrays.asList(ConnectionSpec.MODERN_TLS, ConnectionSpec.COMPATIBLE_TLS))
      .connectTimeout(30, TimeUnit.SECONDS)
      .readTimeout(1800, TimeUnit.SECONDS)
      .writeTimeout(1800, TimeUnit.SECONDS)
      .build())
  val geo = GeoUtils(http)
  import http.exec

  def apiUrl(path: String, params: Map[String, String] = Map.empty) =
    FullUrl
      .https("api.mapbox.com", path)
      .withQuery("access_token" -> token.token)
      .withQuery(params.toList: _*)

  def style(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id")
  def styleTyped(id: StyleId) = get[UpdateStyle](s"/styles/v1/$username/$id")
  def styles = get[JsValue](s"/styles/v1/$username")
  def createStyle(style: JsValue) = http.postJsonAs[JsValue](apiUrl(s"/styles/v1/$username"), style)

  def sources = get[JsValue](s"/tilesets/v1/sources/$username")
  def tilesets = get[JsValue](s"/tilesets/v1/$username")

  def recipeFromGeoJson(geoJsons: Seq[Path]): Future[Recipe] = {
    val mappings = geoJsons.map { file =>
      (file, FilenameUtils.removeExtension(file.getFileName.toString))
    }
    makeRecipe(mappings)
  }

  def makeRecipe(mappings: Seq[(Path, String)]): Future[Recipe] =
    Utils
      .traverseSlowly(mappings, parallelism = 1) {
        case (file, name) =>
          createTilesetSource(TilesetSourceId(name), file).map { response =>
            log.info(s"Created tileset source '${response.id}' from '$file'.")
            SourceLayerId(name) -> LayerObject(response.id)
          }
      }
      .map(pairs => Recipe(pairs.toMap))

  def updateSource(source: SourceId, tileset: TilesetId, to: StyleId) =
    for {
      s <- styleTyped(to)
      r <- update(to, s.updated(source, tileset))
    } yield r

  def updateLayer(style: StyleId, newLayers: Seq[LayerSpec]) =
    transformLayers(style)(_.updated(newLayers))

  def removeLayer(layer: LayerId, from: StyleId): Future[JsValue] =
    transformLayers(from)(_.withoutLayer(layer))

  def transformLayers(style: StyleId)(f: UpdateStyle => UpdateStyle) = for {
    old <- styleTyped(style)
    updated <- update(style, f(old))
  } yield updated

  def update(style: StyleId, updated: UpdateStyle) =
    updateJson(style, Json.toJson(updated))

  def updateJson(style: StyleId, updated: JsValue) =
    patch[JsValue, JsValue](apiUrl(s"/styles/v1/$username/$style"), updated)

  def tilesetSources() = get[JsValue](s"/tilesets/v1/sources/$username")

  def delete(id: TilesetSourceId) = {
    val url = apiUrl(s"/tilesets/v1/sources/$username/$id")
    val req = new Request.Builder().url(url.url).delete().build()
    http.execute(req)
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
    post[TilesetSourceCreated](url, body)
  }

  def createTileset(recipe: Recipe): Future[TilesetId] = {
    val tilesetName = TilesetName.random()
    val tilesetId = TilesetId.random(username, tilesetName)
    createTileset(tilesetId, TilesetSpec(tilesetName, recipe)).map { _ =>
      log.info(s"Created tileset '$tilesetId'.")
      tilesetId
    }
  }

  def createTileset(id: TilesetId, spec: TilesetSpec): Future[JsValue] =
    http.postJsonAs[JsValue](apiUrl(s"/tilesets/v1/$id"), Json.toJson(spec))

  def recipe(id: TilesetId) = get[RecipeResponse](s"/tilesets/v1/$id/recipe")
  def updateRecipe(id: TilesetId, recipe: Recipe): Future[OkHttpResponse] =
    patchEmpty(apiUrl(s"/tilesets/v1/$id/recipe"), recipe)

  def tileset(id: TilesetId) = get[TilesetStatus](s"/tilesets/v1/$id/status")
  def publish(id: TilesetId) =
    post[PublishResponse](apiUrl(s"/tilesets/v1/$id/publish"), RequestBody.create("", textPlain))

  def sprite(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id/sprite")
  def sprite(id: StyleId, to: Path) = http.download(apiUrl(s"/styles/v1/$username/$id/sprite@2x.png"), to)

  /** Images used in a style must be added to its sprite.
    */
  def addImage(icon: IconName, svg: Path, to: StyleId) =
    put[JsValue](apiUrl(s"/styles/v1/$username/$to/sprite/$icon"), RequestBody.create(svg.toFile, svgXml))

  def get[R: Reads](path: String) = http.getAs[R](apiUrl(path))

  def put[R: Reads](url: FullUrl, body: RequestBody) = {
    val req = new Request.Builder().url(url.url).put(body).build()
    http.execute(req).flatMap { r =>
      http.parse[R](r, url)
    }
  }

  def post[R: Reads](url: FullUrl, body: RequestBody) =
    http.post(url, body, Map.empty).flatMap { r =>
      http.parse[R](r, url)
    }

  def patchEmpty[W: Writes](url: FullUrl, w: W) = {
    val body = RequestBody.create(Json.stringify(Json.toJson(w)), OkClient.jsonMediaType)
    val req = new Request.Builder().url(url.url).patch(body).build()
    http.execute(req)
  }

  def patch[W: Writes, R: Reads](url: FullUrl, w: W) =
    patchEmpty(url, w).flatMap { r =>
      http.parse[R](r, url)
    }

  def close(): Unit = http.close()
}
