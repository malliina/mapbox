package com.malliina.mapbox

import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.concurrent.TimeUnit

import com.malliina.http.{FullUrl, OkClient}
import com.malliina.mapbox.MapboxClient.log
import com.malliina.values.{AccessToken, Username}
import com.typesafe.config.ConfigFactory
import okhttp3._
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, Json, Reads}

import scala.concurrent.Future

object MapboxClient {
  private val log = LoggerFactory.getLogger(getClass)

  val confFile = Paths.get(sys.props("user.home")).resolve(".mapbox/mapbox.conf")

  val publicToken = AccessToken(
    "pk.eyJ1IjoibWFsbGlpbmEiLCJhIjoiY2pnbnVmbnBwMXpzYTJ3cndqajh2anFmaSJ9.2a0q5s3Tre_4_KFeuCB7iQ")
  def apply(token: AccessToken) = new MapboxClient(token)
  def public = apply(publicToken)
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
      .readTimeout(30, TimeUnit.SECONDS)
      .writeTimeout(30, TimeUnit.SECONDS)
      .build())
  import http.exec

  def apiUrl(path: String, params: Map[String, String] = Map.empty) =
    FullUrl
      .https("api.mapbox.com", path)
      .withQuery("access_token" -> token.token)
      .withQuery(params.toList: _*)

  def style(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id")
  def styles = get[JsValue](s"/styles/v1/$username")
  def sources = get[JsValue](s"/tilesets/v1/sources/$username")
  def tilesets = get[JsValue](s"/tilesets/v1/$username")

  def update(style: StyleId, updated: UpdateStyle) =
    updateJson(style, Json.toJson(updated))

  def updateJson(style: StyleId, updated: JsValue) = {
    val url = apiUrl(s"/styles/v1/$username/$style")
    val body = RequestBody.create(Json.stringify(updated), OkClient.jsonMediaType)
    val req = new Request.Builder().url(url.url).patch(body).build()
    http.execute(req).flatMap { response =>
      http.parse[JsValue](response, url)
    }
  }

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
    http.post(url, body, Map.empty).flatMap { r =>
      http.parse[TilesetSourceCreated](r, url)
    }
  }

  def createTileset(id: TilesetId, spec: TilesetSpec) = {
    http.postJsonAs[JsValue](apiUrl(s"/tilesets/v1/$id"), Json.toJson(spec))
  }

  def publish(id: TilesetId) = {
    val url = apiUrl(s"/tilesets/v1/$id/publish")
    http.post(url, RequestBody.create("", textPlain), Map.empty).flatMap { r =>
      http.parse[JsValue](r, url)
    }
  }

  def sprite(id: StyleId) = get[JsValue](s"/styles/v1/$username/$id/sprite")
  def sprite(id: StyleId, to: Path) = http.download(apiUrl(s"/styles/v1/$username/$id/sprite@2x.png"), to)

  /** Images used in a style must be added to its sprite.
    */
  def addImage(icon: IconName, svg: Path, to: StyleId) = {
    val url = apiUrl(s"/styles/v1/$username/$to/sprite/$icon")
    val body = RequestBody.create(svg.toFile, svgXml)
    val req = new Request.Builder().url(url.url).put(body).build()
    http.execute(req).flatMap { r =>
      http.parse[JsValue](r, url)
    }
  }

  def get[R: Reads](path: String) = http.getAs[R](apiUrl(path))
}
