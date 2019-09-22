package com.malliina.mapbox

import com.malliina.http.FullUrl
import com.malliina.mapbox.BoatMapGenerator._
import org.slf4j.LoggerFactory

import scala.concurrent.Future

object BoatMapGenerator {
  private val log = LoggerFactory.getLogger(getClass)

  object urls {
    val fairwayAreas = shapeUrl("vaylaalueet")
    val fairways = shapeUrl("vaylat")
    val marks = shapeUrl("turvalaitteet")
    val limitAreas = shapeUrl("rajoitusalue_a")
    val leadingBeacons = shapeUrl("taululinja")
    val depthAreas = shapeUrl("syvyysalue_a", restricted = true, parts = 4)
    val depthLines = shapeUrl("syvyyskayra_v", restricted = true, parts = 2)
    val depthPoints = shapeUrl("syvyyspiste_p", restricted = true, parts = 4)

    val all = Seq(fairwayAreas, fairways, marks, limitAreas, leadingBeacons, depthAreas, depthLines, depthPoints)
  }

  case class UrlTask(url: FullUrl, parts: Int)

  def shapeUrl(name: String, restricted: Boolean = false, parts: Int = 1) = {
    val modifier = if (restricted) "rajoitettu" else "avoin"
    val url = FullUrl.https("julkinen.vayla.fi",
                            s"/inspirepalvelu/wfs?request=getfeature&typename=$modifier:$name&outputformat=shape-zip")
    UrlTask(url, parts)
  }

  def apply(mapbox: MapboxClient = MapboxClient.fromConf()): BoatMapGenerator =
    new BoatMapGenerator(mapbox, GeoUtils(mapbox.http))
}

class BoatMapGenerator(val mapbox: MapboxClient, geo: GeoUtils) {
  import mapbox.http.exec

  /** Adds nautical charts of Finnish waters to the provided Mapbox `style`.
    *
    * Uses data from Finnish Transport Agency.
    *
    * A new tileset ID is generated for each invocation.
    *
    * @param target style to apply the nautical charts to
    * @return the generated tileset ID
    */
  def generate(target: StyleId): Future[TilesetId] =
    for {
      recipe <- recipes()
      tileset <- mapbox.createTileset(recipe)
      _ <- mapbox.updateRecipe(tileset, recipe)
      _ <- mapbox.publish(tileset)
      _ <- mapbox.updateSource(BoatStyle.src, tileset, target)
      _ <- mapbox.updateLayer(target, BoatStyle.all)
    } yield tileset

  def recipes(): Future[Recipe] =
    Utils
      .traverseSlowly(urls.all, parallelism = 1) { task =>
        shapeToRecipe(task.url, task.parts)
      }
      .map(Recipe.merge)

  def shapeToRecipe(url: FullUrl, parts: Int): Future[Recipe] = {
    for {
      unzipped <- geo.shapeToRecipe(url, parts)
      recipe <- mapbox.recipeFromGeoJson(unzipped)
    } yield recipe
  }
}
