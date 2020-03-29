package com.malliina.mapbox

import java.nio.file.Path

import com.malliina.http.{FullUrl, ResponseException}
import com.malliina.mapbox.BoatMapGenerator._
import com.malliina.mapbox.BoatStyle._
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory

import scala.concurrent.Future

trait GeoTask {
  def url: FullUrl
  def parts: Int
}

/**
  * @param name name of shape asset to download; also the default layer ID
  * @param url shape zip URL
  * @param parts split factor
  * @param styling layer styles
  */
case class UrlTask(name: String, url: FullUrl, parts: Int, styling: Seq[LayerStyling]) extends GeoTask

case class SourceLayerFile(sourceLayerId: SourceLayerId, file: Path)

object SourceLayerFile {
  def apply(file: Path): SourceLayerFile = SourceLayerFile(
    SourceLayerId(FilenameUtils.removeExtension(file.getFileName.toString)),
    file
  )
}

object BoatMapGenerator {
  private val log = LoggerFactory.getLogger(getClass)

  def apply(mapbox: MapboxClient = MapboxClient.fromConf()): BoatMapGenerator =
    new BoatMapGenerator(mapbox, GeoUtils(mapbox.http))

  object urls {
    val fairwayAreas = shapeUrl("vaylaalueet")(
      vaylaAlueet
    )
    val fairways = shapeUrl("vaylat")(
      vaylat
    )
    val marks = shapeUrl("turvalaitteet")(
      safeWaters,
      speedLimit,
      kummeli,
      lateralRed,
      lateralGreen,
      cardinalNorth,
      cardinalSouth,
      cardinalWest,
      cardinalEast,
      radar,
      leadingBeacon,
      lighthouseNoLight,
      lighthouseYellow,
      sectorLight,
      noWaves
    )
    val limitAreas = shapeUrl("rajoitusalue_a")(
      limitArea
    )
    val leadingBeacons = shapeUrl("taululinja")(
      taululinja
    )
    val depthAreas = shapeUrl("syvyysalue_a", restricted = true, parts = 4)(
      depthAreaLayers
    )
    // No styling? Not used?
    val depthLines = shapeUrl("syvyyskayra_v", restricted = true, parts = 2)()
    val depthPoints = shapeUrl("syvyyspiste_p", restricted = true, parts = 4)(
      depthPointLayers
    )
    val all = Seq(fairwayAreas, fairways, marks, limitAreas, leadingBeacons, depthAreas, depthLines, depthPoints)
    val allTest = Seq(fairways)
    //    val demo = Seq(marks)
  }

  private def shapeUrl(name: String, restricted: Boolean = false, parts: Int = 1)(styling: LayerStyling*): UrlTask = {
    val modifier = if (restricted) "rajoitettu" else "avoin"
    val url = FullUrl.https(
      "julkinen.vayla.fi",
      s"/inspirepalvelu/wfs?request=getfeature&typename=$modifier:$name&outputformat=shape-zip"
    )
    UrlTask(name, url, parts, styling)
  }
}

class BoatMapGenerator(val mapbox: MapboxClient, geo: GeoUtils) {
  import mapbox.http.exec

  val urls = BoatMapGenerator.urls

  /** Adds nautical charts of Finnish waters to the provided Mapbox `style`.
    *
    * Uses data from Finnish Transport Agency.
    *
    * A new tileset ID is generated for each invocation.
    *
    * @param target style to apply the nautical charts to
    * @return the generated tileset ID
    */
  def generate(target: StyleId): Future[TilesetId] = {
    val attempt = for {
      styledRecipes <- recipes()
      recipe = Recipe.merge(styledRecipes.map(_.recipe))
      tileset <- mapbox.createTileset(recipe)
      _ <- mapbox.updateRecipe(tileset, recipe)
      _ <- mapbox.publishAndAwait(tileset)
      _ <- mapbox.installSourceAndLayers(target, tileset, BoatStyle.src, styledRecipes.flatMap(_.style))
    } yield tileset
    attempt.recoverWith {
      case re: ResponseException =>
        val res = re.response
        log.error(s"Failed to generate map to style '$target'. Status ${res.code}. Body '${res.asString}'.", re)
        Future.failed(re)
    }
  }

  def recipes(): Future[Seq[StyledRecipe]] =
    Concurrent
      .traverseSlowly(urls.allTest, parallelism = 1) { task =>
        shapeToRecipe(task)
      }

  def shapeToRecipe(task: UrlTask): Future[StyledRecipe] =
    for {
      unzipped <- geo.shapeToGeoJson(task)
      layerFiles = unzipped.map(file => SourceLayerFile(file))
      styleSpecs = layerFiles.flatMap(slf => task.styling.map(s => s.toLayerSpec(LayerId(task.name), slf.sourceLayerId))
      )
      recipe <- mapbox.makeRecipe(layerFiles)
    } yield StyledRecipe(recipe, styleSpecs)
}
