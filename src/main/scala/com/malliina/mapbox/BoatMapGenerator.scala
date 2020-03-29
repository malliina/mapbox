package com.malliina.mapbox

import com.malliina.http.{FullUrl, ResponseException}
import com.malliina.mapbox.BoatMapGenerator._
import org.slf4j.LoggerFactory

import scala.concurrent.Future

object BoatMapGenerator {
  private val log = LoggerFactory.getLogger(getClass)

  def apply(source: SourceId = SourceId.random(), mapbox: MapboxClient = MapboxClient.fromConf()): BoatMapGenerator =
    new BoatMapGenerator(source, mapbox, GeoUtils(mapbox.http))

  class Urls(src: SourceId) extends BoatStyle(src) {
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
    val images = marks.styling.map(_.layout).collect { case ImageLayout(name, _, _) => name }
    val imageFiles = images.map { i =>
      ImageFile.orFail(i)
    }
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
    // https://docs.mapbox.com/api/maps/#styles
    // Layers will be drawn in the order of this sequence.
//    val all = Seq(fairwayAreas, fairways, limitAreas, leadingBeacons, depthLines, depthPoints, marks)
    val all = Seq(fairways, fairwayAreas, leadingBeacons, depthAreas, marks, depthPoints, limitAreas)
    val allTest = Seq(fairwayAreas, fairways, limitAreas)
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

class BoatMapGenerator(source: SourceId, val mapbox: MapboxClient, geo: GeoUtils) {
  import mapbox.http.exec

  val urls = new Urls(source)

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
      _ <- installImages(urls.imageFiles, target)
      styledRecipes <- recipes(urls.all)
      recipe = Recipe.merge(styledRecipes.map(_.recipe))
      tileset <- mapbox.createTileset(recipe)
      _ <- mapbox.updateRecipe(tileset, recipe)
      _ <- mapbox.publishAndAwait(tileset)
      _ <- mapbox.installSourceAndLayers(target, tileset, source, styledRecipes.flatMap(_.style))
    } yield tileset
    attempt.recoverWith {
      case re: ResponseException =>
        val res = re.response
        log.error(s"Failed to generate map to style '$target'. Status ${res.code}. Body '${res.asString}'.", re)
        Future.failed(re)
    }
  }

  private def installImages(images: Seq[ImageFile], to: StyleId) = Future.traverse(images) { image =>
    mapbox.addImage(image.image, image.file, to)
  }

  private def recipes(data: Seq[UrlTask] = urls.all): Future[Seq[StyledRecipe]] =
    Concurrent
      .traverseSlowly(data, parallelism = 1) { task =>
        shapeToRecipe(task)
      }

  private def shapeToRecipe(task: UrlTask): Future[StyledRecipe] =
    for {
      unzipped <- geo.shapeToGeoJson(task)
      layerFiles = unzipped.map(file => SourceLayerFile(file))
      recipe <- mapbox.makeRecipe(layerFiles)
    } yield {
      val styleSpecs = layerFiles.flatMap { slf =>
        task.styling.map { s =>
          s.toLayerSpec(LayerId(slf.sourceLayerId.value), slf.sourceLayerId)
        }
      }
      StyledRecipe(recipe, styleSpecs)
    }
}
