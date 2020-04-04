package com.malliina.mapbox

import com.malliina.http.{FullUrl, ResponseException}
import com.malliina.mapbox.BoatMapGenerator._
import org.slf4j.LoggerFactory
import play.api.libs.json.Json

import scala.concurrent.Future

object BoatMapGenerator {
  private val log = LoggerFactory.getLogger(getClass)

  def apply(source: SourceId = SourceId.random(), mapbox: MapboxClient = MapboxClient.fromConf()): BoatMapGenerator =
    new BoatMapGenerator(source, mapbox, GeoUtils(mapbox.http))

  class Urls(src: SourceId) extends BoatStyle(src) {
    val fairwayAreas = shapeUrl("vaylaalueet")(vaylaAlueet)
    val fairways = shapeUrl("vaylat")(vaylat)
    val marks = shapeUrl("turvalaitteet")(
      safeWaters,
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
      sectorLight
    )
    val trafficSigns = shapeUrl("vesiliikennemerkit")(
      noWaves
//      speedLimit
    )
    val limitAreas = shapeUrl("rajoitusalue_a")(limitArea)
    val leadingBeacons = shapeUrl("taululinja")(taululinja)
    val depthAreas = shapeUrl("syvyysalue_a", restricted = true, parts = 4)(depthAreaLayers)
    val areaLimits = shapeUrl("aluemeri_raja_a")(aluemeriRaja)
    // No styling? Not used?
//    val depthLines = shapeUrl("syvyyskayra_v", restricted = true, parts = 2)()
    val depthPoints = shapeUrl("syvyyspiste_p", restricted = true, parts = 4)(
      depthPointLayers
    )
    // https://docs.mapbox.com/api/maps/#styles
    // Layers will be drawn in the order of this sequence.
    val all =
      Seq(fairways, fairwayAreas, leadingBeacons, depthAreas, areaLimits, limitAreas, depthPoints, marks, trafficSigns)
    val layoutImages =
      all.flatMap(_.styling).map(_.layout).collect { case ImageLayout(name, _, _) => name }
    val appImages = Seq("boat-resized-opt-30", "trophy-gold-path").map(IconName.apply)
    val imageFiles = (layoutImages ++ appImages).map { i =>
      ImageFile.orFail(i)
    }
  }

  private def shapeUrl(name: String, restricted: Boolean = false, parts: Int = 1)(styling: LayerStyling*): UrlTask = {
    val url = shapeZipUrl(name, restricted)
    UrlTask(name, url, parts, styling)
  }

  def shapeZipUrl(name: String, restricted: Boolean): FullUrl = {
    val modifier = if (restricted) "rajoitettu" else "avoin"
    FullUrl.https(
      "julkinen.vayla.fi",
      s"/inspirepalvelu/wfs?request=getfeature&typename=$modifier:$name&outputformat=shape-zip"
    )
  }
}

class BoatMapGenerator(source: SourceId, val mapbox: MapboxClient, geo: GeoUtils) {
  import mapbox.http.exec

  val urls = new Urls(source)

  def generate(name: String): Future[GeneratedMap] =
    generate(GenerateMapRequest(name, urls.all, urls.imageFiles, "boat-"))

  /** Generates a map with the content in `request`.
    *
    * The empty style template is obtained as follows:
    *
    * <ol>
    *   <li>Create a new Style in Mapbox's web UI using the Streets template.</li>
    *   <li>Load the style's JSON using `MapboxClient.style(id)`. Use the test case
    *   to write it to a file automatically.</li>
    * </ol>
    *
    * @return style ID and tileset ID
    */
  def generate(request: GenerateMapRequest): Future[GeneratedMap] = {
    val template = Utils.resourceAsString("empty-streets-style.json")
    val payload = Json.parse(template).as[UpdateStyle].copy(name = request.name)
    for {
      style <- mapbox.createStyleTyped(payload)
      tileset <- installTo(style.id, request)
    } yield GeneratedMap(style.id, tileset)
  }

  /** Adds nautical charts of Finnish waters to the provided Mapbox `style`.
    *
    * Uses data from Finnish Transport Agency.
    *
    * A new tileset ID is generated for each invocation.
    *
    * @param target style to apply the nautical charts to
    * @return the generated tileset ID
    */
  def installTo(target: StyleId, request: GenerateMapRequest): Future[TilesetId] = {
    val attempt = for {
      _ <- installImages(request.images, target)
      styledRecipes <- recipes(request.urls)
      recipe = Recipe.merge(styledRecipes.map(_.recipe))
      tileset <- mapbox.createTileset(recipe, request.assetPrefix)
      _ <- mapbox.updateRecipe(tileset, recipe)
      _ <- mapbox.publishAndAwait(tileset)
      _ <- mapbox.installSourceAndLayers(target, tileset, source, styledRecipes.flatMap(_.style), draft = true)
      _ <- mapbox.installSourceAndLayers(target, tileset, source, styledRecipes.flatMap(_.style), draft = false)
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

  private def recipes(data: Seq[UrlTask]): Future[Seq[StyledRecipe]] = {
    val filePrefix = Utils.randomString(6)
    Concurrent
      .traverseSlowly(data, parallelism = 1) { task =>
        shapeToRecipe(task, filePrefix)
      }
  }

  private def shapeToRecipe(task: UrlTask, filePrefix: String): Future[StyledRecipe] =
    for {
      unzipped <- geo.shapeToGeoJson(task, filePrefix)
      layerFiles = unzipped.map(file => SourceLayerFile(file))
      recipe <- mapbox.makeRecipe(layerFiles)
    } yield {
      val styleSpecs = layerFiles.zipWithIndex.flatMap {
        case (slf, idx) =>
          task.styling.map { s =>
            val layerId = if (task.parts > 1) LayerId(s"${task.name}-$idx") else LayerId(task.name)
            s.toLayerSpec(layerId, slf.sourceLayerId)
          }
      }
      StyledRecipe(recipe, styleSpecs)
    }
}
