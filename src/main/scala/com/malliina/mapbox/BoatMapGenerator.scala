package com.malliina.mapbox

import com.malliina.http.{FullUrl, ResponseException}
import com.malliina.mapbox.BoatMapGenerator.*
import com.malliina.util.AppLogger

import java.nio.file.Path
import scala.concurrent.Future

object BoatMapGenerator:
  private val log = AppLogger(getClass)

  def apply(
    source: SourceId = SourceId.random(),
    mapbox: MapboxClient = MapboxClient.fromConf()
  ): BoatMapGenerator =
    new BoatMapGenerator(source, mapbox, GeoUtils(mapbox.http))

class BoatMapGenerator(source: SourceId, val mapbox: MapboxClient, geo: GeoUtils):
  import mapbox.http.exec

  val urls = Urls(source)

  def generate(name: String): Future[GeneratedMap] =
    generate(GenerateMapRequest(name, urls.all, urls.imageFiles, "boat-"))

  /** Generates a map with the content in `request`.
    *
    * The empty style template is obtained as follows:
    *
    * <ol> <li>Create a new Style in Mapbox's web UI using the Streets template.</li> <li>Load the
    * style's JSON using `MapboxClient.style(id)`. Use the test case to write it to a file
    * automatically.</li> </ol>
    *
    * @return
    *   style ID and tileset ID
    */
  def generate(request: GenerateMapRequest): Future[GeneratedMap] =
    val style = styleFromResource()
    val payload = style.copy(name = request.name)
    for
      style <- mapbox.createStyleTyped(payload)
      tileset <- installTo(style.id, request)
    yield GeneratedMap(style.id, tileset)

  def styleFromResource(): UpdateStyle =
    val template = Utils.resourceAsString("empty-streets-style.json")
//    val template = Utils.resourceAsString("streets-cl9a65qy1000814lbx2tctoq1.json")
    io.circe.parser
      .decode[UpdateStyle](template)
      .fold(err => throw Exception(s"Failed to parse JSON. ${err.getMessage}"), identity)

  /** Adds nautical charts of Finnish waters to the provided Mapbox `style`.
    *
    * Uses data from Finnish Transport Agency.
    *
    * A new tileset ID is generated for each invocation.
    *
    * @param target
    *   style to apply the nautical charts to
    * @return
    *   the generated tileset ID
    */
  def installTo(target: StyleId, request: GenerateMapRequest): Future[TilesetId] =
    val attempt = for
      _ <- installImages(request.images, target)
      styledRecipes <- recipes(request.urls)
      recipe = Recipe.merge(styledRecipes.map(_.recipe))
      tileset <- mapbox.createTileset(recipe, request.assetPrefix)
      _ <- mapbox.updateRecipe(tileset, recipe)
      _ <- mapbox.publishAndAwait(tileset)
      _ <- mapbox.installSourceAndLayers(
        target,
        tileset,
        source,
        styledRecipes.flatMap(_.style),
        draft = true
      )
      _ <- mapbox.installSourceAndLayers(
        target,
        tileset,
        source,
        styledRecipes.flatMap(_.style),
        draft = false
      )
    yield tileset
    attempt.recoverWith { case re: ResponseException =>
      val res = re.response
      log.error(
        s"Failed to generate map to style '$target'. Status ${res.code}. Body '${res.asString}'.",
        re
      )
      Future.failed(re)
    }

  private def installImages(images: Seq[ImageFile], to: StyleId) = Future.traverse(images) {
    image =>
      mapbox.addImage(image.image, image.file, to)
  }

  private def recipes(data: Seq[UrlTask]): Future[Seq[StyledRecipe]] =
    val filePrefix = Utils.randomString(6)
    Concurrent
      .traverseSlowly(data, parallelism = 1) { task =>
        shapeToRecipe(task, filePrefix)
      }

  private def shapeToRecipe(task: UrlTask, filePrefix: String): Future[StyledRecipe] =
    for
      unzipped <- geo.shapeToGeoJson(task, filePrefix)
      recipe <- geoJsonToRecipe(task.toFiles(unzipped))
    yield recipe

  private def geoJsonToRecipe(task: FileTask): Future[StyledRecipe] =
    val layerFiles = task.files.map(file => SourceLayerFile(file))
    for recipe <- mapbox.makeRecipe(layerFiles)
    yield
      val styleSpecs = layerFiles.zipWithIndex.flatMap { case (slf, idx) =>
        task.styling.map { s =>
          val layerId = if task.parts > 1 then LayerId(s"${task.name}-$idx") else LayerId(task.name)
          s.toLayerSpec(layerId, slf.sourceLayerId)
        }
      }
      StyledRecipe(recipe, styleSpecs)
