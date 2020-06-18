package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import org.apache.commons.io.FilenameUtils
import play.api.libs.json.{Json, Writes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StyleBuilderTests extends BaseSuite with ClientFixture {
  val playground = StyleId("ck0slgajf056s1cm68wcy5z7w")
  val tileset = TilesetId("malliina.klrhwc")

  http.test("print".ignore) { client =>
    val json = await(client.style(playground))
    out(json)
  }

  http.test("update recipe".ignore) { client =>
    val r = makeRecipeFromNames(Seq("vaylaalueet", "vaylat", "turvalaitteet", "rajoitusalue_a", "taululinja"), client)
    val old = await(client.recipe(tileset))
    await(client.updateRecipe(tileset, old.recipe ++ r))
  }

  http.test("custom update".ignore) { client =>
    val areas = (1 to 3).map { i => userHome.resolve(s"boat/syvyysalue_a$i.json") }
    val r = makeRecipeFromFiles(areas, client)
    val old = await(client.recipe(tileset))
    val up = await(client.updateRecipe(tileset, old.recipe ++ r))
  }

  http.test("current".ignore) { client =>
    val json = await(client.tilesetStatus(tileset))
    out(json)
  }

  http.test("publish".ignore) { client =>
    val json = await(client.startPublishJob(tileset))
    out(json)
  }

  def out[T: Writes](json: T, to: Path = Paths.get("out.json")): Unit = {
    val str = Json.prettyPrint(Json.toJson(json))
    Files.write(to, str.getBytes(StandardCharsets.UTF_8))
  }

  def publishTileset(names: Seq[String], client: MapboxClient): TilesetId = {
    val recipe = makeRecipeFromNames(names, client)
    val tilesetName = TilesetName.random()
    val tilesetId = TilesetId.apply(client.username, tilesetName)
    await(client.createTileset(tilesetId, TilesetSpec(tilesetName, recipe)))
    val r = await(client.startPublishJob(tilesetId))
    println(r)
    tilesetId
  }

  def makeRecipeFromFiles(files: Seq[Path], client: MapboxClient) = {
    val mappings = files.map { file => (file, FilenameUtils.removeExtension(file.getFileName.toString)) }
    makeRecipe(mappings, client)
  }

  def makeRecipeFromNames(names: Seq[String], client: MapboxClient) = {
    val mappings = names.map { name => (Paths.get(s"data/avoin-$name.json"), name) }
    makeRecipe(mappings, client)
  }

  def makeRecipe(files: Seq[(Path, String)], client: MapboxClient) = Recipe {
    files.map {
      case (file, name) =>
        val response = await(client.createTilesetSource(TilesetSourceId(name), file))
        SourceLayerId(name) -> LayerObject(response.id)
    }.toMap
  }

  def addImages(icons: Seq[IconName], to: StyleId, client: MapboxClient) = Future.traverse(icons) { icon =>
    client.addImage(icon, Paths.get(s"data/images/$icon.svg"), to)
  }
}
