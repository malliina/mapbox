package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import com.malliina.mapbox.BoatStyle._
import org.apache.commons.io.FilenameUtils
import play.api.libs.json.{Json, Writes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StyleBuilderTests extends BaseSuite {
  val client = MapboxClient.fromConf()
  val playground = StyleId("ck0slgajf056s1cm68wcy5z7w")
  val tileset = TilesetId("malliina.klrhwc")

  ignore("end-to-end") {
    await(addImages(BoatStyle.icons.map(IconName.apply), playground))
    val tilesetId = publishTileset(Seq("vaylaalueet", "vaylat", "turvalaitteet", "rajoitusalue_a", "taululinja"))
    await(client.updateSource(src, tilesetId, playground))
    await(client.updateLayer(playground, all))
  }

  ignore("update src") {
    val ok = await(client.updateSource(src, tileset, playground))
    out(ok)
  }

  test("update layers") {
    val layerUpdates = await(client.updateLayer(playground, BoatStyle.all))
    out(layerUpdates)
  }

  ignore("print") {
    val json = await(client.style(playground))
    out(json)
  }

  ignore("update recipe") {
    val r = makeRecipeFromNames(Seq("vaylaalueet", "vaylat", "turvalaitteet", "rajoitusalue_a", "taululinja"))
    val old = await(client.recipe(tileset))
    await(client.updateRecipe(tileset, old.recipe ++ r))
  }

  ignore("custom update") {
    val areas = (1 to 3).map { i =>
      userHome.resolve(s"boat/syvyysalue_a$i.json")
    }
    val r = makeRecipeFromFiles(areas)
    val old = await(client.recipe(tileset))
    val up = await(client.updateRecipe(tileset, old.recipe ++ r))
  }

  ignore("current") {
    val json = await(client.tileset(tileset))
    out(json)
  }

  ignore("publish") {
    val json = await(client.publish(tileset))
    out(json)
  }

  def out[T: Writes](json: T, to: Path = Paths.get("out.json")): Unit = {
    val str = Json.prettyPrint(Json.toJson(json))
    Files.write(to, str.getBytes(StandardCharsets.UTF_8))
  }

  def publishTileset(names: Seq[String]): TilesetId = {
    val recipe = makeRecipeFromNames(names)
    val tilesetName = TilesetName.random()
    val tilesetId = TilesetId.random(client.username, tilesetName)
    await(client.createTileset(tilesetId, TilesetSpec(tilesetName, recipe)))
    val r = await(client.publish(tilesetId))
    println(r)
    tilesetId
  }

  def makeRecipeFromFiles(files: Seq[Path]) = {
    val mappings = files.map { file =>
      (file, FilenameUtils.removeExtension(file.getFileName.toString))
    }
    makeRecipe(mappings)
  }

  def makeRecipeFromNames(names: Seq[String]) = {
    val mappings = names.map { name =>
      (Paths.get(s"data/avoin-$name.json"), name)
    }
    makeRecipe(mappings)
  }

  def makeRecipe(files: Seq[(Path, String)]) = Recipe {
    files.map {
      case (file, name) =>
        val response = await(client.createTilesetSource(TilesetSourceId(name), file))
        SourceLayerId(name) -> LayerObject(response.id)
    }.toMap
  }

  def addImages(icons: Seq[IconName], to: StyleId) = Future.traverse(icons) { icon =>
    client.addImage(icon, Paths.get(s"data/images/$icon.svg"), to)
  }

  override protected def afterAll(): Unit = {
    client.close()
  }
}
