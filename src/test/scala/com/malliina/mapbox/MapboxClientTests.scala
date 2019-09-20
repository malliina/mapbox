package com.malliina.mapbox

import java.nio.file.{Files, Paths}

import com.malliina.http.ResponseException
import com.malliina.values.Username
import org.scalatest.FunSuite
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class MapboxClientTests extends FunSuite {
  val client = MapboxClient.fromConf()

  val dynoStyle = StyleId("ck0jvmrkf12q21cln11l3v5wp")

  ignore("proper") {
    val finnishWaters = StyleId("cjgny1fjc008p2so90sbz8nbv")
    val s = await(client.style(finnishWaters))
    val bytes = Json.toBytes(s)
    Files.write(Paths.get("finnish-waters.json"), bytes)
  }

  ignore("list styles") {
    val s = await(client.styles)
//    val s = await(client.tilesets)
    println(Json.prettyPrint(s))
//    val bytes = Json.toBytes(s)
//    Files.write(Paths.get("dynamic.json"), bytes)
  }

  ignore("get style") {
    val s = await(client.style(dynoStyle))
//    println(Json.prettyPrint(s))
    val bytes = Json.toBytes(s)
    Files.write(Paths.get("dyno-fetched.json"), bytes)
  }

  ignore("get sprite") {
    write(client.sprite(dynoStyle), "sprite.json")
  }

  ignore("write png sprite") {
    val r = await(client.sprite(dynoStyle, Paths.get("sprite.png")))
    println(r)
  }

  def write(f: Future[JsValue], to: String) = {
    val json = await(f)
    val bytes = Json.toBytes(json)
    Files.write(Paths.get(to), bytes)
  }

  ignore("update dyno") {
    val json = Json.parse("""{
                            |      "layout": {
                            |        "visibility": "visible"
                            |      },
                            |      "type": "line",
                            |      "source": "composite",
                            |      "id": "vaylat",
                            |      "paint": {
                            |        "line-color": "hsl(122, 89%, 52%)"
                            |      },
                            |      "source-layer": "vaylat_1-13qzbd"
                            |    }""".stripMargin).as[JsObject]
    val id = StyleId("ck0jvmrkf12q21cln11l3v5wp")
    val s = await(client.style(id))
    try {
      val sources = Map("composite" -> StyleSource.vector(
        "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714,malliina.9si6bepc"))
      val old = s.as[UpdateStyle]
      val update = old.copy(sources = Option(sources))
      val updated = await(client.update(id, update))
      val bytes = Json.toBytes(updated)
      Files.write(Paths.get("dyno-updated.json"), bytes)
    } catch {
      case re: ResponseException =>
        println(re.response.asString)
    }

  }

  ignore("get style 2") {
    val id = StyleId("ck0jlem470onc1ckgaxtvlb8m")
    val s = await(client.style(id))
    val bytes = Json.toBytes(s)
    Files.write(Paths.get("updated.json"), bytes)
  }

  ignore("line delimit") {
    FeatureCollection.lineDelimit(Paths.get("data/avoin:vaylat.json"), Paths.get("data/vaylat-delimited.json"))
  }

  ignore("list tileset sources") {
    val response = await(client.tilesetSources())
    println(response)
  }

  ignore("create tileset source") {
    val src = Paths.get("data/avoin:vaylaalueet.json")
    val response = await(client.createTilesetSource(TilesetSourceId("vaylaalueet"), src))
    println(response)
  }

  ignore("end-to-end") {
    publishTileset(Seq("vaylaalueet", "vaylat"))
  }

  def publishTileset(names: Seq[String]): Unit = {
    val recipeMap = names.map { name =>
      val file = Paths.get(s"data/avoin:$name.json")
      val response = await(client.createTilesetSource(TilesetSourceId(name), file))
      SourceLayerId(name) -> LayerObject(response.id)
    }.toMap

    val tilesetName = TilesetName.random()
    val tilesetId = TilesetId.random(client.username, tilesetName)
    await(client.createTileset(tilesetId, TilesetSpec(tilesetName, Recipe(recipeMap))))
    val r = await(client.publish(tilesetId))
    println(r)
  }

  ignore("create tileset") {
    val name = TilesetName.random()
    val src = TilesetSourceId("mapbox://tileset-source/malliina/gnqfih")
    val sourceLayer = SourceLayerId("custom")
    val r = await(
      client.createTileset(TilesetId.random(Username("malliina"), name),
                           TilesetSpec(name, Recipe(Map(sourceLayer -> LayerObject(src))))))
    println(r)
  }

  val actualLayer = Json.parse("""{
      |      "layout": {
      |        "icon-image": "cardinal-east-30-opt",
      |        "icon-offset": [
      |          0,
      |          -15
      |        ]
      |      },
      |      "filter": [
      |        "==",
      |        "NAVL_TYYP",
      |        6
      |      ],
      |      "type": "symbol",
      |      "source": "composite",
      |      "id": "cardinal-east",
      |      "paint": {},
      |      "source-layer": "custom"
      |    }
      |""".stripMargin)

  ignore("serialize spec") {
    val spec = LayerSpec(
      LayerId("cardinal-east"),
      "symbol",
      ImageLayout(IconName("cardinal-east-30-opt"), Option(Seq(0, -15))),
      SourceId("composite"),
      Option(FilterSpec(Operator.Eq, "NAVL_TYYP", 6)),
      SourceLayerId("custom")
    )
    assert(Json.toJson(spec) === actualLayer)
  }

  ignore("add to style") {
//    val tileset = TilesetId("malliina.vndpjq")
    val spec = LayerSpec(
      LayerId("cardinal-east"),
      "symbol",
      ImageLayout(IconName("cardinal-east-30-opt"), Option(Seq(0, -15))),
      SourceId("composite"),
      Option(FilterSpec(Operator.Eq, "NAVL_TYYP", 6)),
      SourceLayerId("custom")
    )
    val layer = Json.parse("""
        |{
        |      "layout": {
        |        "icon-image": "cardinal-east-30-opt",
        |        "icon-offset": [
        |          0,
        |          -15
        |        ]
        |      },
        |      "filter": [
        |        "==",
        |        "NAVL_TYYP",
        |        6
        |      ],
        |      "type": "symbol",
        |      "source": "composite",
        |      "id": "cardinal-east",
        |      "paint": {},
        |      "source-layer": "custom"
        |    }
        |""".stripMargin).as[JsObject]
    val id = dynoStyle
    val s = await(client.style(id))
    val old = s.as[UpdateStyle]
//    val sources = Map("composite" -> StyleSource.vector(
//      "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714,malliina.vndpjq"))
    val update = old.copy(layers = old.layers.map { ls =>
      ls.init ++ Seq(layer)
    })
    val updated = await(client.update(id, update))
    val bytes = Json.toBytes(updated)
    Files.write(Paths.get("dyno-updated-more.json"), bytes)
  }

  ignore("publish tileset") {
//    val jobId = "ck0l7apz1000001qwespt5d5q"
    val tileset = TilesetId("malliina.vndpjq")
    val r = await(client.publish(tileset))
    println(r)
  }

  ignore("add image") {
    val req = client.addImage(IconName("cardinal-east-30-opt"), Paths.get(s"data/cardinal-east-30-opt.svg"), dynoStyle)
    val res = await(req)
    println(res)
  }

  def await[T](f: Future[T]) = Await.result(f, 60.seconds)

  val icons = Seq(
    "cardinal-north-30-opt",
    "cardinal-east-30-opt",
    "cardinal-south-30-opt",
    "cardinal-west-30-opt",
    "lateral-red-30-opt",
    "lateral-green-30-opt",
    "radar-15",
    "leading-beacon-15",
    "no-waves-15",
    "lighthouse-30-nolight",
    "lighthouse-30-yellow",
    "limit-10-30-normal",
    "lighthouse-15"
  )
}
