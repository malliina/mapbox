package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.malliina.http.ResponseException
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

import scala.concurrent.Future

class MapboxClientTests extends BaseSuite {
  val client = MapboxClient.fromConf()

  val dynoStyle = StyleId("ck0jvmrkf12q21cln11l3v5wp") // Dyno

  ignore("proper") {
    val finnishWaters = StyleId("cjgny1fjc008p2so90sbz8nbv")
    val s = await(client.style(finnishWaters))
    write(s, "finnish-waters.json")
  }

  ignore("list styles") {
    val s = await(client.stylesTyped)
//    println(Json.prettyPrint(s))
    println(s.mkString("\n"))
    //    val bytes = Json.toBytes(s)
    //    Files.write(Paths.get("dynamic.json"), bytes)
  }

  test("get style") {
    val styleId = StyleId("ck8c7vg9g16to1ippuj3j7gqt")
//    val styleId = StyleId("ck8c8hro301921ik9c5eu5xbv")
    val s = await(client.style(styleId))
    //    println(Json.prettyPrint(s))
    write(s, s"style-$styleId.json")
  }

  ignore("get sprite") {
    write(client.sprite(dynoStyle), "sprite.json")
  }

  ignore("write png sprite") {
    val r = await(client.sprite(dynoStyle, Paths.get("sprite.png")))
    println(r)
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
      val sources = Map(
        "composite" -> StyleSource.vector(
          "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714,malliina.9si6bepc"
        )
      )
      val old = s.as[UpdateStyle]
      val update = old.copy(sources = Option(sources))
      val updated = await(client.updateStyle(id, update))
      write(updated, "dyno-updated.json")
    } catch {
      case re: ResponseException =>
        println(re.response.asString)
    }

  }

  ignore("get style 2") {
    val id = StyleId("ck0jlem470onc1ckgaxtvlb8m")
    val s = await(client.style(id))
    write(s, "updated.json")
  }

  ignore("line delimit") {
    FeatureCollection.lineDelimit(Paths.get("data/avoin:vaylat.json"), Paths.get("data/vaylat-delimited.json"))
  }

  ignore("list tileset sources") {
    val response = await(client.tilesetSources())
    println(response)
  }

  ignore("create tileset source") {
    val src = userHome.resolve("boat/syvyysalue_a0.json")
//    val src = Paths.get("data/avoin:vaylaalueet.json")
    val response = await(client.createTilesetSource(TilesetSourceId("syvyysalue_a0"), src))
    println(response)
  }

  ignore("create tileset") {
    val name = TilesetName.random()
    val src = TilesetSourceId("mapbox://tileset-source/malliina/gnqfih")
    val sourceLayer = SourceLayerId("custom")
    val r = await(
      client.createTileset(
        TilesetId.random(client.username, name),
        TilesetSpec(name, Recipe(Map(sourceLayer -> LayerObject(src))))
      )
    )
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
      SourceLayerId("custom"),
      Option(FilterSpec(Operator.Eq, "NAVL_TYYP", 6))
    )
    assert(Json.toJson(spec) === actualLayer)
  }

  ignore("add to style") {
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
    //      "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714"))
    val update = old.copy(layers = old.layers.map { ls =>
      ls.init ++ Seq(layer)
    })
    val updated = await(client.updateStyle(id, update))
    write(updated, "dyno-updated-more.json")
  }

  ignore("publish tileset") {
    //    val jobId = "ck0l7apz1000001qwespt5d5q"
    val tileset = TilesetId("malliina.vndpjq")
    val r = await(client.startPublishJob(tileset))
    println(r)
  }

  ignore("add image") {
    val req = client.addImage(IconName("cardinal-east-30-opt"), Paths.get(s"data/cardinal-east-30-opt.svg"), dynoStyle)
    val res = await(req)
    println(res)
  }

  ignore("renames") {
    Files.list(Paths.get("data")).forEach { path =>
      val name = path.getFileName.toString
      if (!Files.isDirectory(path) && name.startsWith("avoin:")) {
        Files.move(path, path.getParent.resolve(s"avoin-${name.drop("avoin:".length)}"))
      }
    }
  }

  ignore("update sources") {
    val style = dynoStyle
    val us = await(client.styleTyped(style))
    val srcUrl =
      "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714"
    val update = us.copy(sources = Option(Map("composite" -> StyleSource.vector(srcUrl))))
    val json = await(client.updateStyle(style, update))
    println(json)
  }

  ignore("inject") {
    val style = dynoStyle
    val us = await(client.styleTyped(style))
    val tileset = TilesetId("malliina.klrhwc")
    val update = us.withSource(BoatStyle.src, tileset)
    val json = await(client.updateStyle(style, update))
    println(json)
    write(json, "dyno-updated-more.json")
  }

  def write(f: Future[JsValue], to: String) = {
    val json = await(f)
    val bytes = Json.toBytes(json)
    Files.write(Paths.get(to), bytes)
  }

  def write[T: Writes](t: T, to: String) = {
    val bytes = Json.prettyPrint(Json.toJson(t)).getBytes(StandardCharsets.UTF_8)
    Files.write(Paths.get("target/maps").resolve(to), bytes)
  }

  override protected def afterAll(): Unit = client.close()
}
