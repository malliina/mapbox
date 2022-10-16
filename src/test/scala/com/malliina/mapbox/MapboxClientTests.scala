package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import com.malliina.http.ResponseException
import munit.FunSuite

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scala.concurrent.Future

trait ClientFixture:
  self: FunSuite =>
  val http = FunFixture[MapboxClient](
    _ => MapboxClient.fromConf(),
    client => client.close()
  )

class MapboxClientTests extends BaseSuite with ClientFixture:
  val dynoStyle = StyleId("ck0jvmrkf12q21cln11l3v5wp") // Dyno

  http.test("empty template".ignore) { client =>
    val finnishWaters = StyleId("ck8g72lr949js1invb4ovnyhg")
    val s = await(client.style(finnishWaters))
    write(s, "empty-streets-style.json")
  }

  http.test("add icon to style".ignore) { client =>
    val style = StyleId("ck8epthw303kj1imowjvyiagy")
    val iconName = "trophy-gold-path"
    val op = client.addImage(IconName(iconName), Paths.get(s"data/images/$iconName.svg"), style)
    val json = await(op)
    println(json)
  }

  http.test("list styles".ignore) { client =>
    val s = await(client.stylesTyped)
//    println(Json.prettyPrint(s))
    println(s.mkString("\n"))
  //    val bytes = Json.toBytes(s)
  //    Files.write(Paths.get("dynamic.json"), bytes)
  }

  http.test("get style".ignore) { client =>
//    val styleId = StyleId("ck8d9h3vn2mrn1imyk025ya8v")
    val styleId = StyleId("cl9a65qy1000814lbx2tctoq1")
    val s = await(client.style(styleId))
    //    println(Json.prettyPrint(s))
    write(s, s"streets-$styleId.json")
  }

  http.test("update".ignore) { client =>
    val wip = StyleId("ck8d9h3vn2mrn1imyk025ya8v")
    val op = client.updateLayer(
      wip,
      Seq(
        LayerSpec(
          LayerId("nav-rajoitusalue_a0"),
          "fill",
          EmptyLayout,
          SourceId("ytcgew"),
          SourceLayerId("nav-rajoitusalue_a0"),
          paint = Option(
            Paint(`fill-color` = Option("hsl(321, 96%, 56%)"), `fill-opacity` = Option(0.01))
          )
        ),
        LayerSpec(
          LayerId("nav-vaylaalueet0"),
          "fill",
          EmptyLayout,
          SourceId("ytcgew"),
          SourceLayerId("nav-vaylaalueet0"),
          paint = Option(
            Paint(`fill-color` = Option("hsl(118, 96%, 37%)"), `fill-opacity` = Option(0.02))
          )
        )
//        LayerSpec(
//          LayerId("nav-syvyysalue_a0"),
//          "fill",
//          EmptyLayout,
//          SourceId("ytcgew"),
//          SourceLayerId("nav-syvyysalue_a0"),
//          paint = Option(Paint(`fill-color` = Option("#000000"), `fill-opacity` = Option(0.01)))
//        )
      )
    )
    write(op, s"updated-$wip.json")
  }

  http.test("get sprite".ignore) { client =>
    write(client.sprite(dynoStyle), "sprite.json")
  }

  http.test("write png sprite".ignore) { client =>
    val r = await(client.sprite(dynoStyle, Paths.get("sprite.png")))
    println(r)
  }

  http.test("update dyno".ignore) { client =>
    val json = io.circe.parser
      .decode[JsonObject]("""{
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
        |    }""".stripMargin)
      .fold(err => throw Exception(err.getMessage), identity)
    val id = StyleId("ck0jvmrkf12q21cln11l3v5wp")
    val s = await(client.style(id))
    try
      val sources = Map(
        "composite" -> StyleSource.vector(
          "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714,malliina.9si6bepc"
        )
      )
      val old = s.as[UpdateStyle].fold(err => throw Exception(err.message), identity)
      val update = old.copy(sources = Option(sources))
      val updated = await(client.updateStyle(id, update))
      write(updated, "dyno-updated.json")
    catch
      case re: ResponseException =>
        println(re.response.asString)

  }

  http.test("get style 2".ignore) { client =>
    val id = StyleId("ck0jlem470onc1ckgaxtvlb8m")
    val s = await(client.style(id))
    write(s, "updated.json")
  }

  http.test("line delimit".ignore) { client =>
    FeatureCollection.lineDelimit(
      Paths.get("data/avoin:vaylat.json"),
      Paths.get("data/vaylat-delimited.json")
    )
  }

  http.test("list tileset sources".ignore) { client =>
    val response = await(client.tilesetSources())
    println(response)
  }

  http.test("create tileset source".ignore) { client =>
    val src = userHome.resolve("boat/syvyysalue_a0.json")
//    val src = Paths.get("data/avoin:vaylaalueet.json")
    val response = await(client.createTilesetSource(TilesetSourceId("syvyysalue_a0"), src))
    println(response)
  }

  http.test("create tileset".ignore) { client =>
    val name = TilesetName.random()
    val src = TilesetSourceId("mapbox://tileset-source/malliina/gnqfih")
    val sourceLayer = SourceLayerId("custom")
    val r = await(
      client.createTileset(
        TilesetId.apply(client.username, name),
        TilesetSpec(name, Recipe(Map(sourceLayer -> LayerObject(src))))
      )
    )
    println(r)
  }

  val actualLayer = io.circe.parser
    .parse("""{
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
    .fold(err => throw Exception(err.message), identity)

  http.test("serialize spec".ignore) { client =>
    val spec = LayerSpec(
      LayerId("cardinal-east"),
      "symbol",
      ImageLayout(IconName("cardinal-east-30-opt"), Option(Seq(0, -15))),
      SourceId("composite"),
      SourceLayerId("custom"),
      Option(FilterSpec(Operator.Eq, "NAVL_TYYP", 6))
    )
    assert(spec.asJson == actualLayer)
  }

  http.test("add to style".ignore) { client =>
    val layer = io.circe.parser
      .decode[JsonObject]("""
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
        |""".stripMargin)
      .fold(err => throw Exception(err.getMessage), identity)
    val id = dynoStyle
    val s = await(client.style(id))
    val old = s.as[UpdateStyle].fold(err => throw Exception(err.message), identity)
    //    val sources = Map("composite" -> StyleSource.vector(
    //      "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714"))
    val update = old.copy(layers = old.layers.map { ls => ls.init ++ Seq(layer) })
    val updated = await(client.updateStyle(id, update))
    write(updated, "dyno-updated-more.json")
  }

  http.test("publish tileset".ignore) { client =>
    //    val jobId = "ck0l7apz1000001qwespt5d5q"
    val tileset = TilesetId("malliina.vndpjq")
    val r = await(client.startPublishJob(tileset))
    println(r)
  }

  http.test("add image".ignore) { client =>
    val req = client.addImage(
      IconName("cardinal-east-30-opt"),
      Paths.get(s"data/cardinal-east-30-opt.svg"),
      dynoStyle
    )
    val res = await(req)
    println(res)
  }

  http.test("renames".ignore) { client =>
    Files.list(Paths.get("data")).forEach { path =>
      val name = path.getFileName.toString
      if !Files.isDirectory(path) && name.startsWith("avoin:") then
        Files.move(path, path.getParent.resolve(s"avoin-${name.drop("avoin:".length)}"))
    }
  }

  http.test("update sources".ignore) { client =>
    val style = dynoStyle
    val us = await(client.styleTyped(style))
    val srcUrl =
      "mapbox://malliina.1efes2rg,malliina.3zb4yj6s,malliina.bbe2g1zj,malliina.7yl5ppr0,malliina.2bh0gyii,mapbox.mapbox-streets-v7,malliina.bld4dv1t,malliina.9hdccori,malliina.36mi8ybt,malliina.auqlq9ea,malliina.5m3qz8qs,malliina.ctyq7xyj,mapbox.mapbox-terrain-v2,malliina.497s8714"
    val update = us.copy(sources = Option(Map("composite" -> StyleSource.vector(srcUrl))))
    val json = await(client.updateStyle(style, update))
    println(json)
  }

  def write(f: Future[Json], to: String) =
    val json = await(f)
    val bytes = MapboxClient.printer.print(json).getBytes(StandardCharsets.UTF_8)
    Files.write(Paths.get(to), bytes)

  def write[T: Encoder](t: T, to: String) =
    val bytes = MapboxClient.printer.print(t.asJson).getBytes(StandardCharsets.UTF_8)
    val dir = Paths.get("target/maps")
    Files.createDirectories(dir)
    Files.write(dir.resolve(to), bytes)
