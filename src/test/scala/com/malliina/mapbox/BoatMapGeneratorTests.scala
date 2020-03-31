package com.malliina.mapbox

class BoatMapGeneratorTests extends BaseSuite {
  val client = BoatMapGenerator()

  ignore("create map") {
    val tileset = await(client.generate(StyleId("todo")))
    println(s"Generated $tileset.")
  }

  ignore("compare  layers") {
    val good = StyleId("ck8d5q2h02j0r1iqltul99unj")
    val wip = StyleId("ck8devqu81col1jmvr1ltxhxm")
    val bad = StyleId("ck8d9h3vn2mrn1imyk025ya8v")
    val mapbox = client.mapbox
    def layers(id: StyleId) =
      await(mapbox.styleTyped(id)).layers.getOrElse(Nil).filter(obj => (obj \ "id").as[String].startsWith("nav"))
    println(layers(wip))
    println(layers(good))
    println(layers(bad))
  }

  override protected def afterAll(): Unit = {
    client.mapbox.close()
  }
}
