package com.malliina.mapbox

class BoatMapGeneratorTests extends BaseSuite {
  val client = BoatMapGenerator()
  val tileset = TilesetId("malliina.klrhwc")

  ignore("create map") {
    val tileset = await(client.generate(StyleId("ck8csi4ur1pcc1irzpxgc4pb7")))
    println(s"Generated $tileset.")
  }

  test("compare  layers") {
    val official = StyleId("cjgny1fjc008p2so90sbz8nbv")
    val wip = StyleId("ck8csi4ur1pcc1irzpxgc4pb7")
    val mapbox = client.mapbox
    def layers(id: StyleId) =
      await(mapbox.styleTyped(id)).layers.getOrElse(Nil).filter(obj => (obj \ "id").as[String].startsWith("vaylat"))
    println(layers(wip))
    println(layers(official))
  }

  ignore("recipes") {
    val r = await(client.recipes())
    println(s"Generated recipes $r.")
  }

  override protected def afterAll(): Unit = {
    client.mapbox.close()
  }
}
