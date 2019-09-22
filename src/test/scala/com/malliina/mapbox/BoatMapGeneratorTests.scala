package com.malliina.mapbox

class BoatMapGeneratorTests extends BaseSuite {
  val client = BoatMapGenerator()
  val tileset = TilesetId("malliina.klrhwc")

  ignore("generate recipes") {
    val r = await(client.recipes())
    await(client.mapbox.updateRecipe(tileset, r))
  }

  override protected def afterAll(): Unit = {
    client.mapbox.close()
  }
}
