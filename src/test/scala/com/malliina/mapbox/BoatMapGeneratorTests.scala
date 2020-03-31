package com.malliina.mapbox

import java.time.{LocalDate, ZoneId}

class BoatMapGeneratorTests extends BaseSuite {
  val client = BoatMapGenerator()
  val mapbox = client.mapbox

  ignore("create map from scratch") {
    client.generate("Test map")
  }

  test("can generate map with styles from WFS API") {
    val date = LocalDate.now(ZoneId.of("Europe/Helsinki"))
    val prefix = s"test-$date-"
    val req = GenerateMapRequest(s"Testmap-$prefix-${Utils.randomString(6)}", Seq(client.urls.fairways), Nil, prefix)
    val map = await(client.generate(req))
    await(mapbox.deleteTileset(map.tileset))
    val d = await(mapbox.deleteStyle(map.style))
    assert(d.code === 204)
    val ts = await(mapbox.deleteTileset(map.tileset))
    assert(ts.code === 204)
  }

  override protected def afterAll(): Unit = mapbox.close()
}
