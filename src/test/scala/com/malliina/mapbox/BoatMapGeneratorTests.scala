package com.malliina.mapbox

import java.time.{LocalDate, ZoneId}

class BoatMapGeneratorTests extends BaseSuite:
  val map = FunFixture[BoatMapGenerator](_ => BoatMapGenerator(), _.mapbox.close())

  map.test("create map from scratch".ignore) { client =>
    await(client.generate("Boat-Tracker API"))
  }

  map.test("can generate map with styles from WFS API".ignore) { client =>
    val mapbox = client.mapbox
    val date = LocalDate.now(ZoneId.of("Europe/Helsinki"))
    val prefix = s"test-$date-"
    val req = GenerateMapRequest(
      s"Testmap-$prefix-${Utils.randomString(6)}",
      Seq(client.urls.fairways),
      Nil,
      prefix
    )
    val map = await(client.generate(req))
    val d = await(mapbox.deleteStyle(map.style))
    assert(d.code == 204)
    val ts = await(mapbox.deleteTileset(map.tileset))
    assert(ts.code == 200)
  }

  map.test("create style".ignore) { client =>
    val spec = client.styleFromResource().copy(name = "Boom")
    val f = client.mapbox.createStyleTyped(spec)
    await(f)
  }
