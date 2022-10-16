package com.malliina.mapbox

import com.malliina.http.OkClient

import java.nio.file.{Files, Paths}

class GeoUtilsTests extends BaseSuite:
  val client = FunFixture[OkClient](_ => MapboxClient.mapboxHttpClient(), _.close())

  client.test("can download shapefile and convert to GeoJSON".ignore) { http =>
    val geo = GeoUtils(http)
    val task = new GeoTask:
      override def name = "aluemeri"
      override def url = Urls.shapeZipUrl("aluemeri_raja_a", restricted = false)
      override def parts = 1
    val paths = await(geo.shapeToGeoJson(task, Utils.randomString(6)))
  }

  client.test("download all shapefiles".ignore) { http =>
    val geo = GeoUtils(http)
    val urls = Urls(SourceId.random())
    val dir = Files.createDirectories(Paths.get("target/shapes"))
    val task = Concurrent.traverseSlowly(urls.all, parallelism = 1) { url =>
      geo.shapeToGeoJson(url, Utils.randomString(6), dir)
    }
    await(task)
  }
