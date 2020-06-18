package com.malliina.mapbox

class GeoUtilsTests extends BaseSuite {
  test("can download shapefile and convert to GeoJSON".ignore) {
    val geo = GeoUtils(MapboxClient.mapboxHttpClient())
    val task = new GeoTask {
      override def url = BoatMapGenerator.shapeZipUrl("aluemeri_raja_a", restricted = false)
      override def parts = 1
    }
    val paths = await(geo.shapeToGeoJson(task, Utils.randomString(6)))
  }
}
