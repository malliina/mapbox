package com.malliina.mapbox

import com.malliina.util.AppLogger

import java.io.Serializable
import java.nio.file.Paths
import org.geotools.data.DataStoreFinder
import org.geotools.data.simple.SimpleFeatureIterator
import org.geotools.data.store.ContentFeatureSource
import org.geotools.data.wfs.WFSDataStoreFactory
import org.geotools.feature.DefaultFeatureCollection
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.geojson.feature.FeatureJSON
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeographicCRS
import org.locationtech.jts.geom.Geometry

import scala.jdk.CollectionConverters.MapHasAsJava

class WFSTests extends BaseSuite:
  private val log = AppLogger(getClass)
  val openCapabilities = connectionParameters(
    "https://julkinen.vayla.fi/inspirepalvelu/avoin/wfs?request=getcapabilities"
  )
  val restrictedCapabilities = connectionParameters(
    "https://julkinen.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs?request=getcapabilities"
  )
  // Must be shapefile, not JSON, otherwise the server times out after one minute, most likely due to the payload size
  val depthAreasUrl =
    "https://julkinen.vayla.fi/inspirepalvelu/wfs?request=getfeature&typename=rajoitettu:syvyysalue_a&outputformat=shape-zip"

  val depthAreas = userHome.resolve("boat/syvyysalue_a.shp")

  def connectionParameters(url: String) = Map[String, Serializable](
    "WFSDataStoreFactory:GET_CAPABILITIES_URL" -> url,
    "WFSDataStoreFactory:TIMEOUT" -> 600000
  )

  test("read zip".ignore) {
    val store = DataStoreFinder.getDataStore(Map("url" -> depthAreas.toUri.toString).asJava)
    println(store.getTypeNames.toList)
  }

  test("typenames".ignore) {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(openCapabilities.asJava)
    ds.getTypeNames.toList.sorted foreach println
  }

  test("restricted".ignore) {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(restrictedCapabilities.asJava)
    ds.getTypeNames.toList.sorted foreach println
  }

  test("write".ignore) {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(restrictedCapabilities.asJava)
    val names = ds.getTypeNames.toList
    names.map { name =>
      log.info(s"Inspecting $name...")
      try
        val src: ContentFeatureSource = ds.getFeatureSource(name)
        val features = src.getFeatures.features()
        write(features, s"${name.replace(":", "-")}.json")
      catch
        case e: Throwable =>
          log.error(s"Failed to read $name", e)
      finally {
//        ds.dispose()
      }
    }
    ds.dispose()
  }

  def write(features: SimpleFeatureIterator, file: String): Unit =
    println(s"Writing $file...")
    val writer = new FeatureJSON()
    val outCollection = new DefaultFeatureCollection()
    while features.hasNext do
      val f = features.next()
      val p = f.getDefaultGeometry.asInstanceOf[Geometry]
      val targetCrs = DefaultGeographicCRS.WGS84
      val transformation =
        CRS.findMathTransform(f.getBounds.getCoordinateReferenceSystem, targetCrs, true)
      val tp = JTS.transform(p, transformation)
      val builder = new SimpleFeatureBuilder(f.getType)
      val feature = builder.buildFeature(null)
      feature.setAttributes(f.getAttributes)
      feature.setDefaultGeometry(tp)
      outCollection.add(feature)
    writer.writeFeatureCollection(outCollection, Paths.get(file).toFile)
