package com.malliina.mapbox

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
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.MapHasAsJava

class WFSTests extends BaseSuite {
  val log = LoggerFactory.getLogger(getClass)
  val openCapabilities = connectionParameters(
    "https://julkinen.vayla.fi/inspirepalvelu/avoin/wfs?request=getcapabilities")
  val restrictedCapabilities = connectionParameters(
    "https://julkinen.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs?request=getcapabilities")
  // Must be shapefile, not JSON, otherwise the server times out after one minute, most likely due to the payload size
  val depthAreasUrl = "https://julkinen.vayla.fi/inspirepalvelu/wfs?request=getfeature&typename=rajoitettu:syvyysalue_a&outputformat=shape-zip"

  val depthAreas = userHome.resolve("boat/syvyysalue_a.shp")

  def connectionParameters(url: String) = Map[String, Serializable](
    "WFSDataStoreFactory:GET_CAPABILITIES_URL" -> url,
    "WFSDataStoreFactory:TIMEOUT" -> 600000
  )

  ignore("read zip") {
    val store = DataStoreFinder.getDataStore(Map("url" -> depthAreas.toUri.toString).asJava)
    println(store.getTypeNames.toList)
  }

  ignore("typenames") {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(openCapabilities.asJava)
    ds.getTypeNames.toList.sorted foreach println
  }

  test("restricted") {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(restrictedCapabilities.asJava)
    ds.getTypeNames.toList.sorted foreach println
  }

  ignore("write") {
    val factory = new WFSDataStoreFactory
    val ds = factory.createDataStore(restrictedCapabilities.asJava)
    val names = ds.getTypeNames.toList
    names.map { name =>
      log.info(s"Inspecting $name...")
      try {
        val src: ContentFeatureSource = ds.getFeatureSource(name)
        val features = src.getFeatures.features()
        write(features, s"${name.replace(":", "-")}.json")
      } catch {
        case e: Throwable =>
          log.error(s"Failed to read $name", e)
      } finally {
//        ds.dispose()
      }
    }
    ds.dispose()
  }

  def write(features: SimpleFeatureIterator, file: String) = {
    println(s"Writing $file...")
    val writer = new FeatureJSON()
    val outCollection = new DefaultFeatureCollection()
    while (features.hasNext) {
      val f = features.next()
      val p = f.getDefaultGeometry.asInstanceOf[Geometry]
      val targetCrs = DefaultGeographicCRS.WGS84
      val transformation = CRS.findMathTransform(f.getBounds.getCoordinateReferenceSystem, targetCrs, true)
      val tp = JTS.transform(p, transformation)
      val builder = new SimpleFeatureBuilder(f.getType)
      val feature = builder.buildFeature(null)
      feature.setAttributes(f.getAttributes)
      feature.setDefaultGeometry(tp)
      outCollection.add(feature)
    }
    writer.writeFeatureCollection(outCollection, Paths.get(file).toFile)
  }

//  def hmm() = {
//    import scala.jdk.CollectionConverters.MapHasAsJava
//    val connectionParameters = Map(
//      "WFSDataStoreFactory:GET_CAPABILITIES_URL" -> "https://julkinen.vayla.fi/inspirepalvelu/avoin/wfs?request=getcapabilities")
//
//    // Step 2 - connection
//    val data = DataStoreFinder.getDataStore(connectionParameters.asJava)
//
//    println(data)
//    // Step 3 - discouvery
//    val typeNames = data.getTypeNames
//    val typeName = typeNames(0)
//    val schema = data.getSchema(typeName)
//
//    // Step 4 - target
////    FeatureSource<SimpleFeatureType, SimpleFeature> source = data.getFeatureSource( typeName );
//    val source = data.getFeatureSource(typeName)
//    println(s"Metadata Bounds: ${source.getBounds}")
//
//    // Step 5 - query
//    val geomName = schema.getGeometryDescriptor.getLocalName
////    val geomName = schema.getDefaultGeometry().getLocalName();
//    val bbox = new Envelope(-100.0, -70, 25, 40)
//
//    val ff = CommonFactoryFinder.getFilterFactory2(GeoTools.getDefaultHints)
//    val polygon = JTS.toGeometry(bbox)
//    val filter = ff.intersects(ff.property(geomName), ff.literal(polygon))
//
//    val query = new Query(typeName, filter, Array(geomName))
//    //    FeatureCollection<SimpleFeatureType, SimpleFeature> features = source.getFeatures( query )
//    val features = source.getFeatures(query)
//
//    val bounds = new ReferencedEnvelope()
//    val iterator = features.features()
//    try {
//      while (iterator.hasNext) {
//        val feature = iterator.next()
//        bounds.include(feature.getBounds)
//      }
//      println(s"Calculated Bounds: $bounds")
//    } finally {
////      features.close( iterator );
//    }
//  }
}
