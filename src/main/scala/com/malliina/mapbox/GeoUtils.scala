package com.malliina.mapbox

import java.nio.file.{Files, Path}
import com.malliina.http.{FullUrl, OkClient, ResponseException}
import com.malliina.mapbox.GeoUtils.log
import com.malliina.storage.StorageSize
import com.malliina.util.AppLogger
import org.apache.commons.io.FilenameUtils
import org.geotools.data.DataStoreFinder
import org.geotools.data.simple.SimpleFeatureCollection
import org.geotools.feature.DefaultFeatureCollection
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.geojson.feature.FeatureJSON
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.{CRS, crs}
import org.locationtech.jts.geom.Geometry

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Random

object GeoUtils:
  private val log = AppLogger(getClass)

class GeoUtils(val http: OkClient):
  import http.exec

  /** <ol> <li>Downloads a shapefile from the url in `task`</li> <li>Unzips it</li> <li>Converts any
    * shapefiles to GeoJSON files</li> </ol>
    *
    * @param task
    *   file to download
    * @return
    */
  def shapeToGeoJson(
    task: GeoTask,
    filePrefix: String,
    dir: Path = Files.createTempDirectory("shapes")
  ): Future[Seq[Path]] =
    val url = task.url
    val downloadedFile = Files.createTempFile(dir, s"${task.name}-", ".zip")
    val absolutePath = downloadedFile.toAbsolutePath
    log.info(s"Downloading '$url' to '$absolutePath'...")
    download(url, downloadedFile).flatMap { size =>
      log.info(s"Downloaded $size to '$absolutePath'. Unzipping...")
      val unzipFolder = FilenameUtils.removeExtension(downloadedFile.getFileName.toString)
      Utils
        .unzip(downloadedFile, downloadedFile.getParent.resolve(unzipFolder))
        .find(f => FilenameUtils.getExtension(f.getFileName.toString) == "shp")
        .map { shape =>
          Future.successful(convert(shape, task.parts, filePrefix))
        }
        .getOrElse {
          Future.failed(Exception(s"No shapefile found in '$absolutePath'."))
        }
    }

  def download(url: FullUrl, to: Path, retries: Int = 3): Future[StorageSize] =
    http.download(url, to).flatMap { e =>
      e.fold(
        err =>
          if err.code == 502 && retries > 0 then
            log.info(
              s"Request to '$url' failed with 502. $retries retries remaining. Retrying soon..."
            )
            Concurrent.scheduleIn(5.seconds) {
              download(url, to, retries - 1)
            }
          else Future.failed(ResponseException(err)),
        sz => Future.successful(sz)
      )
    }

  /** Converts `shapeFile` to `splitFactor` GeoJSON files.
    *
    * Splits the shapefile into `splitFactor` JSON files for easier further processing.
    *
    * @param shapeFile
    *   source
    * @param splitFactor
    *   number of output files
    * @return
    */
  def convert(shapeFile: Path, splitFactor: Int, filePrefix: String): Seq[Path] =
    val parent = shapeFile.getParent
    val store = DataStoreFinder.getDataStore(Map("url" -> shapeFile.toUri.toString).asJava)
    val writer = new FeatureJSON()
    val collections: List[SimpleFeatureCollection] =
      store.getTypeNames.map(store.getFeatureSource).map(_.getFeatures).toList
    val transformedCollections = collections.map { coll =>
      val outs = List.fill(splitFactor)(new DefaultFeatureCollection())
      val srcCrs = coll.getSchema.getCoordinateReferenceSystem
      val targetCrs = crs.DefaultGeographicCRS.WGS84
      val transformation = CRS.findMathTransform(srcCrs, targetCrs, true)
      val srcFeatures = coll.features()
      try while srcFeatures.hasNext do
        val srcFeature = srcFeatures.next()
        val geo: Geometry = srcFeature.getDefaultGeometry.asInstanceOf[Geometry]
        val transformed = JTS.transform(geo, transformation)
        val builder = new SimpleFeatureBuilder(coll.getSchema)
        val feature = builder.buildFeature(null)
        feature.setAttributes(srcFeature.getAttributes)
        feature.setDefaultGeometry(transformed)
        val idx = Random.between(0, splitFactor)
        outs(idx).add(feature)
      finally srcFeatures.close()
      outs
    }
    (0 until splitFactor).map { i =>
      val name = FilenameUtils.removeExtension(shapeFile.getFileName.toString)
      val fileOut = parent.resolve(s"$filePrefix-$name$i.json")
      writer.writeFeatureCollection(transformedCollections.head(i), fileOut.toFile)
      log.info(s"Wrote '${fileOut.toAbsolutePath}'.")
      fileOut
    }.toList
