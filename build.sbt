version := "0.0.1"
scalaVersion := "2.13.0"

val geoVersion = "21.2"

resolvers += "GeoTools" at "https://download.osgeo.org/webdav/geotools/"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.slf4j" % "slf4j-api" % "1.7.26",
  "javax.media" % "jai_core" % "1.1.3" from "https://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
  "org.geotools" % "gt-shapefile" % geoVersion exclude ("javax.media", "jai_core"),
  "org.geotools" % "gt-geojson" % geoVersion exclude ("javax.media", "jai_core"),
  "org.geotools" % "gt-wfs-ng" % geoVersion exclude ("javax.media", "jai_core"),
  "org.geotools" % "gt-epsg-hsql" % "21.1" % Test
)

libraryDependencies ++= Seq(
  "com.malliina" %% "okclient" % "1.11.0",
  "com.typesafe" % "config" % "1.3.4",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

val mapbox = project.in(file("."))
