val geoVersion = "21.2"

val mapbox = project
  .in(file("."))
  .settings(
    version := "0.0.1",
    scalaVersion := "2.13.1",
    resolvers += "GeoTools" at "https://download.osgeo.org/webdav/geotools/",
    libraryDependencies ++= Seq(
      "com.malliina" %% "okclient" % "1.14.0",
      "com.typesafe" % "config" % "1.4.0",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.slf4j" % "slf4j-api" % "1.7.30",
      "javax.media" % "jai_core" % "1.1.3" from "https://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
      "org.geotools" % "gt-shapefile" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-geojson" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-wfs-ng" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-epsg-hsql" % "21.1" % Test,
      "org.scalatest" %% "scalatest" % "3.1.1" % Test
    )
  )

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / bloopExportJarClassifiers := Some(Set("sources"))
