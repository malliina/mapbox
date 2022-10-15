val geoVersion = "23.1"

val mapbox = project
  .in(file("."))
  .settings(
    version := "0.0.1",
    scalaVersion := "3.2.0",
    resolvers += "OSGeo Release Repository" at "https://repo.osgeo.org/repository/release/",
    libraryDependencies ++= Seq(
      "com.malliina" %% "okclient-io" % "3.2.0",
      "com.typesafe" % "config" % "1.4.0",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.slf4j" % "slf4j-api" % "1.7.30",
      "javax.media" % "jai_core" % "1.1.3",
      "org.geotools" % "gt-shapefile" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-geojson" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-wfs-ng" % geoVersion exclude ("javax.media", "jai_core"),
      "org.geotools" % "gt-epsg-hsql" % "23.1" % Test,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / bloopExportJarClassifiers := Some(Set("sources"))
