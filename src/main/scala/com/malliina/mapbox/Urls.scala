package com.malliina.mapbox

import com.malliina.http.FullUrl
import com.malliina.mapbox.Urls.shapeZipUrlTraficom

class Urls(src: SourceId) extends BoatStyle(src):
  val fairwayAreas = shapeUrl("vaylaalueet")(vaylaAlueet)
  val fairways = shapeUrl("vaylat")(vaylat)
  val marks = shapeUrl("turvalaitteet")(
    safeWaters,
    kummeli,
    lateralRed,
    lateralGreen,
    cardinalNorth,
    cardinalSouth,
    cardinalWest,
    cardinalEast,
    radar,
    leadingBeacon,
    lighthouseNoLight,
    lighthouseYellow,
    sectorLight
  )
  val trafficSigns = shapeUrl("vesiliikennemerkit")(
    noWaves
    //      speedLimit
  )
  val limitAreas = shapeUrl("rajoitusalue_a")(limitArea)
  val leadingBeacons = shapeUrl("taululinja")(taululinja)
  val depthAreas =
    UrlTask(
      "DepthArea_A",
      shapeZipUrlTraficom("DepthArea_A", restricted = true),
      4,
      Seq(depthAreaLayers)
    )
  //    val depthLines =
  //      UrlTask("DepthContour_L", shapeZipUrlTraficom("DepthContour_L"), 2, Nil)
  val areaLimits = UrlTask(
    "TerritorialSeaArea_A",
    shapeZipUrlTraficom("TerritorialSeaArea_A", restricted = false),
    1,
    Seq(aluemeriRaja)
  )
  val depthPoints =
    UrlTask(
      "Sounding_P",
      shapeZipUrlTraficom("Sounding_P", restricted = true),
      4,
      Seq(depthPointLayers)
    )
  // https://docs.mapbox.com/api/maps/#styles
  // Layers will be drawn in the order of this sequence.
  val all = Seq(
    fairways,
    fairwayAreas,
    leadingBeacons,
    depthAreas,
    areaLimits,
    limitAreas,
    depthPoints,
    marks,
    trafficSigns
  )
  val layoutImages =
    all.flatMap(_.styling).map(_.layout).collect { case ImageLayout(name, _, _) => name }
  val appImages = Seq("boat-resized-opt-30", "trophy-gold-path").map(IconName.apply)
  val imageFiles = (layoutImages ++ appImages).map { i =>
    ImageFile.orFail(i)
  }

  private def shapeUrl(name: String, restricted: Boolean = false, parts: Int = 1)(
    styling: LayerStyling*
  ): UrlTask =
    val url = Urls.shapeZipUrl(name, restricted)
    UrlTask(name, url, parts, styling)

object Urls:
  def shapeZipUrl(name: String, restricted: Boolean): FullUrl =
    val modifier = if restricted then "rajoitettu" else "avoin"
    FullUrl.https(
      "julkinen.vayla.fi",
      s"/inspirepalvelu/wfs?request=getfeature&typename=$modifier:$name&outputformat=shape-zip"
    )

  def shapeZipUrlTraficom(name: String, restricted: Boolean) =
    val modifier = if restricted then "rajoitettu" else "avoin"
    FullUrl.https(
      "julkinen.traficom.fi",
      s"/inspirepalvelu/$modifier/wfs?request=GetFeature&outputFormat=shape-zip&typeNames=$modifier:$name"
    )
