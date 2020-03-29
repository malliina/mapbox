package com.malliina.mapbox

object BoatStyle extends BoatStyle(SourceId("mle7"))

class BoatStyle(val src: SourceId) {
  val icons = Seq(
    "cardinal-north-30-opt",
    "cardinal-east-30-opt",
    "cardinal-south-30-opt",
    "cardinal-west-30-opt",
    "lateral-red-30-opt",
    "lateral-green-30-opt",
    "radar-15",
    "leading-beacon-15",
    "no-waves-15",
    "lighthouse-30-nolight",
    "lighthouse-30-yellow",
    "limit-10-30-normal",
    "lighthouse-15"
  )
  val lateralRed = markLayer("lateral-red", "lateral-red-30-opt", 1)
  val lateralGreen = markLayer("lateral-green", "lateral-green-30-opt", 2)

  val cardinalNorth = markLayer("cardinal-north", "cardinal-north-30-opt", 3)
  val cardinalSouth = markLayer("cardinal-south", "cardinal-south-30-opt", 4)
  val cardinalWest = markLayer("cardinal-west", "cardinal-west-30-opt", 5)
  val cardinalEast = markLayer("cardinal-east", "cardinal-east-30-opt", 6)

  val radar = simpleSymbolLayer("marks-tutka", "radar-15", "TY_JNR", 8, -7.5)
  val leadingBeacon = simpleSymbolLayer("marks-linjamerkki", "leading-beacon-15", "TY_JNR", 3, -7.5)
  val lighthouseNoLight = simpleSymbolLayer("marks-tunnusmajakka", "lighthouse-30-nolight", "TY_JNR", 11, -15)
  val lighthouseYellow = simpleSymbolLayer("marks-merimajakka", "lighthouse-30-yellow", "TY_JNR", 1, -15)
  val sectorLight = simpleSymbolLayer("marks-sektoriloisto", "lighthouse-15", "TY_JNR", 2, 0)
  val noWaves = simpleSymbolLayer("marks-no-waves", "no-waves-15", "VLM_LAJI", 6, -7.5)
  val safeWaters = LayerStyling(
    "circle",
    VisibilityLayout(Visibility.Visible),
    src,
    Option(FilterSpec(Operator.Eq, "NAVL_TYYP", 8)),
    Option(Paint(`circle-color` = Option("hsl(0, 58%, 82%)"))),
    minzoom = Option(13),
    layerIdOverride = Option(LayerId("turvavesi"))
  )
  val kummeli = LayerStyling(
    "circle",
    VisibilityLayout(Visibility.Visible),
    src,
    Option(FilterSpec(Operator.Eq, "TY_JNR", 13)),
    Option(Paint(`circle-color` = Option("hsl(0, 6%, 98%)"))),
    minzoom = Option(13),
    layerIdOverride = Option(LayerId("kummeli"))
  )
  val speedLimit = symbolLayer(
    "marks-speed-limit",
    "limit-10-30-normal",
    MultiFilter(Combinator.All, Seq(FilterSpec(Operator.Eq, "RA_ARVO", 10), FilterSpec(Operator.Eq, "VLM_LAJI", 11))),
    0
  )
  val vaylaAlueet = LayerStyling(
    "fill",
    VisibilityLayout(Visibility.Visible),
    src,
    paint = Option(Paint(`fill-color` = Option("hsl(118, 96%, 37%)"), `fill-opacity` = Option(0.1)))
  )
  val vaylat = LayerStyling(
    "line",
    VisibilityLayout(Visibility.Visible),
    src,
    paint = Option(Paint(`line-color` = Option("hsl(122, 89%, 52%)")))
  )
  val taululinja = LayerStyling(
    "line",
    VisibilityLayout(Visibility.Visible),
    src,
    paint = Option(Paint(`line-color` = Option("hsl(0, 84%, 76%)")))
  )
  val limitArea = LayerStyling(
    "fill",
    EmptyLayout,
    src,
    paint = Option(Paint(`fill-color` = Option("hsla(321, 96%, 56%, 0.05)")))
  )
  val depthPointLayers = LayerStyling(
    "symbol",
    TextLayout("{DEPTH}", 10),
    src,
    minzoom = Option(14.7)
  )
  val aluemeriRaja = LayerStyling(
    "fill",
    VisibilityLayout(Visibility.Visible),
    src,
    paint = Option(Paint(`fill-color` = Option("hsl(0, 95%, 16%)"), `fill-opacity` = Option(0.1)))
  )

  val depthAreaLayers = LayerStyling(
    "fill",
    VisibilityLayout(Visibility.Visible),
    src,
    paint = Option(Paint(`fill-color` = Option("#000000"), `fill-opacity` = Option(0.1)))
  )
  val all = Seq(
    depthAreaLayers,
    depthPointLayers,
    vaylaAlueet,
    limitArea,
    vaylat,
    taululinja,
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
    sectorLight,
    noWaves,
    safeWaters,
    speedLimit
  )
  val allTest = Seq(vaylat)

  def markLayer(id: String, icon: String, navFilterValue: Int) =
    simpleSymbolLayer(id, icon, "NAVL_TYYP", navFilterValue, -15)

  def simpleSymbolLayer(id: String, icon: String, prop: String, propValue: Int, offsetY: Double) =
    symbolLayer(id, icon, FilterSpec(Operator.Eq, prop, propValue), offsetY)

  def symbolLayer(id: String, icon: String, filter: FilterLike, offsetY: Double) =
    LayerStyling(
      "symbol",
      iconLayout(icon, offsetY),
      src,
      Option(filter),
      layerIdOverride = Option(LayerId(id))
    )

  def iconLayout(name: String, offsetY: Double) = ImageLayout(IconName(name), Option(Seq(0, offsetY)))
  def eqFilter(prop: String, value: Int) = FilterSpec(Operator.Eq, prop, value)
}
