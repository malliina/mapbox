package com.malliina.mapbox

class BoatStyle {
  val src = SourceId("composite")
  val srcLayer = SourceLayerId("custom")

  val lateralRed = markLayer("lateral-red", "lateral-green-30-opt", 1)
  val lateralGreen = markLayer("lateral-green", "lateral-green-30-opt", 2)

  val cardinalNorth = markLayer("cardinal-north", "cardinal-north-30-opt", 3)
  val cardinalSouth = markLayer("cardinal-south", "cardinal-south-30-opt", 4)
  val cardinalWest = markLayer("cardinal-west", "cardinal-west-30-opt", 5)
  val cardinalEast = markLayer("cardinal-east", "cardinal-east-30-opt", 6)

  val radar = simpleSymbolLayer("marks-tutka", "radar-15", "TY_JNR", 8)
  val leadingBeacon = simpleSymbolLayer("marks-linjamerkki", "leading-beacon-15", "TY_JNR", 3)
  val lighthouseNoLight = simpleSymbolLayer("marks-tunnusmajakka", "lighthouse-30-nolight", "TY_JNR", 11)
  val lighthouseYellow = simpleSymbolLayer("marks-merimajakka", "lighthouse-30-yellow", "TY_JNR", 1)
  val sectorLight = simpleSymbolLayer("marks-sektoriloisto", "lighthouse-15", "TY_JNR", 2)
  val noWaves = simpleSymbolLayer("marks-no-waves", "no-waves-15", "VLM_LAJI", 6)
  val speedLimit = symbolLayer(
    "marks-speed-limit",
    "limit-10-30-normal",
    MultiFilter(Combinator.All, Seq(FilterSpec(Operator.Eq, "RA_ARVO", 10), FilterSpec(Operator.Eq, "VLM_LAJI", 11))))

  def markLayer(id: String, icon: String, navFilterValue: Int) =
    simpleSymbolLayer(id, icon, "NAVL_TYYP", navFilterValue)

  def simpleSymbolLayer(id: String, icon: String, prop: String, propValue: Int) =
    symbolLayer(id, icon, FilterSpec(Operator.Eq, prop, propValue))

  def symbolLayer(id: String, icon: String, filter: FilterLike) =
    LayerSpec(
      LayerId(id),
      "symbol",
      iconLayout(icon),
      SourceId("composite"),
      Option(filter),
      SourceLayerId("custom")
    )

  def iconLayout(name: String) = ImageLayout(IconName(name), Option(Seq(0, -15)))
  def eqFilter(prop: String, value: Int) = FilterSpec(Operator.Eq, prop, value)
}
