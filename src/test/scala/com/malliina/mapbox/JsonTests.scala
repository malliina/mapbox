package com.malliina.mapbox

import munit.FunSuite
import io.circe.syntax.*

class JsonTests extends FunSuite:
  test("serialize models") {
    val json = LayerSpec(
      LayerId("1"),
      "Joo",
      TextLayout("Joo", 2),
      SourceId("1"),
      SourceLayerId("1"),
      filter = None
    ).asJson
    println(json)
  }
