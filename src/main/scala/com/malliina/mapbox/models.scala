package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.malliina.values.{StringCompanion, Username, WrappedString}
import play.api.libs.json.{Format, JsArray, JsObject, JsValue, Json, Writes}

/** Name of an icon in a sprite of a style.
  *
  * @param value
  */
case class IconName(value: String) extends WrappedString
object IconName extends StringCompanion[IconName]

case class StyleId(value: String) extends WrappedString
object StyleId extends StringCompanion[StyleId]

case class LayerId(value: String) extends WrappedString
object LayerId extends StringCompanion[LayerId]

case class SourceId(value: String) extends WrappedString
object SourceId extends StringCompanion[SourceId]

/** Totally distinct from LayerId.
  *
  * @see https://docs.mapbox.com/help/glossary/source-layer/
  */
case class SourceLayerId(value: String) extends WrappedString
object SourceLayerId extends StringCompanion[SourceLayerId]

case class TilesetId(value: String) extends WrappedString
object TilesetId extends StringCompanion[TilesetId] {
  def random(username: Username, name: TilesetName) = apply(s"$username.$name")
}

case class TilesetName(value: String) extends WrappedString
object TilesetName extends StringCompanion[TilesetName] {
  def random(): TilesetName = apply(Utils.randomString(6))
}

case class TilesetSourceId(value: String) extends WrappedString
object TilesetSourceId extends StringCompanion[TilesetSourceId] {
  def random(): TilesetSourceId = apply(Utils.randomString(6))
}

case class LayerObject(source: TilesetSourceId,
                       minzoom: Int,
                       maxzoom: Int,
                       features: Option[JsObject],
                       tiles: Option[JsObject])
object LayerObject {
  implicit val json = Json.format[LayerObject]

  def apply(source: TilesetSourceId): LayerObject = apply(source, 0, 16, None, None)
}

case class Recipe(version: Int, layers: Map[String, LayerObject])

object Recipe {
  implicit val json = Json.format[Recipe]

  def apply(layers: Map[SourceLayerId, LayerObject]): Recipe = apply(1, layers.map { case (k, v) => k.value -> v })
}

case class TilesetSpec(name: TilesetName, recipe: Recipe, description: Option[String] = None)

object TilesetSpec {
  implicit val json = Json.format[TilesetSpec]
}

case class SimpleLayer(id: String, `type`: String, paint: JsObject, interactive: Boolean)

object SimpleLayer {
  implicit val json = Json.format[SimpleLayer]

  def background(id: String) = SimpleLayer(id, "background", Json.obj("background-color" -> "#111"), true)
}

case class SourcedLayer(id: String, source: SourceId)

object SourcedLayer {
  implicit val json = Json.format[SourcedLayer]
}

case class Style(version: Int, name: Option[String], sources: JsObject, layers: Seq[SourcedLayer])

object Style {
  implicit val json = Json.format[Style]
}

case class StyleSource(url: String, `type`: String)

object StyleSource {
  implicit val json = Json.format[StyleSource]

  def vector(url: String) = StyleSource(url, "vector")
}

case class UpdateStyle(version: Int,
                       name: String,
                       metadata: Option[JsObject],
                       sources: Option[Map[String, StyleSource]],
                       sprite: Option[String],
                       glyphs: Option[String],
                       layers: Option[Seq[JsObject]])

object UpdateStyle {
  implicit val json = Json.format[UpdateStyle]
}

case class FeatureCollection(features: Seq[JsObject])

object FeatureCollection {
  implicit val json = Json.format[FeatureCollection]

  def lineDelimit(in: Path, out: Path) = {
    val coll = Json.parse(Files.readAllBytes(in)).as[FeatureCollection]
    val lines = coll.features.map(Json.stringify).mkString("\n")
    Files.write(out, lines.getBytes(StandardCharsets.UTF_8))
  }
}

case class TilesetSourceCreated(id: TilesetSourceId, files: Int)

object TilesetSourceCreated {
  implicit val json = Json.format[TilesetSourceCreated]
}

sealed abstract class Visibility(val name: String)

object Visibility {
  implicit val writer = Writes[Visibility] { v =>
    Json.toJson(v.name)
  }
  case object None extends Visibility("none")
  case object Visible extends Visibility("visible")
}

sealed trait LayoutSpec

object LayoutSpec {
  implicit val writer = Writes[LayoutSpec] {
    case is @ ImageLayout(_, _, _) => ImageLayout.json.writes(is)
    case ts @ TextLayout(_, _, _)  => TextLayout.json.writes(ts)
    case LayoutEmpty               => Json.obj()
  }
}

case object LayoutEmpty extends LayoutSpec

case class ImageLayout(`icon-image`: IconName, `icon-offset`: Option[Seq[Int]], visibility: Option[String] = None)
    extends LayoutSpec
object ImageLayout {
  implicit val json = Json.format[ImageLayout]
}

case class TextLayout(`text-field`: String, `text-size`: Int, visibility: Option[String] = None) extends LayoutSpec
object TextLayout {
  implicit val json = Json.format[TextLayout]
}

sealed abstract class Operator(val value: String)
object Operator {
  case object Eq extends Operator("==")
  case object Gt extends Operator(">=")
  case object Lt extends Operator("<=")
}

sealed abstract class Combinator(val name: String)

object Combinator {
  case object All extends Combinator("all")
}

sealed trait FilterLike

object FilterLike {
  implicit val writer: Writes[FilterLike] = Writes[FilterLike] {
    case fs @ FilterSpec(_, _, _) => FilterSpec.writer.writes(fs)
    case mf @ MultiFilter(_, _)   => MultiFilter.writer.writes(mf)
  }
}

case class FilterSpec(op: Operator, property: String, value: Int) extends FilterLike
object FilterSpec {
  implicit val writer: Writes[FilterSpec] = Writes[FilterSpec] { fs =>
    Json.arr(fs.op.value, fs.property, fs.value)
  }
}

case class MultiFilter(op: Combinator, operands: Seq[FilterSpec]) extends FilterLike

object MultiFilter {
  implicit val writer: Writes[MultiFilter] = Writes[MultiFilter] { mf =>
    Json.arr(mf.op.name) ++ JsArray(mf.operands.map { fs =>
      FilterSpec.writer.writes(fs)
    })
  }
}

case class Paint(`circle-color`: Option[String] = None, `fill-color`: Option[String] = None)

object Paint {
  implicit val json = Json.format[Paint]
}

case class LayerSpec(id: LayerId,
                     `type`: String,
                     layout: LayoutSpec,
                     source: SourceId,
                     filter: Option[FilterLike] = None,
                     `source-layer`: SourceLayerId,
                     paint: Option[Paint] = None,
                     minzoom: Option[Int] = None)
object LayerSpec {
  implicit val json = Json.writes[LayerSpec]
}
