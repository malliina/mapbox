package com.malliina.mapbox

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import com.malliina.http.FullUrl
import com.malliina.mapbox.JobStatus.{Failed, Processing, Queued, Success}
import com.malliina.util.AppLogger
import com.malliina.values.{StringCompanion, StringEnumCompanion, Username, WrappedString}
import org.apache.commons.io.FilenameUtils
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.Encoder.encodeJsonObject

abstract class BoatCompanion[T <: WrappedString] extends StringCompanion[T]:
  def random(prefix: String = ""): T = apply(s"$prefix${Utils.randomString(6)}")

/** Name of an icon in a sprite of a style.
  */
case class IconName(value: String) extends WrappedString
object IconName extends StringCompanion[IconName]

case class StyleId(value: String) extends WrappedString
object StyleId extends StringCompanion[StyleId]

case class LayerId(value: String) extends WrappedString
object LayerId extends StringCompanion[LayerId]

case class SourceId(value: String) extends WrappedString
object SourceId extends BoatCompanion[SourceId]

/** Totally distinct from LayerId.
  *
  * @see
  *   https://docs.mapbox.com/help/glossary/source-layer/
  */
case class SourceLayerId(value: String) extends WrappedString
object SourceLayerId extends StringCompanion[SourceLayerId]

case class TilesetId(value: String) extends WrappedString
object TilesetId extends StringCompanion[TilesetId]:
  def apply(username: Username, name: TilesetName): TilesetId = apply(s"$username.$name")

case class TilesetName(value: String) extends WrappedString
object TilesetName extends BoatCompanion[TilesetName]

case class TilesetSourceId(value: String) extends WrappedString
object TilesetSourceId extends BoatCompanion[TilesetSourceId]:
  val MaxLength = 32

case class JobId(value: String) extends WrappedString
object JobId extends StringCompanion[JobId]

sealed abstract class JobStatus(val status: String):
  override def toString: String = status
  def isInProgress: Boolean = this == Queued || this == Processing
  def isCompleted: Boolean = this == Success || this == Failed

object JobStatus extends StringEnumCompanion[JobStatus]:
  // https://docs.mapbox.com/api/maps/#publish-a-tileset
  override val all: Seq[JobStatus] = Seq(Queued, Processing, Success, Failed)

  override def write(t: JobStatus): String = t.status

  case object Queued extends JobStatus("queued")
  case object Processing extends JobStatus("processing")
  case object Success extends JobStatus("success")
  case object Failed extends JobStatus("failed")

case class GeneratedMap(style: StyleId, tileset: TilesetId)

case class GenerateMapRequest(
  name: String,
  urls: Seq[UrlTask],
  images: Seq[ImageFile],
  assetPrefix: String
)

case class LayerObject(
  source: TilesetSourceId,
  minzoom: Int,
  maxzoom: Int,
  features: Option[JsonObject],
  tiles: Option[JsonObject]
)
object LayerObject:
  implicit val json: Codec[LayerObject] = deriveCodec[LayerObject]

  def apply(source: TilesetSourceId): LayerObject = apply(source, 0, 16, None, None)

case class Recipe(version: Int, layers: Map[String, LayerObject]):
  def updated(newLayers: Map[String, LayerObject]) = Recipe(version, layers ++ newLayers)
  def ++(other: Recipe) = Recipe(version, layers ++ other.layers)

object Recipe:
  implicit val json: Codec[Recipe] = deriveCodec[Recipe]

  val empty = apply(1, Map.empty)

  def merge(rs: Seq[Recipe]): Recipe = rs.foldLeft(Recipe.empty)(_ ++ _)

  def apply(layers: Map[SourceLayerId, LayerObject]): Recipe =
    apply(1, layers.map { case (k, v) => k.value -> v })

case class RecipeResponse(id: TilesetId, recipe: Recipe)
object RecipeResponse:
  implicit val json: Codec[RecipeResponse] = deriveCodec[RecipeResponse]

case class StyledRecipe(recipe: Recipe, style: Seq[LayerSpec])

case class TilesetSpec(name: TilesetName, recipe: Recipe, description: Option[String] = None)

object TilesetSpec:
  implicit val json: Codec[TilesetSpec] = deriveCodec[TilesetSpec]

case class Tileset(id: TilesetId, name: TilesetName)

object Tileset:
  implicit val json: Codec[Tileset] = deriveCodec[Tileset]

case class SimpleLayer(id: String, `type`: String, paint: JsonObject, interactive: Boolean)

object SimpleLayer:
  implicit val json: Codec[SimpleLayer] = deriveCodec[SimpleLayer]

  def background(id: String): SimpleLayer =
    SimpleLayer(id, "background", JsonObject("background-color" -> "#111".asJson), true)

case class SourcedLayer(id: String, source: SourceId)

object SourcedLayer:
  implicit val json: Codec.AsObject[SourcedLayer] = deriveCodec[SourcedLayer]

case class Style(
  id: String,
  version: Int,
  name: Option[String],
  sources: JsonObject,
  created: String
) //, layers: Seq[SourcedLayer])

object Style:
  implicit val json: Codec.AsObject[Style] = deriveCodec[Style]

case class StyleSource(url: String, `type`: String)

object StyleSource:
  implicit val json: Codec.AsObject[StyleSource] = deriveCodec[StyleSource]

  def vector(url: String) = StyleSource(url, "vector")

case class FeatureCollection(features: Seq[JsonObject])

object FeatureCollection:
  implicit val json: Codec.AsObject[FeatureCollection] = deriveCodec[FeatureCollection]
  private val log = AppLogger(getClass)

  def lineDelimit(in: Path, out: Path): Path =
    log.info(s"Line-delimiting '$in' to '${out.toAbsolutePath}'...")
    val coll = io.circe.parser
      .decode[FeatureCollection](Files.readString(in, StandardCharsets.UTF_8))
      .getOrElse(throw new Exception(s"Failed to read $in as JSON."))
    log.info(s"Parsed '$in' as JSON, writing...")
    val lines = coll.features.map(f => f.asJson.noSpaces).mkString("\n")
    Files.write(out, lines.getBytes(StandardCharsets.UTF_8))

case class PublishResponse(message: String, jobId: JobId)

object PublishResponse:
  implicit val json: Codec.AsObject[PublishResponse] = deriveCodec[PublishResponse]

case class JobStatusResponse(id: JobId, stage: JobStatus, tilesetId: TilesetId)

object JobStatusResponse:
  implicit val json: Codec.AsObject[JobStatusResponse] = deriveCodec[JobStatusResponse]

case class TilesetStatus(
  id: TilesetId,
  status: JobStatus,
  latest_job: JobId
)

object TilesetStatus:
  implicit val json: Codec.AsObject[TilesetStatus] = deriveCodec[TilesetStatus]

case class TilesetSourceCreated(id: TilesetSourceId, files: Int)

object TilesetSourceCreated:
  implicit val json: Codec.AsObject[TilesetSourceCreated] = deriveCodec[TilesetSourceCreated]

sealed abstract class Visibility(val name: String)

object Visibility:
  implicit val writer: Encoder[Visibility] = Encoder[Visibility] { v =>
    v.name.asJson
  }
  case object None extends Visibility("none")
  case object Visible extends Visibility("visible")

sealed trait LayoutSpec

object LayoutSpec:
  implicit val writer: Encoder[LayoutSpec] = Encoder[LayoutSpec] {
    case is @ ImageLayout(_, _, _) => ImageLayout.json(is)
    case ts @ TextLayout(_, _, _)  => TextLayout.json(ts)
    case vl @ VisibilityLayout(_)  => VisibilityLayout.writer(vl)
    case EmptyLayout               => JsonObject.empty.asJson
  }

case object EmptyLayout extends LayoutSpec

case class VisibilityLayout(visibility: Visibility) extends LayoutSpec
object VisibilityLayout:
  implicit val writer: Encoder[VisibilityLayout] = deriveEncoder[VisibilityLayout]

case class ImageLayout(
  `icon-image`: IconName,
  `icon-offset`: Option[Seq[Double]],
  visibility: Option[String] = None
) extends LayoutSpec
object ImageLayout:
  implicit val json: Codec[ImageLayout] = deriveCodec[ImageLayout]

case class TextLayout(`text-field`: String, `text-size`: Int, visibility: Option[String] = None)
  extends LayoutSpec
object TextLayout:
  implicit val json: Codec[TextLayout] = deriveCodec[TextLayout]

sealed abstract class Operator(val value: String)
object Operator:
  case object Eq extends Operator("==")
  case object Gt extends Operator(">=")
  case object Lt extends Operator("<=")

sealed abstract class Combinator(val name: String)

object Combinator:
  case object All extends Combinator("all")

sealed trait FilterLike

object FilterLike:
  implicit val writer: Encoder[FilterLike] = Encoder[FilterLike] {
    case fs @ FilterSpec(_, _, _) => FilterSpec.writer(fs)
    case mf @ MultiFilter(_, _)   => MultiFilter.writer(mf)
  }

case class FilterSpec(op: Operator, property: String, value: Int) extends FilterLike
object FilterSpec:
  implicit val writer: Encoder[FilterSpec] = Encoder[FilterSpec] { fs =>
    Json.arr(fs.op.value.asJson, fs.property.asJson, fs.value.asJson)
  }

case class MultiFilter(op: Combinator, operands: Seq[FilterSpec]) extends FilterLike
object MultiFilter:
  implicit val writer: Encoder[MultiFilter] = Encoder[MultiFilter] { mf =>
    val name = mf.op.name.asJson
    val more = mf.operands.map { fs =>
      FilterSpec.writer(fs)
    }
    Json.arr((name +: more)*)
  }

case class Paint(
  `circle-color`: Option[String] = None,
  `fill-color`: Option[String] = None,
  `fill-opacity`: Option[Double] = None,
  `line-color`: Option[String] = None,
  `line-width`: Option[Int] = None
)
object Paint:
  implicit val json: Codec[Paint] = deriveCodec[Paint]

case class LayerStyling(
  `type`: String,
  layout: LayoutSpec,
  source: SourceId,
  filter: Option[FilterLike] = None,
  paint: Option[Paint] = None,
  minzoom: Option[Double] = None,
  layerIdOverride: Option[LayerId] = None
):
  def toLayerSpec(layer: LayerId, sourceLayer: SourceLayerId) =
    LayerSpec(
      layerIdOverride.getOrElse(layer),
      `type`,
      layout,
      source,
      sourceLayer,
      filter,
      paint,
      minzoom
    )

case class LayerSpec(
  id: LayerId,
  `type`: String,
  layout: LayoutSpec,
  source: SourceId,
  `source-layer`: SourceLayerId,
  filter: Option[FilterLike] = None,
  paint: Option[Paint] = None,
  minzoom: Option[Double] = None
)

object LayerSpec:
  implicit val json: Encoder.AsObject[LayerSpec] = deriveEncoder[LayerSpec]

case class FullStyle(
  id: StyleId,
  version: Int,
  name: String,
  metadata: Option[JsonObject],
  sources: Option[Map[String, StyleSource]],
  sprite: Option[String],
  glyphs: Option[String],
  layers: Option[Seq[JsonObject]],
  owner: Username,
  modified: String,
  created: String
)

object FullStyle:
  implicit val json: Codec[FullStyle] = deriveCodec[FullStyle]

case class UpdateStyle(
  version: Int,
  name: String,
  metadata: Option[JsonObject],
  sources: Option[Map[String, StyleSource]],
  sprite: Option[String],
  glyphs: Option[String],
  layers: Option[Seq[JsonObject]],
  owner: Username,
  draft: Boolean
):
  def withLayers(newLayers: Seq[LayerSpec]): UpdateStyle =
    val asJson = newLayers.map { l =>
      LayerSpec.json.encodeObject(l)
    }
    copy(
      layers = Option(
        layers
          .getOrElse(Nil)
          .filterNot(l =>
            newLayers.exists(_.id == l.asJson.hcursor.downField("id").as[LayerId])
          ) ++ asJson
      )
    )

  def withoutLayer(id: LayerId) =
    copy(layers =
      layers.map(_.filterNot(obj => obj.asJson.hcursor.downField("id").as[LayerId] == id))
    )

  def withSource(source: SourceId, tileset: TilesetId): UpdateStyle =
    val styleSource = StyleSource.vector(s"mapbox://$tileset")
    copy(sources = Option(sources.getOrElse(Map.empty).updated(source.value, styleSource)))

object UpdateStyle:
  implicit val json: Codec[UpdateStyle] = deriveCodec[UpdateStyle]

trait GeoTask:
  def name: String
  def url: FullUrl
  def parts: Int

/** @param name
  *   name of shape asset to download; also the default layer ID
  * @param url
  *   shape zip URL
  * @param parts
  *   split factor
  * @param styling
  *   layer styles
  */
case class UrlTask(name: String, url: FullUrl, parts: Int, styling: Seq[LayerStyling])
  extends GeoTask:
  def toFiles(unzipped: Seq[Path]): FileTask = FileTask(name, unzipped, parts, styling)

case class FileTask(name: String, files: Seq[Path], parts: Int, styling: Seq[LayerStyling])

case class SourceLayerFile(sourceLayerId: SourceLayerId, file: Path)

object SourceLayerFile:
  def apply(file: Path): SourceLayerFile = SourceLayerFile(
    SourceLayerId(FilenameUtils.removeExtension(file.getFileName.toString)),
    file
  )

case class ImageFile(image: IconName, file: Path)

object ImageFile:
  def apply(icon: IconName): ImageFile = ImageFile(icon, Paths.get(s"data/images/$icon.svg"))

  def orFail(icon: IconName): ImageFile =
    val candidate = apply(icon)
    if Files.exists(candidate.file) then candidate
    else throw new NoSuchFileException(candidate.file.toString)
