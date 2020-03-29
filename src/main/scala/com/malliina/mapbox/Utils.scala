package com.malliina.mapbox

import java.io.FileOutputStream
import java.nio.file.Path
import java.text.Normalizer
import java.util.zip.ZipFile

import org.apache.commons.io.IOUtils

import scala.jdk.CollectionConverters.IteratorHasAsScala
import org.apache.commons.text.{CharacterPredicates, RandomStringGenerator}

import scala.concurrent.{ExecutionContext, Future}

object Utils {
  private val generator = new RandomStringGenerator.Builder()
    .withinRange('a', 'z')
    .filteredBy(CharacterPredicates.LETTERS)
    .build()

  def randomString(length: Int) = generator.generate(length).toLowerCase

  def normalize(input: String): String =
    Normalizer
      .normalize(input, Normalizer.Form.NFD)
      .replaceAll("[^\\p{ASCII}]", "")
      .toLowerCase
      .replaceAll("[^-a-zA-Z0-9]", "-")
      .trim

  def unzip(zipFile: Path): Seq[Path] = unzip(zipFile, zipFile.getParent)

  def unzip(zipFile: Path, outDir: Path): Seq[Path] = {
    val outDir = zipFile.getParent
    val zip = new ZipFile(zipFile.toFile)
    try {
      zip
        .entries()
        .asIterator()
        .asScala
        .map { entry =>
          val out = outDir.resolve(entry.getName)
          if (entry.isDirectory) {
            out.toFile.mkdirs()
          } else {
            val inStream = zip.getInputStream(entry)
            try {
              val outStream = new FileOutputStream(out.toFile)
              try {
                IOUtils.copy(inStream, outStream)
              } finally outStream.close()
            } finally inStream.close()
          }
          out
        }
        .toList
    } finally zip.close()
  }
}
