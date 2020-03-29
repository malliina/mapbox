package com.malliina.mapbox

import java.nio.file.Paths

import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class BaseSuite extends FunSuite with BeforeAndAfterAll {
  val userHome = Paths.get(sys.props("user.home"))

  def await[T](f: Future[T]): T = Await.result(f, 7200.seconds)
}
