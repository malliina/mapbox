package com.malliina.mapbox

import java.nio.file.Paths
import munit.FunSuite

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class BaseSuite extends FunSuite:
  val userHome = Paths.get(sys.props("user.home"))
  implicit val ec: ExecutionContext = munitExecutionContext

  def await[T](f: Future[T]): T = Await.result(f, 7200.seconds)
