package com.malliina.mapbox

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}

object Concurrent:
  private val exec = Executors.newSingleThreadScheduledExecutor()
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(exec)

  def scheduleIn[T](time: FiniteDuration)(code: => Future[T]): Future[T] =
    val p = Promise[T]()
    val runnable = new Runnable:
      override def run(): Unit = code.onComplete { result =>
        p.complete(result)
      }
    exec.schedule(runnable, time.toSeconds, TimeUnit.SECONDS)
    p.future

  def traverseSlowly[T, U](ls: Seq[T], parallelism: Int)(
    run: T => Future[U]
  )(implicit ec: ExecutionContext): Future[Seq[U]] =
    val (batch, remaining) = ls.splitAt(parallelism)
    if batch.isEmpty then Future.successful(Nil)
    else
      Future
        .traverse(batch) { t =>
          run(t)
        }
        .flatMap { batchResult =>
          traverseSlowly(remaining, parallelism)(run).map { nextResults =>
            batchResult ++ nextResults
          }
        }

  def close(): Unit = exec.shutdown()
