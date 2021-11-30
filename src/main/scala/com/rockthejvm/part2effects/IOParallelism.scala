package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.IO
import com.rockthejvm.utils.*
import cats.syntax.all.*
import cats.Parallel
import cats.effect.implicits.*

object IOParallelism extends IOApp.Simple{
  def run: IO[Unit] = 
    //composedIO.map(println)
    // meaningOfLife.map(println) *> favLang.map(println)
    // goalInLife.map(println)
    // parallelIO1
    // parallelIO2
    // goalInLife_v2.debug.void
    // goalInLife_v3.debug.void
    // parallelWithFailure.debug.void
    twoFailures.debug.void
}

val aniIO = IO(s"[${Thread.currentThread.getName}] Ani")
val kamranIO = IO(s"[${Thread.currentThread.getName}] Kamran")

val composedIO=
  for
    ani <- aniIO
    kamran <- kamranIO
  yield s"$ani and $kamran love Rock the JVM"

val meaningOfLife: IO[Int] = IO.delay(42)
val favLang: IO[String] = IO.delay("Scala")

val goalInLife = (meaningOfLife.debug, favLang.debug).mapN{ (ml, fl) =>
    s"My goal in life is $ml + $fl"
  }

val parallelIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
val parallelIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)

val goalInLifeParallel: IO.Par[String] = (parallelIO1, parallelIO2).mapN{ (ml, fl) =>
    s"My goal in life is $ml + $fl"
  }

val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

val goalInLife_v3: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN{ (ml, fl) =>
    s"My goal in life is $ml + $fl"
  }

val aFailure: IO[String] = IO.raiseError(new RuntimeException("Boom!"))

val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN{ (ml, fl) =>
    s"My goal in life is $ml + $fl"
  }

val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Cataboom!"))

val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
