package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Traverse
import com.rockthejvm.utils.*
import scala.concurrent.duration.*
import cats.syntax.parallel.*

object IOTraversal extends IOApp.Simple:
  def run: IO[Unit] = 
    // traversalIOs.map(_.sum).debug.void
    parallelTraversalIO.map(_.sum).debug.void

def heavyComputation(string: String): Future[Int] = Future {
  Thread.sleep(Random.nextInt(1000))
  string.split(" ").length
}

val workload: List[String] = List("1 2 3", "1 2 3 4 5", "1 2")

def clunkyFutures(): Unit = {
  val futures: List[Future[Int]] = workload.map(heavyComputation)
  futures.foreach(_.foreach(println))
}

def traverseFuture(): Unit = {
  val singleFuture: Future[List[Int]] = Traverse[List].traverse(workload)(heavyComputation)
  singleFuture.foreach(print)
}


def computeAsIO(string: String): IO[Int] = IO {
  IO.sleep(Random.nextInt(1000).millis)
  string.split(" ").length
}.debug

def clunkyIOs: List[IO[Int]] = workload.map(computeAsIO)

val traversalIOs: IO[List[Int]] = 
  Traverse[List].traverse(workload)(computeAsIO)

val parallelTraversalIO: IO[List[Int]] = 
  workload.parTraverse(computeAsIO)

//Exercises

def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
  Traverse[List].traverse(listOfIOs)(identity)

def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
  Traverse[F].traverse(wrapperOfIOs)(identity)

def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
  listOfIOs.parTraverse(identity)

def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
  wrapperOfIOs.parTraverse(identity)
