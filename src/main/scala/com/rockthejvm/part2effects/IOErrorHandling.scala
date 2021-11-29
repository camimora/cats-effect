package com.rockthejvm.part2effects

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object IOErrorHandling extends App:
  optionIO(Some(2)).map(println).unsafeRunSync()
  //optionIO(None).map(println).unsafeRunSync()

  try2IO(Try(10)).map(println).unsafeRunSync()
  //try2IO(Try(throw RuntimeException("Try BOOM!"))).map(println).unsafeRunSync()

  either2IO(Right(20)).map(println).unsafeRunSync()
  //either2IO(Left(new RuntimeException("Either BOOM!"))).map(println).unsafeRunSync()

def optionIO[A](maybeInt: Option[A]): IO[A] =
  maybeInt.fold(
    IO.raiseError(new RuntimeException("Empty Option"))
  )(myA => myA.pure[IO])

def try2IO[A](myTry: Try[A]): IO[A] =
  myTry match
    case Success(res) => res.pure[IO]
    case Failure(e)   => IO.raiseError[A](e)

def either2IO[A](myEither: Either[Throwable, A]): IO[A] =
  myEither match
    case Right(res) => res.pure[IO]
    case Left(e)   => IO.raiseError[A](e)

def handleIOError[A](io: IO[A])(handle: Throwable => A): IO[A] =
  io.redeem(
    handle,
    identity 
  )

def handleIOErrorWith[A](io: IO[A])(handle: Throwable => IO[A]): IO[A] =
  io.redeemWith(
    handle,
    _.pure[IO]
  )

