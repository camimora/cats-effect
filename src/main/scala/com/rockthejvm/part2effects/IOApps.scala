package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode
import scala.io.StdIn

object IOApps:
  val program: IO[Unit] =
    for
      written <- IO(StdIn.readLine())
      _ <- IO(println(s"You just written: $written"))
    yield ()

object FirstCEApp extends IOApp:
  import IOApps._
  override def run(args: List[String]): IO[ExitCode] = 
    program.redeemWith[ExitCode](
      e => IO(s"There was an error: ${e.getMessage}").map(println) *> IO(ExitCode.Error),
      _ => IO("All good!").map(println) *> IO(ExitCode.Success)
    )
