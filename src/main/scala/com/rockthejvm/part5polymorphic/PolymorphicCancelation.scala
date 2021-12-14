package com.rockthejvm.part5polymorphic

import cats.effect.IOApp
import cats.effect.{IO, Sync, GenSpawn}
import scala.concurrent.duration.*
import cats.effect.kernel.MonadCancel
import com.rockthejvm.utils.general.*
import cats.Functor
import cats.syntax.all.*
import cats.effect.syntax.all.*
import cats.implicits.*

object PolymorphicCancelation extends IOApp.Simple:

  //1

  def myGeneralLogin[F[_], E](using GenSpawn[F, E]) = {
    val inputPassword: F[String] = 
      GenSpawn[F, E].pure("Input password:").myDebug >> 
        GenSpawn[F, E].pure("(typing password)").myDebug >> 
        GenSpawn[F, E].unit.unsafeSleep(2.seconds) >> 
        GenSpawn[F, E].pure("RockTheJVM1!")

    val verifyPassword: String => F[Boolean] = (pw: String) => 
      GenSpawn[F, E].pure("verifying...").myDebug >> 
        GenSpawn[F, E].pure("").unsafeSleep(2.seconds) >> 
        GenSpawn[F, E].pure(pw == "RockTheJVM1!")

    val authFlow: F[Unit] = GenSpawn[F, E].uncancelable { poll =>
      for {
        pw <- poll(inputPassword).onCancel(GenSpawn[F, E].pure("Authentication timed out. Try again later.").myDebug.void) // this is cancelable
        verified <- verifyPassword(pw) // this is NOT cancelable
        _ <- if (verified) GenSpawn[F, E].pure("Authentication successful.").myDebug // this is NOT cancelable
            else GenSpawn[F, E].pure("Authentication failed.").myDebug
      } yield ()
    }

    for {
      authFib <- authFlow.start
      _ <- GenSpawn[F, E].unit.unsafeSleep(3.seconds) >> GenSpawn[F, E].pure("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
      _ <- authFib.join
    } yield ()
  }

  def run = myGeneralLogin[IO, Throwable]
