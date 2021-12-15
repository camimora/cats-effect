package com.rockthejvm.part5polymorphic

import cats.effect.IOApp
import cats.effect.Spawn
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

object PolymorphicFibers extends IOApp.Simple:

  //1

  def simpleRace[F[_], A, B](ioa: F[A], iob: F[B])(using sp: Spawn[F]): F[Either[A, B]] =
    sp.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
        case Errored(e) => fibB.cancel >> sp.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => sp.raiseError(e)
          case Canceled() => sp.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fibA.cancel >> sp.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => sp.raiseError(e)
          case Canceled() => sp.raiseError(new RuntimeException("Both computations canceled."))
        }
      }
    }
    
  def run = ???
