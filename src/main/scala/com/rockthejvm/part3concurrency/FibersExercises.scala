package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import cats.syntax.all.*
import cats.Traverse
import cats.implicits.*
import scala.concurrent.duration.*

object FibersExercises extends IOApp.Simple:

  def run: IO[Unit] = ???

//1

def processResultFromFiber[A](io: IO[A]): IO[A] =
    io
      .start
      .flatMap(_.join)
      .flatMap{
        case Succeeded(res) => res
        case Errored(e) => IO.raiseError[A](e)
        case Canceled() => IO.raiseError[A](new RuntimeException("I was cancelled"))
    }

//2

def tupleIOs[A, B](ioA: IO[A], ioB: IO[B]): IO[(A, B)] = 
  ioA.start.flatMap{ fib1 =>
    ioB.start.flatMap{ fib2 =>
      fib1.join.flatMap{ res1 =>
        fib2.join.flatMap{ res2 =>
            (res1, res2) match 
              case (Succeeded(r1), Succeeded(r2)) => (r1, r2).mapN((_, _))
              case (Errored(e1), _) => IO.raiseError(e1)
              case (_, Errored(e2)) => IO.raiseError(e2)
              case _ => IO.raiseError(new RuntimeException("Some of the fibers was cancelled"))
          }
        }
      }
    }

// 3
def timeout[A](ioA: IO[A], duration: FiniteDuration): IO[A] =
  for
    fib <- ioA.start
    fibTimeOut <- IO.sleep(duration) >> fib.cancel
    res1 <- fib.join 
  yield
    ???
