package com.rockthejvm.part2effects

import cats.effect.IO
import cats.syntax.apply.*
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*

object IOIntroduction extends App:
  // sumIO(3).map(println).unsafeRunSync()
  // fibonacci(6).map(println).unsafeRunSync()
  IO(println("Hola")).as(10).map(println).unsafeRunSync()

def smallProgramme(): IO[Unit] =
  (IO("Hello"), IO("Bye")).mapN((a, b) => println(a + " - " + b))

def sequenceTakeLast[A, B](ioA: IO[A], ioB: IO[B]): IO[B] =
  ioA.flatMap(_ => ioB)

def sequenceTakeFirst[A, B](ioA: IO[A], ioB: IO[B]): IO[A] =
  ioA.flatMap(a => ioB.map(_ => a))

def forever[A](io: IO[A]): IO[A] =
  io.flatMap(_ => forever(io))

def convert[A, B](ioA: IO[A], value: B): IO[B] =
  ioA.map(_ => value)

def asUnit[A](ioA: IO[A]): IO[Unit] = 
  ioA.map(_ => ())
  
def sum(n: Int): Int =
  if n <= 0 then 0
  else n + sum(n-1) 

def sumIO(n: Int): IO[Int] =
  def sumAccum(left: Int, accum: Int): IO[Int] = 
    if left <= 0 then IO.pure(accum)
    else sumAccum(left - 1, accum + left)

  sumAccum(n, 0)

def fibonacci(n: Int): IO[BigInt] =
  n match
    case 0 => IO.pure(0)
    case 1 => IO.pure(1)
    case _ => 
      for
        nMinus1 <- fibonacci(n-1)
        nMinus2 <- fibonacci(n-2)
      yield
          nMinus1 + nMinus2
