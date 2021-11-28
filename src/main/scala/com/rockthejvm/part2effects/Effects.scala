package com.rockthejvm.part2effects

import scala.io.StdIn.readLine

object Effects extends App:
  println(currentSystemMillisIO)
  println(currentSystemMillisIO.unsafeRun())

  val sleep1Second: () => Unit = () => Thread.sleep(1000)

  println(
    elapsedIO(
      MyIO[Unit](sleep1Second)
    )
  )

  println(
    elapsedIO(
      MyIO(sleep1Second)
    ).unsafeRun()
  )

  printIO("Hola!").unsafeRun()
  
  val x =
    for
      _    <- printIO("Enter your name!")
      name <- readIO
      _    <- printIO(s"Hi $name")
    yield ()

  x.unsafeRun()

val readIO: MyIO[String]             = MyIO(() => readLine())
def printIO(msg: String): MyIO[Unit] = MyIO(() => println(msg))

val currentSystemMillisIO: MyIO[Long] =
  MyIO(() => System.currentTimeMillis())

def elapsedIO(task: MyIO[Unit]): MyIO[Long] =
  for
    before <- currentSystemMillisIO
    _      <- task
    after  <- currentSystemMillisIO
  yield after - before

case class MyIO[A](unsafeRun: () => A):
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() =>
      f(unsafeRun())
        .unsafeRun()
    )
