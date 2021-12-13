package com.rockthejvm.part4coordination

import cats.effect.IOApp
import cats.effect.std.Semaphore
import cats.effect.IO
import cats.syntax.all.*
import com.rockthejvm.utils.*
import scala.concurrent.duration.*
import scala.util.Random

object Semaphores extends IOApp.Simple:
  
  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)
  
  //1

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) *> IO(Random.nextInt)

  val mutex = Semaphore[IO](1)

  val users: IO[List[Int]] = 
    mutex.flatMap{ sem => 
      (1 to 10).toList.parTraverse{ id =>
        for
          _ <- IO(s"[session $id] waiting to log in ...").debug
          _ <- sem.acquire
          _ <- IO(s"[session $id] logged in, working ...").debug
          res <- doWorkWhileLoggedIn()
          _ <- IO(s"[session $id] done: $res, logging out ...").debug
          _ <- sem.release
        yield res
      }
    }

  def run = users.debug.void
