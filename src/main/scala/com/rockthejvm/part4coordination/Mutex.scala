package com.rockthejvm.part4coordination

import cats.effect.IO
import cats.effect.IOApp
import scala.util.Random
import scala.concurrent.duration.*
import com.rockthejvm.utils.*
import cats.syntax.parallel.*
import cats.effect.kernel.Ref
import cats.effect.kernel.Deferred
import scala.collection.immutable.Queue
import cats.syntax.all.*

abstract class Mutex:
  def acquire: IO[Unit]
  def release: IO[Unit]

object Mutex:
  def create: IO[Mutex] = 
    Ref[IO].of(Queue.empty).flatMap{ (waitingQDef: Ref[IO, Queue[Deferred[IO, Unit]]]) =>
      Ref[IO].of(false).map{ (isBusyR: Ref[IO, Boolean]) =>
        new Mutex {
          def acquire: IO[Unit] = 
            for
              waitingQ <- waitingQDef.get
              isBusy <- isBusyR.get
              _ <- IO(s"Is it busy? => $isBusy").debug
              _ <-  
                if isBusy then
                  IO.deferred[Unit]
                    .flatMap(newDefe => 
                      waitingQDef.modify(x => (x.enqueue(newDefe), newDefe.get))
                        
                    ).flatten
                else isBusyR.modify(_ => (true, IO.unit)).flatten
            yield ()

          def release: IO[Unit] = 
            for
              waitingQ <- waitingQDef.get
              isBusy <- isBusyR.get
              _ <- waitingQ.dequeueOption match
                case Some((x, y)) => 
                  waitingQDef.modify(_ => (y , ())) *> 
                    isBusyR.modify(x => (true, ())) *>
                    x.complete(()).void
                case None => 
                  isBusyR.modify(x => (false, ()))
            yield ()
        }
      }  
    }

object MutexPlayground extends IOApp.Simple:

  def criticalTask():IO[Int] = IO.sleep(2.second) >> IO(Random.nextInt(100))
  def createNonLockingTask(id: Int): IO[Int] = 
    for
      _ <- IO(s"[task $id] Working").debug
      res <- criticalTask()
      _ <- IO(s"[task $id] got result $res").debug
    yield res

  def demoNonLockingTask(): IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)

  def demoLockingTask(id: Int, mutex: Mutex): IO[Int] =
    for
      _ <- IO(s"[task $id] Waiting for permission").debug
      _ <- mutex.acquire
      _ <- IO(s"[task $id] Working").debug
      res <- criticalTask()
      _ <- IO(s"[task $id] got result $res").debug
      _ <- mutex.release
      _ <- IO(s"[task $id] Locked removed").debug
    yield res
    
  def demoLockingTasks(): IO[Unit] =
    for
      mutex <- Mutex.create
      res <- (1 to 10).toList.parTraverse(id => demoLockingTask(id, mutex))
    yield ()
  def run: IO[Unit] = demoLockingTasks().debug.void