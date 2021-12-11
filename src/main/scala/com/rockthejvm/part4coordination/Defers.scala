package com.rockthejvm.part4coordination

import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.traverse.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import cats.effect.FiberIO
import cats.effect.kernel.Fiber
import cats.syntax.all.*

object Defers extends IOApp.Simple {

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get // blocks the fiber
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[downloader] got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] File download complete").debug
           else IO("[notifier] downloading...").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef) // busy wait!
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  // deferred works miracles for waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // blocks until the signal is completed
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  def alarmNotification(): IO[Unit] =
    val refSeconds: IO[Ref[IO, Int]] = IO.ref(0)
    val defAlarm: IO[Deferred[IO, Unit]] = IO.deferred

    def ticking(refSeconds: Ref[IO, Int], defAlarm: Deferred[IO, Unit]): IO[Unit] =
      for
        _ <- IO("Ticking...").debug
        _ <- IO.sleep(1.second)
        updatedSeconds <- refSeconds.updateAndGet(_ + 1)
        _ <- IO(s"Ticking updated: $updatedSeconds").debug
        _ <- if updatedSeconds == 10 then defAlarm.complete(()) else ticking(refSeconds, defAlarm)
      yield ()

    def alarmLogger(defAlarm: Deferred[IO, Unit]): IO[Unit] =
      for
        _ <- IO("Logger, waiting ...").debug
        _ <- defAlarm.get
        _ <- IO("ALARM!!! ACHTUNG!!! ACHTUNG!!!").debug
      yield ()

    for
      ref <- refSeconds
      defe <- defAlarm
      fibTick <- ticking(ref, defe).start
      fibAlarm <- alarmLogger(defe).start
      _ <- fibTick.join
      _ <- fibAlarm.join
    yield ()

  //2

  
  def task1(): IO[Unit] = 
      IO("[Fib1] Starting").debug >> 
        IO.sleep(2.seconds) >> 
        IO("[Fib1] Finishing").debug.void // >> defe.complete(Outcome.succeeded(IO.unit)).void

    def task2(): IO[Unit] = 
      IO("[Fib2] Starting").debug >> 
        IO.sleep(1.seconds) >> 
        IO("[Fib2] Finishing").debug.void // >> defe.complete(Outcome.succeeded(IO.unit)).void

  type EitherOutcomes[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]
  type MySignal[A, B] =  IO[Deferred[IO, EitherOutcomes[A, B]]]

  // def deferredRacePair[A, B](io1: IO[A], io2: IO[B]): IO[Either[(Outcome[IO, Throwable, A], FiberIO[B]), (FiberIO[A], Outcome[IO, Throwable, B])]] =
  def deferredRacePair[A, B](io1: IO[A], io2: IO[B]): IO[Either[(Outcome[IO, Throwable, A], FiberIO[B]), (FiberIO[A], Outcome[IO, Throwable, B])]] =

    def signal: MySignal[A, B] = IO.deferred

    def refFiberLeft: IO[Ref[IO, Option[FiberIO[A]]]] = IO.ref[Option[FiberIO[A]]](None)
    def refFiberRight: IO[Ref[IO, Option[FiberIO[B]]]] = IO.ref[Option[FiberIO[B]]](None)
    
    def runLeft(io: IO[A], defe: Deferred[IO, EitherOutcomes[A, B]], ref: Ref[IO, Option[FiberIO[A]]]): IO[Unit] = 
      for
        fibL <- io.start
        _ <- IO("[Left] Setting fiber ref").debug >> ref.updateAndGet(_ => fibL.some).debug
        res <- fibL.join 
        _ <- defe.complete(Left(res)) 
      yield ()
    
    def runRight(io: IO[B], defe: Deferred[IO, EitherOutcomes[A, B]], ref: Ref[IO, Option[FiberIO[B]]]): IO[Unit] = 
      for
        fib <- io.start
        _ <- IO("[Right] Setting fiber ref").debug >> ref.updateAndGet(_ => fib.some).debug
        res <- fib.join 
        _ <- defe.complete(Right(res)) 
      yield ()

    for
      mySignal <- signal
      leftRef <- refFiberLeft
      rightRef <- refFiberRight
      _ <- runLeft(io1, mySignal, leftRef).start
      _ <- runRight(io2, mySignal, rightRef).start
      res <- mySignal.get
      leftFib <- leftRef.get
      rightFib <- rightRef.get
    yield 
      res match
        case Left(io1Res) => Left(io1Res, rightFib.get)
        case Right(io2Res) => Right(leftFib.get, io2Res)

    // for
    //   defe <- signal
    //   fib1 <- io1.start.guaranteeCase{ 
    //     case Succeeded(res) => res.flatMap(_.join.flatMap(r1 => defe.complete(Left(r1)).void)) 
    //     case Errored(e) => defe.complete(Left(Errored(e))).void
    //     case Canceled() => defe.complete(Left(Canceled())).void
    //   }
    //   fib2 <- io2.start.guaranteeCase{
    //     case Succeeded(res) => defe.complete(Right(Succeeded(res))).void
    //     case Errored(e) => defe.complete(Right(Errored(e))).void
    //     case Canceled() => defe.complete(Right(Canceled())).void
    //   }
    //   outcomeFirst <- defe.get
    // yield 
    //   outcomeFirst match
    //     case Left(res) => Left((res, fib2))
    //     case Right(res) => Right((fib1, res))

  override def run = deferredRacePair(task1(), task2()).debug.void
}
