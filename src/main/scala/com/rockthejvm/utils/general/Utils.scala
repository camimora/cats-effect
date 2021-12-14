package com.rockthejvm.utils.general

import cats.Functor
import cats.syntax.functor.*
import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.MonadCancel

extension [F[_], A](fa: F[A])
  def myDebug(using functor: Functor[F]) : F[A] =
    fa.map{ a =>
      val t = Thread.currentThread().getName
      println(s"[$t] $a")
      a
    }

  def unsafeSleep[E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

