package com.rockthejvm.utils

import cats.effect.IO

extension [A](myIO: IO[A])
  def debug: IO[A] =
    for
      res <- myIO
      _ = println(s"[${Thread.currentThread.getName}] - $res")
    yield res
