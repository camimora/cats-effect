package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import java.util.Scanner
import java.io.FileReader
import java.io.File
import scala.concurrent.duration.*
import cats.syntax.all.*
import com.rockthejvm.utils.*

object Resources extends IOApp.Simple:
  def run: IO[Unit] = 
    bracketReadFile("/Users/camimora/VSCodeProjects/cats-effect/test.txt")

def openFileScanner(path: String): IO[Scanner] =
  IO(new Scanner(new FileReader(new File(path))))

def bracketReadFile(path: String): IO[Unit] =
  openFileScanner(path)
    .bracket(readFile)(sc => 
      IO("Closing File").debug *> 
        IO.delay(sc.close)
    )

def readFile(scanner: Scanner): IO[Unit] =
  if scanner.hasNextLine then 
    IO.delay(scanner.nextLine).debug >>
      IO.sleep(100.millis) >> 
      readFile(scanner)
  else 
    IO(println("Doesn't have next :shrug:")) *>
      IO.unit

// object TempMain extends App:
//   val x = new Scanner(new FileReader(new File("/Users/camimora/VSCodeProjects/cats-effect/test.txt"))).hasNext
//   println(x)