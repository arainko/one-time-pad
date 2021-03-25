package io.github.arainko

import io.github.arainko.cli._
import io.github.arainko.model._
import zio.ZIO
import zio.blocking.Blocking
import zio.stream._

import java.nio.charset.StandardCharsets

object Cipher {

  private val alphabet = ('a' to 'z').toSet + ' '

  private def readFile(file: File) =
    ZStream
      .fromFile(file.javaPath)
      .transduce(Transducer.utf8Decode)
      .runCollect
      .map(_.mkString)
      .mapError(err => FileMissingError(err.getMessage))

  private def writeToFile(content: String, file: File) =
    ZStream
      .fromIterable(content.getBytes(StandardCharsets.UTF_8))
      .run(Sink.fromFile(file.javaPath))
      .mapError(err => FileFormatError(err.getMessage))

  def prepareM: ZIO[Blocking, ApplicationError, Unit] =
    for {
      original <- readFile(File.Orig)
      droppedNewlines = original.toLowerCase.filter(alphabet.contains)
      keyLenght       = Config.keyLength
      normalized = List
        .unfold(droppedNewlines) { value =>
          val piece     = value.take(keyLenght)
          val remainder = value.drop(keyLenght)
          Option.when(!piece.isEmpty)(piece -> remainder)
        }
        .mkString("\n")
      _ <- writeToFile(normalized, File.Plain)
    } yield ()

  def cryptoM: ZIO[Blocking, ApplicationError, Unit] =
    for {
      plain <- readFile(File.Plain).map(Plain)
      key   <- readFile(File.Key).map(Key)
      validatedKey <- ZIO.fromEither(key.validated)
      encrypted = OneTimePad.encrypt(plain, validatedKey)
      _ <- writeToFile(encrypted.value, File.Crypto)
    } yield ()

  def cryptoanalysisM = 
    for {
      crypto <- readFile(File.Crypto).map(Crypto)
      decrypted = OneTimePad.decrypt(crypto)
      _ <- writeToFile(decrypted.value, File.Decrypt)
    } yield ()
}
