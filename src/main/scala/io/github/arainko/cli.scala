package io.github.arainko

import scala.util.control.NoStackTrace
import cats.syntax.either._
import io.github.arainko.cli.File._
import java.nio.file.Path
import java.nio.file.Paths

object cli {

  sealed trait ApplicationError extends NoStackTrace

  final case class CliError(message: String) extends ApplicationError
  final case class FileMissingError(message: String) extends ApplicationError
  final case class FileFormatError(message: String) extends ApplicationError

  sealed trait Argument

  object Argument {
    case object Prepare extends Argument
    case object Encode extends Argument
    case object Cryptoanalysis extends Argument

    def fromString(value: String): Either[CliError, Argument] = 
      value match {
        case "-p" => Prepare.asRight
        case "-e" => Encode.asRight
        case "-k" => Cryptoanalysis.asRight
        case other => CliError(s"$other is not a valid argument").asLeft
      }
  }

  sealed trait File {
    final def path: String = 
      this match {
        case Crypto => "files/crypto.txt"
        case Decrypt => "files/decrypt.txt"
        case Key => "files/key.txt"
        case Orig => "files/orig.txt"
        case Plain => "files/plain.txt"
      }

    final def javaPath: Path = Paths.get(this.path)
  }

  object File {
    case object Crypto extends File
    case object Decrypt extends File
    case object Key extends File
    case object Orig extends File
    case object Plain extends File
  }
}
