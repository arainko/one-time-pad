package io.github.arainko

import io.github.arainko.model._
import scodec._
import scodec.bits._
import java.nio.charset.StandardCharsets
import scala.util.Try

object OneTimePad {

  def encrypt(plain: Plain, key: Key): Crypto = {
    val keyBytes = ByteVector.view(key.value.getBytes(StandardCharsets.US_ASCII))
    Crypto {
      plain.value.linesIterator.toSeq
        .map(ByteVector.encodeAscii)
        .map(_.toOption.get)
        .map(_.xor(keyBytes))
        .map(_.decodeAscii.toOption.get)
        .mkString("\n")
    }
  }

  private def possibleKey(cryptoLines: List[String]): Key = {
    val dirtyKey = Array.fill(Config.keyLength)(' ')
    cryptoLines.foreach { line =>
      line.zipWithIndex.foreach { case (char, idx) =>
        val maybeKey = (char ^ ' ').toChar
        if (char > 32 && maybeKey >= 32 && maybeKey <= 127)
          dirtyKey.update(idx, maybeKey)
      }
    }
    Key(dirtyKey.toList.mkString)
  }

  def decrypt(crypto: Crypto): Decrypt = {
    val length = Config.keyLength + 1 // account for the newline char
    val lines = List.unfold(crypto.value) { value =>
      val piece     = value.take(length)
      val remainder = value.drop(length)
      Option.when(!piece.isEmpty)(piece -> remainder)
    }
    val maybeKey = possibleKey(lines)
    val keyBytes = ByteVector.view(maybeKey.value.getBytes(StandardCharsets.US_ASCII))
    Decrypt {
      lines
        .map(ByteVector.encodeAscii)
        .map(_.toOption.get)
        .map(_.xor(keyBytes))
        .map(_.decodeAscii.toOption.get)
        .mkString("\n")
    }
  }
}
