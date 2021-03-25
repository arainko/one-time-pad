package io.github.arainko

import cli._

object model {
  final case class Crypto(value: String)  extends AnyVal
  final case class Decrypt(value: String) extends AnyVal

  final case class Key(value: String) extends AnyVal {

    def validated: Either[CliError, Key] =
      Either.cond(value.length == Config.keyLength, this, CliError(s"Wrong, key length configured to be ${Config.keyLength}"))
  }
  final case class Orig(value: String)  extends AnyVal
  final case class Plain(value: String) extends AnyVal
}
