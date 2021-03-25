import zio._
import zio.console._
import io.github.arainko.Cipher
import io.github.arainko.cli._
import io.github.arainko.cli.Argument._
import io.github.arainko.model._

object Xor extends App {

  def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val argument = args.headOption
      .toRight(CliError("Argument missing"))
      .flatMap(Argument.fromString)

    ZIO
      .fromEither(argument)
      .flatMap(dispatch)
      .tapError { err =>
        err match {
          case CliError(message)         => putStrLnErr(message)
          case FileMissingError(message) => putStrLnErr(message)
          case FileFormatError(message)  => putStrLnErr(message)
        }
      }
      .fold(_ => ExitCode.failure, _ => ExitCode.success)
  }

  private def dispatch(argument: Argument) =
    argument match {
      case Prepare        => Cipher.prepareM
      case Encode         => Cipher.cryptoM
      case Cryptoanalysis => Cipher.cryptoanalysisM
    }
}
