package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.annotation._
import metaconfig.generic.Settings
import metaconfig.{ConfDecoder, ConfEncoder, generic}
import java.nio.file.{Path, Paths}
import scala.meta.internal.fastpass.pantsbuild.Codecs._
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath

case class SharedOptions(
    @Description("The root directory of the Pants build.")
    workspace: Path = PathIO.workingDirectory.toNIO,
    @Description(
      "The path to the `pants` executable. " +
        "Defaults to the `pants` executable in the workspace directory."
    )
    pants: Option[Path] = None,
    bazel: Option[Path] = None,
    useBazel: Boolean = false
) {
  def bloopDirectory: Path = workspace.resolve(".bloop")
  def pantsBinary: Path = pants.getOrElse(workspace.resolve("pants"))
  def bazelBinary: Path = bazel.getOrElse(workspace.resolve("tools/bazel"))
  val home: AbsolutePath = AbsolutePath {
    Option(System.getenv("FASTPASS_HOME")) match {
      case Some(value) => Paths.get(value)
      case None => workspace.resolveSibling("bsp-projects")
    }
  }
}

object SharedOptions {
  val default: SharedOptions = SharedOptions()
  implicit lazy val surface: generic.Surface[SharedOptions] =
    generic.deriveSurface[SharedOptions]
  implicit lazy val encoder: ConfEncoder[SharedOptions] =
    generic.deriveEncoder[SharedOptions]
  implicit lazy val decoder: ConfDecoder[SharedOptions] =
    generic.deriveDecoder[SharedOptions](default)
  implicit lazy val settings: Settings[SharedOptions] = Settings[SharedOptions]
}
