package scala.meta.internal.fastpass

import org.eclipse.lsp4j.jsonrpc.CancelChecker
import java.io.OutputStream
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.meta.internal.fastpass.pantsbuild.MessageOnlyException
import scala.sys.process._

object SystemProcess {
  def run(
      shortName: String,
      args: List[String],
      reproduceArgs: List[String],
      cwd: Path,
      token: CancelChecker,
      out: OutputStream,
      err: OutputStream
  )(implicit ec: ExecutionContext): Unit = {
    val io = BasicIO
      .standard(false)
      .withOutput(is => BasicIO.transferFully(is, out))
      .withError(is => BasicIO.transferFully(is, err))
    val exportTimer = new Timer(Time.system)
    scribe.info(args.mkString("process: ", " ", ""))
    val process = Process(args, cwd = Some(cwd.toFile())).run(io)
    RunningProcesses.submitNewProcess(process)
    val exit = process.exitValue()
    if (exit != 0) {
      val message = s"$shortName command failed with exit code $exit, " +
        s"to reproduce run the command below:\n\t${reproduceArgs.mkString(" ")}"
      throw MessageOnlyException(message)
    } else {
      scribe.info(s"time: ran '$shortName' in $exportTimer")
    }
  }

  object RunningProcesses {
    var allRunningProcesses: scala.collection.concurrent.Map[Int, Process] =
      new ConcurrentHashMap[Int, Process]().asScala

    def submitNewProcess(process: Process): Option[Process] = {
      val processesToRemove = allRunningProcesses
        .filter { case (_, process) => !process.isAlive() }
        .map { case (hash, _) => hash }
      processesToRemove.foreach { hash =>
        allRunningProcesses.remove(hash)
      }

      allRunningProcesses.put(process.hashCode(), process)
    }

    def destroyAll(): Unit = {
      SystemProcess.RunningProcesses.allRunningProcesses.values.foreach { p =>
        p.destroy()
      }
    }
  }

}
