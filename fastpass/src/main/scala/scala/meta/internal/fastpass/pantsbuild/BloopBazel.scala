package scala.meta.internal.fastpass.pantsbuild

import com.google.devtools.build.lib.analysis.AnalysisProtosV2
import java.nio.file.{Files, Path, Paths}
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.io.AbsolutePath

object BloopBazel {
  type TargetMap = Map[String, AnalysisProtosV2.ConfiguredTarget]

  val InterestingClasses = Set(
    "_scala_macro_library", "target_union", "resources_union", "thrift_library",
    "scala_junit_test", "java_library", "scala_binary", "scala_library",
    "scala_import", "third_party_jvm_import", "alias"
  )

  private class PathFragmentResolver(
      pathFragmentLookup: Map[Int, AnalysisProtosV2.PathFragment]
  ) {
    private[this] final val memo = mutable.Map[Int, String]()

    private def resolveImpl(id: Int, sb: StringBuilder): Unit = {
      val fragments = mutable.Buffer[String]()
      var curr = id
      while (curr != 0) {
        val pf = pathFragmentLookup(curr)
        fragments += pf.getLabel
        curr = pf.getParentId
      }

      sb ++= fragments.reverse.mkString("/")
    }

    def resolve(id: Int): String = {
      memo.getOrElseUpdate(
        id, {
          val sb = new StringBuilder
          resolveImpl(id, sb)
          sb.toString()
        }
      )
    }
  }

  private class DepSetResolver(
      depSetsById: Map[Int, AnalysisProtosV2.DepSetOfFiles],
      artifactsById: Map[Int, AnalysisProtosV2.Artifact],
      pathResolver: PathFragmentResolver
  ) {
    private[this] final val memo = mutable.Map[Int, Set[String]]()

    private def resolveImpl(
        id: Int,
        builder: mutable.Builder[String, _]
    ): Unit = {
      val depSet = depSetsById(id)
      val directFragments =
        depSet.getDirectArtifactIdsList.iterator().asScala.map { artId =>
          val artifact = artifactsById(artId)
          pathResolver.resolve(artifact.getPathFragmentId)
        }
      builder ++= directFragments

      depSet.getTransitiveDepSetIdsList
        .iterator()
        .asScala
        .foreach(d => resolveImpl(d, builder))
    }

    def resolve(id: Int): Set[String] = {
      memo.getOrElseUpdate(
        id, {
          val sb = Set.newBuilder[String]
          resolveImpl(id, sb)
          sb.result()
        }
      )
    }
  }

  final case class BuildIO(
      inputClasspath: Set[Path],
      sources: Set[Path],
      outputs: Set[Path],
      srcJar: Set[Path]
  )

  private def buildActionGraph(
      graph: AnalysisProtosV2.ActionGraphContainer
  ): Map[String, BuildIO] = {
    val ruleClassLookup = graph.getRuleClassesList
      .iterator()
      .asScala
      .map { rc => rc.getId -> rc.getName }
      .toMap

    val pathFragmentLookup = graph.getPathFragmentsList
      .iterator()
      .asScala
      .map { pf => pf.getId -> pf }
      .toMap

    val actionsByTarget = graph.getActionsList.asScala
      .groupBy(_.getTargetId)

    val targetLookup = graph.getTargetsList
      .iterator()
      .asScala
      .filterNot(_.getLabel.endsWith(".semanticdb"))
      .map { t => t.getLabel -> t }
      .toMap

    val depSetsById = graph.getDepSetOfFilesList
      .iterator()
      .asScala
      .map { ds => ds.getId -> ds }
      .toMap

    val artifactsById = graph.getArtifactsList
      .iterator()
      .asScala
      .map { art => art.getId -> art }
      .toMap

    val pathFragmentResolver = new PathFragmentResolver(pathFragmentLookup)
    val depSetResolver =
      new DepSetResolver(depSetsById, artifactsById, pathFragmentResolver)

    def resolveInputs(act: AnalysisProtosV2.Action): Iterator[String] = {
      act.getInputDepSetIdsList
        .iterator()
        .asScala
        .flatMap(ds => depSetResolver.resolve(ds))
    }

    def resolveOutputs(act: AnalysisProtosV2.Action): Iterator[String] = {
      act.getOutputIdsList.iterator().asScala.map { art =>
        pathFragmentResolver.resolve(artifactsById(art).getPathFragmentId)
      }
    }

    targetLookup.map {
      case (targetName, target) =>
        val targetActions = actionsByTarget(target.getId)
        val srcJarAction =
          targetActions.find(act => act.getMnemonic == "JavaSourceJar")
        val scalacAction =
          targetActions.find(act => act.getMnemonic == "Scalac")

        val scalacInputs = scalacAction.iterator
          .flatMap(resolveInputs)
          .filter(_.endsWith(".jar"))
          .toSet

        val scalacOutputs = scalacAction.iterator
          .flatMap(resolveOutputs)
          .filter(_.endsWith(".jar"))
          .toSet

        val srcJarInputs = srcJarAction.iterator
          .flatMap(resolveInputs)
          .filter(p => p.endsWith(".scala") || p.endsWith(".java"))
          .toSet

        val srcJarOutputs = srcJarAction.iterator
          .flatMap(resolveOutputs)
          .toSet

        //targetName -> BuildIO(
        //  inputClasspath = scalacInputs,
        //  sources = srcJarInputs,
        //  outputs = scalacOutputs,
        //  srcJar = srcJarOutputs)
        ???
    }
  }

  class Impl(
      args: Export,
      targetMap: TargetMap,
      directTargets: Set[String],
      buildIO: Map[String, BuildIO],
      internalSourcesDir: Path
  ) {
    private[this] final val transitiveMemo = mutable.Map[String, Set[String]]()

    private def directDeps(
        target: AnalysisProtosV2.ConfiguredTarget
    ): Set[String] = {
      target.getTarget.getRule.getAttributeList.asScala
        .find(_.getName == "deps") match {
        case None => Set.empty
        case Some(deps) =>
          deps.getStringListValueList.asScala.toSet
      }
    }

    private def transitiveDeps(
        root: AnalysisProtosV2.ConfiguredTarget
    ): Set[String] = {
      transitiveMemo.getOrElseUpdate(
        root.getTarget.getRule.getName, {
          val dd = directDeps(root)
          dd.flatMap { target =>
            transitiveDeps(targetMap(target))
          } ++ dd ++ Seq(root.getTarget.getRule.getName)
        }
      )
    }

    private def stringListAttr(
        target: AnalysisProtosV2.ConfiguredTarget,
        name: String
    ): Option[Seq[String]] = {
      target.getTarget.getRule.getAttributeList.asScala
        .find(_.getName == name)
        .map(_.getStringListValueList.asScala.toBuffer)
    }

    private def getJvmModuleInfo(
        target: AnalysisProtosV2.ConfiguredTarget
    ): Option[PantsLibrary] = {
      val tags = target.getTarget.getRule.getAttributeList.asScala
        .find(_.getName == "tags")
        .map { attr => attr.getStringListValueList.asScala }
        .getOrElse(Nil)

      val module = tags.find(_.startsWith("jvm_module")).map(_.substring(11))
      val version = tags.find(_.startsWith("jvm_version")).map(_.substring(12))

      (module, version) match {
        case (Some(m), Some(v))
            if buildIO.contains(target.getTarget.getRule.getName) =>
          Some(
            PantsLibrary(
              name = s"$m:$v",
              module = m,
              values = Map(
                "default" -> buildIO(
                  target.getTarget.getRule.getName
                ).outputs.head
              )
            )
          )
        case _ => None
      }
    }

    private def transitiveLibraries(
        root: AnalysisProtosV2.ConfiguredTarget
    ): Set[String] = {
      val myTransitiveDeps = transitiveDeps(root)
      //val myDirectDeps = directDeps(root)
      val mavenDeps = myTransitiveDeps.iterator
        .filter { dep => dep.startsWith("@maven//:") }
        .flatMap { dep =>
          val target = targetMap(dep)
          getJvmModuleInfo(target).map { lib =>
            lib.name
          }
        }
        .toSet

      mavenDeps
    }

    def collectScalaCompiler(): PantsScalaPlatform = {
      //val compilerTarget = targetMap("@io_bazel_rules_scala//scala:scala_compile_classpath_provider")
      //val allDeps = transitiveDeps(compilerTarget)
      PantsScalaPlatform.fromJson(ujson.Obj())
    }

    def collectLibraries(): java.util.HashMap[String, PantsLibrary] = {
      val out = new util.HashMap[String, PantsLibrary]()
      targetMap.valuesIterator
        .filter { t => t.getTarget.getRule.getName.startsWith("@maven//:") }
        .map(t => t -> getJvmModuleInfo(t))
        .collect { case (t, Some(libInfo)) => t -> libInfo }
        .foreach {
          case (t, libInfo) =>
            out.put(libInfo.name, libInfo)
        }
      out
    }

    def buildTargets(
        cquery: AnalysisProtosV2.CqueryResult
    ): Map[String, PantsTarget] = {
      cquery.getResultsList
        .iterator()
        .asScala
        .filterNot(_.getTarget.getRule.getName.endsWith(".semanticdb"))
        //.filter(ct => InterestingClasses.contains(ct.getTarget.getRule.getRuleClass))
        .flatMap { ct =>
          val targetName = ct.getTarget.getRule.getName
          val directorySafeName =
            if (targetName.startsWith("//")) {
              targetName.substring(2)
            } else {
              targetName
            }
          val id = directorySafeName.replace('/', '.')
          val directoryName = BloopPants.makeClassesDirFilename(id)
          val classesDir = Files.createDirectories(
            args.bloopDir.resolve(directoryName).resolve("classes")
          )
          val internalSourcesJar =
            internalSourcesDir.resolve(id + "-sources.jar")

          val baseDirectory = PantsConfiguration
            .baseDirectory(AbsolutePath(args.workspace), directorySafeName)
            .toNIO
          val targetRoot = args.project.common.workspace

          val myBuildIO = buildIO.get(targetName)

          val sources = myBuildIO
            .map { bio =>
              bio.sources.iterator
                .filter(_.getFileName.toString.endsWith(".scala"))
                .map(_.getParent)
                .toSet[Path]
                .map(targetRoot.relativize)
                .map(_.resolve("*.scala").toString)
            }
            .getOrElse(Set.empty)

          val sourceJar = myBuildIO.flatMap { bio =>
            bio.srcJar.headOption
          }

          val classpath = myBuildIO.map { bio =>
            bio.inputClasspath
          }

          val javaHome = "/usr/lib/jvm/java-1.8.0-twitter"
          val scalacOptions = stringListAttr(ct, "scalacopts")
          val javacOptions = stringListAttr(ct, "javacopts")
          val targetType = ct.getTarget.getRule.getRuleClass match {
            case "_scala_macro_library" => "scala_library"
            case "third_party_jvm_import" => "jar_library"
            case _ => "scala_library"
          }

          Seq(
            targetName -> new PantsTarget(
              name = targetName,
              id = id,
              dependencies = directDeps(ct).toSeq,
              javaSources = Nil,
              excludes = Set.empty,
              platform = Some(javaHome),
              runtimePlatform = Some(javaHome),
              libraries = transitiveLibraries(ct).toSeq,
              isPantsModulizable = directTargets.contains(targetName),
              targetType = TargetType("SOURCE"),
              pantsTargetType = PantsTargetType(targetType),
              globs = PantsGlobs(include = sources.toList, Nil),
              roots = PantsRoots(Nil),
              scalacOptions = scalacOptions.getOrElse(Nil).toList,
              javacOptions = javacOptions.getOrElse(Nil).toList,
              extraJvmOptions = Nil,
              directoryName = directoryName,
              baseDirectory = baseDirectory,
              classesDir = classesDir,
              internalSourcesJar = internalSourcesJar,
              strictDeps = false,
              isSynthetic = false,
              exports = Set.empty,
              scope = PantsScope("default"),
              targetBase = Some("tcdc/fastavro/src/main/scala"),
              mainClass = None,
              computedClasspath = classpath,
              bazelSourcesJar = sourceJar
            )
          )
        }
        .toMap
    }
  }

  def createExport(
      args: Export,
      cquery: AnalysisProtosV2.CqueryResult,
      directTargets: Set[String],
      buildIO: Map[String, BuildIO]
  ): PantsExport = {
    val targetMap = cquery.getResultsList.asScala.map { t =>
      t.getTarget.getRule.getName -> t
    }.toMap

    val internalSourcesDir =
      Files.createDirectories(args.bloopDir.resolve("sources-jar"))

    val impl =
      new Impl(args, targetMap, directTargets, buildIO, internalSourcesDir)
    val targets = impl.buildTargets(cquery)
    val libs = impl.collectLibraries()
    val scalaCompiler = impl.collectScalaCompiler()

    PantsExport(
      targets = targets,
      librariesJava = libs,
      internalSourcesDir = internalSourcesDir,
      scalaPlatform = scalaCompiler,
      jvmDistribution = PantsPreferredJvmDistribution(
        Some(Paths.get("/usr/lib/jvm/java-1.8.0-twitter"))
      )
    )
  }
}
