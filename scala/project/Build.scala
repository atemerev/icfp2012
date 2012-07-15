import sbt._
import sbt.Project._
import Keys._

object BuildSettings {
  val buildOrganization = "org.pubmath"
  val buildVersion      = "1.0"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.8" % "test",
      "jline" % "jline" % "0.9.9"
    ),
    resolvers ++= Seq(
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"
    )
  )
}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
  )

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s %s %s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object ProjectBuild extends Build {

  import BuildSettings._
  import sbtassembly.Plugin._
  import AssemblyKeys._

  // (xb) cant make this compile
  // couldn't care less about sbt, so I just copy paste

  // private def mkGameTask(name: String, runner: String) =
  //   InputKey[Unit](name) <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
  //     (argTask, fullClasspath in Compile, runner) map { (args, classpath, runner) =>
  //       if (args.length != 1) {
  //         println("usage: " + name + " <file name in data>")
  //       } else {
  //         val filename = file("../data/" + args(0) + ".txt").absolutePath
  //         val logger = ConsoleLogger()
  //         Run.executeTrapExit({
  //           Run.run(runner,
  //                   classpath map (_.data),
  //                   Seq(filename),
  //                   logger)(runner)
  //         }, logger)
  //       }
  //     }
  //   }

  excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
    cp filter {_.data.getName == "junit-3.8.1.jar"}
  }

  lazy val project = Project(
    id = "icfp2012",
    base = file("."),
    settings = buildSettings ++ assemblySettings ++ Seq(
      InputKey[Unit]("game") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, fullClasspath in Compile, runner) map { (args, classpath, runner) =>
          if (args.length != 1) {
            println("usage: game <file name in data>")
          } else {
            val filename = file("../data/" + args(0) + ".txt").absolutePath
            val logger = ConsoleLogger()
            Run.executeTrapExit({
              Run.run("icfp.Main",
                      classpath map (_.data),
                      Seq("i", filename),
                      logger)(runner)
            }, logger)
          }
        }
      },
      InputKey[Unit]("gen1") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, fullClasspath in Compile, runner) map { (args, classpath, runner) =>
          if (args.length != 1) {
            println("usage: gen1 <file name in data>")
          } else {
            val filename = file("../data/" + args(0) + ".txt").absolutePath
            val logger = ConsoleLogger()
            Run.executeTrapExit({
              Run.run("icfp.Main",
                      classpath map (_.data),
                      Seq("gen1", filename),
                      logger)(runner)
            }, logger)
          }
        }
      },
      InputKey[Unit]("ast") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, fullClasspath in Compile, runner) map { (args, classpath, runner) =>
          if (args.length != 1) {
            println("usage: ast <file name in data>")
          } else {
            val filename = file("../data/" + args(0) + ".txt").absolutePath
            val logger = ConsoleLogger()
            Run.executeTrapExit({
              Run.run("icfp.Main",
                classpath map (_.data),
                Seq("a*", filename),
                logger)(runner)
            }, logger)
          }
        }
      },
      InputKey[Unit]("our-test") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, fullClasspath in Compile, runner) map { (args, classpath, runner) =>
          if (args.length != 0) {
            println("usage: our-test")
          } else {
            val logger = ConsoleLogger()
            Run.executeTrapExit({
              Run.run("icfp.Main",
                      classpath map (_.data),
                      Seq("t"),
                      logger)(runner)
            }, logger)
          }
        }
      }
    )
  )
}