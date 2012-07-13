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
    ),
    mainClass := Some("icfp.Interpreter")
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

  lazy val project = Project(
    id = "icfp2012",
    base = file("."),
    settings = buildSettings
  )
}