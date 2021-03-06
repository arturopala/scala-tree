ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "com.github.arturopala"
ThisBuild / organizationName := "Artur Opala"
ThisBuild / startYear := Some(2020)

lazy val supportedScalaVersions = List("0.27.0-RC1", "2.13.3", "2.12.11", "2.11.12")

lazy val Benchmark = config("benchmark") extend Test

lazy val root = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin, GhpagesPlugin, SiteScaladocPlugin)
  .settings(
    name := "tree",
    licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    libraryDependencies ++= Seq(
      "com.github.arturopala" %% "buffer-and-slice" % "1.33.0",
      "org.scalameta"         %% "munit"            % "0.7.12" % Test
    ),
    libraryDependencies ++= dependencies(scalaVersion.value),
    crossScalaVersions := supportedScalaVersions,
    excludeFilter in (Compile, unmanagedResources) := NothingFilter,
    scalafmtOnCompile in Compile := true,
    scalafmtOnCompile in Test := true,
    releaseVersionBump := sbtrelease.Version.Bump.Minor,
    publishTo := sonatypePublishTo.value,
    scalacOptions in (Compile, doc) ++= Seq(
      "-groups"
    ),
    git.remoteRepo := "git@github.com:arturopala/scala-tree.git",
    testFrameworks += new TestFramework("munit.Framework"),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false,
    parallelExecution in Test := false,
    parallelExecution in Benchmark := false
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings): _*
  )

def dependencies(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) =>
      List(
        "com.storm-enroute" %% "scalameter" % "0.19" % Test
      )
    case _ => List()
  }
