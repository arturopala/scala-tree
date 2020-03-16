ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.github.arturopala"
ThisBuild / organizationName := "Artur Opala"
ThisBuild / startYear := Some(2020)

lazy val supportedScalaVersions = List("2.13.1", "2.12.10", "2.11.12")

lazy val root = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin, GhpagesPlugin, SiteScaladocPlugin)
  .settings(
    name := "tree",
    licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.1" % Test
    ),
    crossScalaVersions := supportedScalaVersions,
    excludeFilter in (Compile, unmanagedResources) := NothingFilter,
    scalafmtOnCompile in Compile := true,
    scalafmtOnCompile in Test := true,
    releaseVersionBump := sbtrelease.Version.Bump.Minor,
    publishTo := sonatypePublishTo.value,
    scalacOptions in (Compile, doc) ++= Seq(
      "-groups"
    ),
    git.remoteRepo := "git@github.com:arturopala/scala-tree.git"
  )
