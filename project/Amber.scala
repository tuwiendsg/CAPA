/*
 * Copyright 2012 Sanjin Sehic
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import Keys._

object Info {

  val version = "0.2-SNAPSHOT"

  val settings: Seq[Setting[_]] = Seq(
    Keys.version := version,
    organization := "at.ac.tuwien.infosys",
    organizationName := "Distributed Systems Group, " +
                        "Insitute of Information Systems 184/1, " +
                        "Vienna University of Technology",
    organizationHomepage := Some(url("http://www.infosys.tuwien.ac.at/")),
    licenses +=("Apache License Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    startYear := Some(2012)
  )
}

object Layout {
  val settings: Seq[Setting[_]] = Seq(
    sourceDirectory in Compile <<= baseDirectory(_ / "src"),
    sourceDirectory in Test <<= baseDirectory(_ / "test")
  )
}

object Build {
  val settings: Seq[Setting[_]] = Seq(
    scalaVersion := "2.9.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    javacOptions ++= Seq("-target", "6", "-source", "6", "-encoding", "utf8")
  )
}

// Shell prompt which show the current project,
// git branch and build version
object Shell {

  object devnull extends ProcessLogger {

    def info(s: => String) {}

    def error(s: => String) {}

    def buffer[T](f: => T): T = f
  }

  def branch() = (("git status -sb" lines_! devnull headOption) getOrElse "-" stripPrefix "## ")

  val settings: Seq[Setting[_]] = Seq(
    shellPrompt := {
      (state: State) => "%s:%s:%s> ".format(Project.extract(state).currentProject.id, branch(), Info.version)
    }
  )
}

object Dependency {

  object Akka {
    val version = "1.3.1"
    val repo = "Akka Repo" at "http://repo.typesafe.com/typesafe/releases/"
    val actor = "se.scalablesolutions.akka" % "akka-actor" % version
    val testkit = "se.scalablesolutions.akka" % "akka-testkit" % version
  }

  object Logback {
    val version = "1.0.6"
    val classic = "ch.qos.logback" % "logback-classic" % version
  }

  object Scalaz {
    val version = "6.0.4"
    val core = "org.scalaz" %% "scalaz-core" % version
  }

  object SLF4J {
    val version = "1.6.6"
    val api = "org.slf4j" % "slf4j-api" % version
  }

  object ScalaTest {
    val version = "1.8"
    val core = "org.scalatest" %% "scalatest" % version
  }

  object Mockito {
    val version = "1.9.0"
    val all = "org.mockito" % "mockito-all" % version
  }
}

object Amber extends Build {

  import Dependency._

  lazy val amber = Project(
    "amber",
    file("."),
    settings = defaultSettings
  ) aggregate(core, simple, akka, demo)

  lazy val core = module(
    name = "core",
    dependencies = Seq(
                     Scalaz.core,
                     SLF4J.api % "provided",
                     ScalaTest.core % "test",
                     Mockito.all % "test"
                   )
  )

  lazy val simple = module("simple") dependsOn(core % "test->test;compile")
  lazy val akka = module(
    name = "akka",
    dependencies = Seq(Akka.actor, Akka.testkit % "test"),
    resolvers = Seq(Akka.repo)
  ) dependsOn(core % "test->test;compile")
  lazy val demo = module(
    name = "demo",
    dependencies = Seq(Logback.classic % "runtime")
  ) dependsOn(core, simple, akka)

  val defaultSettings = Defaults.defaultSettings ++
                        Info.settings ++
                        Layout.settings ++
                        Build.settings ++
                        Shell.settings

  def module(name: String,
             dependencies: Seq[ModuleID] = Nil,
             resolvers: Seq[Resolver] = Nil) = Project(
    name,
    file(name),
    settings = defaultSettings ++
               Seq(
                 fork in test := true,
                 Keys.resolvers ++= resolvers,
                 libraryDependencies ++= dependencies
               )
  )
}
