scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.12"

initialCommands in console := """
import fpinscala.par
import fpinscala.par.Time._
"""
