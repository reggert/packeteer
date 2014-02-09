name := "packeteer"

organization := "packeteer"

version := "0.0.1"

scalaVersion := "2.10.2"

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

libraryDependencies += "org.threeten" % "threetenbp" % "0.8.1" 

libraryDependencies += "com.typesafe.play" %% "play-iteratees" % "2.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"
