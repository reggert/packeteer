name := "pakka"

organization := "pakka"

version := "0.0.1"

scalaVersion := "2.10.2"

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.1"

libraryDependencies += "org.threeten" % "threetenbp" % "0.8.1" 

libraryDependencies += "com.typesafe.play" %% "play-iteratees" % "2.2.0"

resolvers += "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.2-SNAPSHOT"


