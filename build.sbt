name := "typeclass-inject"

version := "0.0.2"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0-SNAPSHOT"

resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

scalacOptions += "-feature"

scalacOptions += "-language:higherKinds"

scalacOptions += "-Xlint"

scalacOptions += "-Xfatal-warnings"

scalacOptions += "-Yno-adapted-args"

scalacOptions += "-Ywarn-all"
