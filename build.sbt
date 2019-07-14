name := "spell-labyrinth"

version := "1.1-SNAPSHOT"

scalaVersion := "2.12.8"

// https://mvnrepository.com/artifact/org.jgrapht/jgrapht-core
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.0.1"

// https://mvnrepository.com/artifact/com.github.scopt/scopt_2.12
libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"

assemblyJarName in assembly := s"${name.value}-${version.value}"