val scala3Version = "3.0.2"

lazy val plugin = project.settings(
    name := "Fremo-Plugin",
    organization := "me.frde",
    version := "0.1.0",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scala3Version % "provided"
)

lazy val runtime = project
  .settings(
      name := "Fremo-Plugin-runtime",
      organization := "me.frde",
      version := "0.1.0",
      scalaVersion := scala3Version
  )

lazy val fremo = project
  .in(file("."))
  .settings(
    name := "Fremo",
    version := "0.1.0",

    scalaVersion := scala3Version,

    //scalacOptions += "-P:fremo:Fremo.yml",

    assembly / mainClass := Some("me.frde.fremo.NetTest"),
    assembly / assemblyJarName := "Fremo.jar",

    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    },

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.11.1",
    libraryDependencies += "com.azure" % "azure-cosmos" % "4.4.0",
    libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.3",
//    libraryDependencies += "me.frde" %% "fremo-plugin-runtime" % "0.1.0",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scala3Version,
//    libraryDependencies += compilerPlugin("me.frde" %% "fremo-plugin" % "0.1.0")

  )

lazy val root = project.aggregate(plugin, runtime)
