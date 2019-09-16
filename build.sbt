val dottyVersion = "0.18.1-RC1"

lazy val commonScalacOptions = Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-deprecation",
  "-unchecked",
  "-language:strict",
)
lazy val exercises = project
  .in(file("exercises"))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions := commonScalacOptions,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

lazy val answers = project
  .in(file("answers"))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions := commonScalacOptions,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

