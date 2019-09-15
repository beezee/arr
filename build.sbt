lazy val root = (project in file(".")).
  settings(
    organization := "bz",
    scalaVersion := "2.12.5",
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-language:higherKinds",
      "-Xfatal-warnings",
      "-Xlint:-unused,_",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:privates,locals",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ybackend-parallelism", java.lang.Runtime.getRuntime.availableProcessors.toString,
      "-Ycache-plugin-class-loader:last-modified",
      //"-Xlog-implicits",
      "-Ycache-macro-class-loader:last-modified"),
    name := "arr"
  )
