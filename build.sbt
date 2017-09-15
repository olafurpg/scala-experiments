inThisBuild(
  List(
    organization := "com.geirsson",
    scalaVersion := "2.12.3"
  )
)

lazy val benchmarks = project
  .enablePlugins(JmhPlugin)
  .dependsOn(experiments)

lazy val experiments = project
  .settings(
    libraryDependencies += "org.scalameta" %% "testkit" % "2.0.0-M3",
    buildInfoPackage := "experiment",
    buildInfoKeys := Seq[BuildInfoKey](
      "cwd" -> baseDirectory.in(ThisBuild).value
    )
  )
  .enablePlugins(BuildInfoPlugin)
