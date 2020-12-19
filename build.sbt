val dottyVersion = "3.0.0-M3"

scalaVersion in ThisBuild := dottyVersion
version in ThisBuild := "0.1.0"

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(
      name := "quarto-core"
    )

lazy val root = project
  .in(file("."))
  .settings(
    name := "quarto"
  )
  .dependsOn(core.jvm)

lazy val js = project
  .in(file("js"))
  .settings(
    name := "quarto-js",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.0.0").withDottyCompat(dottyVersion)
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js)