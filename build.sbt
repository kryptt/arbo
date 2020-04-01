val CacheVersion = "0.28.0"
val CatsEffectsVersion = "2.1.2"
val CatsVersion = "2.1.1"
val CirceVersion = "0.13.0"
val CirisVersion = "1.0.4"
val DisciplineVersion = "1.1.0"
val DrosteVersion = "0.8.0"
val Http4sVersion = "0.21.2"
val KindProjectorVersion = "0.11.0"
val LogbackVersion = "1.2.3"
val MonocleVersion = "2.0.4"
val ScodecCoreVersion = "1.11.7"
val Specs2Version = "4.9.2"

enablePlugins(JavaAppPackaging, DockerPlugin)

lazy val root = (project in file("."))
  .settings(
    organization := "co.arbo",
    name := "arbo",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "is.cir" %% "ciris" % CirisVersion,
      "org.scodec" %% "scodec-core" % ScodecCoreVersion,
      "org.typelevel" %% "cats-effect" % CatsEffectsVersion,
      "com.github.cb372" %% "scalacache-caffeine" % CacheVersion,
      "com.github.cb372" %% "scalacache-cats-effect" % CacheVersion,
      "io.higherkindness" %% "droste-core" % DrosteVersion,
      "io.chrisdavenport" %% "cats-scalacheck" % "0.2.0",
      "com.github.julien-truffaut" %% "monocle-core" % MonocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % MonocleVersion,
      "com.github.julien-truffaut" %% "monocle-law" % MonocleVersion % Test,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "discipline-specs2" % DisciplineVersion % Test,
      "org.specs2" %% "specs2-scalacheck" % Specs2Version % Test,
      "ch.qos.logback" %  "logback-classic" % LogbackVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % KindProjectorVersion cross CrossVersion.full),
    scalacOptions += "-Ymacro-annotations"
  )
