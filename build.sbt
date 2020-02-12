val Http4sVersion = "0.21.0"
val DrosteVersion = "0.8.0"
val CirceVersion = "0.12.3"
val Specs2Version = "4.8.3"
val LogbackVersion = "1.2.3"
val MonocleVersion = "2.0.1"

lazy val root = (project in file("."))
  .settings(
    organization := "co.arbo",
    name := "arbo",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "io.higherkindness" %% "droste-core"       % DrosteVersion,
      "io.chrisdavenport" %% "cats-scalacheck"   % "0.2.0",
      "com.github.julien-truffaut" %% "monocle-core"  % MonocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % MonocleVersion,
      "com.github.julien-truffaut" %% "monocle-law"   % MonocleVersion % "test",
      "org.specs2"      %% "specs2-scalacheck"        % Specs2Version % "test",
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.full),
    scalacOptions += "-Ymacro-annotations"
  )
