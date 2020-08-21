lazy val scalaVersions: Map[String, String] = Map("2.10" -> "2.10.6", "2.11" -> "2.11.12", "2.12" -> "2.12.1")

lazy val scalaTestVersion = "3.0.1"

val scalinVersion = "0.16.0.2-SNAPSHOT"
val spireVersion = "0.16.0"
val spireCycloVersion = "0.16.0.1-SNAPSHOT"

name := "cyclolab"

organization := "com.faacets"

scalaVersion := scalaVersions("2.11")

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-language:implicitConversions")

licenses := Seq("MIT" -> url("http://opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/denisrosset/gluon"))

bintrayRepository := "maven"

libraryDependencies ++= Seq(
  "net.alasc" %% "cyclo-core" % spireCycloVersion,
  "net.alasc" %% "scalin-core" % scalinVersion,
  "org.typelevel" %% "spire" % spireVersion,
  "com.lihaoyi" %% "fastparse" % "2.1.2",
)

enablePlugins(PackPlugin)
