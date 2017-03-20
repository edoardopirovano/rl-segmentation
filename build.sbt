lazy val commonSettings = Seq(
  organization := "org.edoardo",
  version := "1.0.0",
  scalaVersion := "2.12.1"
)

resolvers +=
    "ImageJ Releases" at "http://maven.imagej.net/content/repositories/releases/"

lazy val root: Project = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "RL Segmentation",
    scalaSource in Compile := baseDirectory.value / "src",
    maxErrors := 20,
    pollInterval := 1000,
    scalacOptions += "-deprecation",
    fork := true,
    javaOptions += "-Xmx4G",
    libraryDependencies +=
      "log4j" % "log4j" % "1.2.15" excludeAll(
        ExclusionRule(organization = "com.sun.jdmk"),
        ExclusionRule(organization = "com.sun.jmx"),
        ExclusionRule(organization = "javax.jms")
      ),
      libraryDependencies += "net.imglib2" % "imglib2" % "3.2.1",
      libraryDependencies += "net.imglib2" % "imglib2-algorithm" % "0.6.2",
      libraryDependencies += "net.imglib2" % "imglib2-ij" % "2.0.0-beta-35",
      libraryDependencies += "fr.inra.ijpb" % "MorphoLibJ_" % "1.3.2",
      libraryDependencies += "com.google.guava" % "guava" % "21.0"
  )
