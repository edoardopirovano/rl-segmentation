// factor out common settings into a sequence
lazy val commonSettings = Seq(
  organization := "org.edoardo",
  version := "1.0.0",
  // set the Scala version used for the project
  scalaVersion := "2.12.0"
)

resolvers +=
    "ImageJ Releases" at "http://maven.imagej.net/content/repositories/releases/"

resolvers +=
    "Boundless" at "http://repo.boundlessgeo.com/main/"

resolvers +=
    "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/"

lazy val root: Project = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    // set the name of the project
    name := "RL Segmentation",

    // set the main Scala source directory to be <base>/src
    scalaSource in Compile := baseDirectory.value / "src",

    // reduce the maximum number of errors shown by the Scala compiler
    maxErrors := 20,

    // increase the time between polling for file changes when using continuous execution
    pollInterval := 1000,

    // append -deprecation to the options passed to the Scala compiler
    scalacOptions += "-deprecation",
    // fork a new JVM for 'run' and 'test:run'
    fork := true,

    // add a JVM option to use when forking a JVM for 'run'
    javaOptions += "-Xmx4G",

    // Exclude transitive dependencies, e.g., include log4j without including logging via jdmk, jmx, or jms.
    libraryDependencies +=
      "log4j" % "log4j" % "1.2.15" excludeAll(
        ExclusionRule(organization = "com.sun.jdmk"),
        ExclusionRule(organization = "com.sun.jmx"),
        ExclusionRule(organization = "javax.jms")
      ),
      
      // https://mvnrepository.com/artifact/net.imglib2/imglib2
      libraryDependencies += "net.imglib2" % "imglib2" % "3.2.1",
    
      // https://mvnrepository.com/artifact/net.imglib2/imglib2-algorithm
      libraryDependencies += "net.imglib2" % "imglib2-algorithm" % "0.6.2",
    
      // https://mvnrepository.com/artifact/net.imglib2/imglib2-ij
      libraryDependencies += "net.imglib2" % "imglib2-ij" % "2.0.0-beta-35",
    
      // https://mvnrepository.com/artifact/io.scif/scifio
      libraryDependencies += "io.scif" % "scifio" % "0.30.0",
    
      // https://mvnrepository.com/artifact/net.sf.ij-plugins/ijp-toolkit_2.12
      libraryDependencies += "net.sf.ij-plugins" % "ijp-toolkit_2.12" % "2.1.0",
    
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

)
