// factor out common settings into a sequence
lazy val commonSettings = Seq(
  organization := "org.edoardo",
  version := "1.0.0",
  // set the Scala version used for the project
  scalaVersion := "2.10.6"
)

// define ModuleID for library dependencies
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.0"

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  dependsOn(RootProject(uri("https://github.com/pvorb/scala-pnm.git"))).
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
      )

  )

EclipseKeys.withSource := true
EclipseKeys.withJavadoc := true
