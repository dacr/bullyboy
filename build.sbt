name := "bullyboy"

version := "0.0.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-unchecked", "-deprecation" , "-feature", "-language:implicitConversions")

mainClass in assembly := Some("bullyboy.Brute")

assemblyJarName in assembly := "bullyboy.jar"

libraryDependencies ++= Seq(
   "commons-codec" % "commons-codec" % "1.10",
   "de.tynne" % "saphir-hash-jca" % "3.0.1",
   //"com.typesafe.akka" %% "akka-stream-experimental" % "2.0.2",
   "com.timgroup" %% "iterata" % "0.1.7",
   "io.github.andrebeat" %% "scala-pool" % "0.4.1"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"


initialCommands in console := """
  import bullyboy._
"""


sourceGenerators in Compile +=  Def.task {
  val dir = (sourceManaged in Compile).value
  val jarbasename = (assemblyJarName in assembly).value.split("[.]").head
  val file = dir / "dummy" / "ProjectMetaInfo.scala"
  val fiso = java.time.format.DateTimeFormatter.ISO_INSTANT
  val buildate = java.time.ZonedDateTime.now().format(fiso)
  IO.write(file,
    """package bullyboy
      |object ProjectMetaInfo {
      |  val name="%s"
      |  val version="%s"
      |  val buildate="%s"
      |  val jarbasename="%s"
      |  val appcode="bullyboy"
      |}
      |""".stripMargin.format(name.value, version.value, buildate, jarbasename) )
  Seq(file)
}.taskValue
