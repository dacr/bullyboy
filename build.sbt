name := "bullyboy"

version := "0.0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation" , "-feature", "-language:implicitConversions")

mainClass in assembly := Some("bullyboy.Brute")

jarName in assembly := "bullyboy.jar"

libraryDependencies ++= Seq(
   "commons-codec" % "commons-codec" % "1.10",
   "de.tynne" % "saphir-hash-jca" % "3.0.1",
   "com.typesafe.akka" %% "akka-actor" % "2.4.1"  
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "junit" % "junit" % "4.12" % "test"

initialCommands in console := """
  import bullyboy._
"""

sourceGenerators in Compile <+= 
 (sourceManaged in Compile, version, name, jarName in assembly) map {
  (dir, version, projectname, jarexe) =>
  val file = dir / "bullyboy" / "MetaInfo.scala"
  IO.write(file,
  """package bullyboy
    |object MetaInfo { 
    |  val version="%s"
    |  val project="%s"
    |  val jarbasename="%s"
    |}
    |""".stripMargin.format(version, projectname, jarexe.split("[.]").head) )
  Seq(file)
}
