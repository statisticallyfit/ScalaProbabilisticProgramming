name := "ScalaProbabilisticProgramming"

version := "0.1"

scalaVersion := "2.13.2"


// ----------------------------------------------------------------------------------

//For the Kind projector plugin
resolvers += Resolver.sonatypeRepo("releases")
//addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

// ----------------------------------------------------------------------------------



libraryDependencies ++= Seq(
	//"org.scala-lang" % "scala-library" % "2.12.0", //scala language
	//"org.scala-lang" % "scala-reflect" % "2.12.0", // scala compiler
	//Dependencies of Numsca:
	//"org.nd4j" % "nd4j-native-platform" % "0.9.1",
	//"com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
	//"ch.qos.logback" % "logback-classic" % "1.2.3",
	/*

		// Odds dependencies (were unresolved before)
		"org.scala-lang.virtualized" % "scala-library" % "2.10.2-RC1",
		"org.scala-lang.virtualized" % "scala-compiler" % "2.10.2-RC1",
		"org.scala-lang.virtualized" % "scala-reflect" % "2.10.2-RC1",
	*/

	//Scala Reflections
	"org.scala-lang" % "scala-reflect" % "2.13.2",

	//ScalaTest
	"org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,

	//ScalaCheck
	"org.scalacheck" %% "scalacheck" % "1.14.3" % Test,

	//Specs2
	"org.specs2" %% "specs2-core" % "4.9.4" % Test,
	"org.specs2" %% "specs2-scalacheck" % "4.9.4" % Test,

	//Discipline
	"org.typelevel" %% "discipline-core" % "1.0.2",
	"org.typelevel" %% "discipline-scalatest" % "1.0.1" % Test,
	"org.typelevel" %% "discipline-specs2" % "1.1.0" % Test,

	// Spire
	"org.typelevel" %% "spire" % "0.17.0-M1",
	"org.typelevel" %% "spire-laws" % "0.17.0-M1" % Test,

	// Algebra
	"org.typelevel" %% "algebra" % "2.0.1",
	"org.typelevel" %% "algebra-laws" % "2.0.1" % Test,

	// Cats
	"org.typelevel" %% "cats-core" % "2.2.0-M3",
	"org.typelevel" %% "cats-kernel" % "2.2.0-M3",
	"org.typelevel" %% "cats-laws" % "2.2.0-M3" % Test,
	"org.typelevel" %% "cats-free" % "2.2.0-M3",
	"org.typelevel" %% "cats-macros" % "2.1.1",
	"org.typelevel" %% "cats-testkit" % "2.2.0-M3" % Test,

	//Shapeless ...

	// Kind projector plugin
	// technicalities here = https://github.com/typelevel/kind-projector
	compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
)
