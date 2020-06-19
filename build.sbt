name := "ScalaProbabilisticProgramming"

version := "0.1"

scalaVersion := "2.13.2"




// PROJECTS

// global is the parent project, which aggregates all the other projects
lazy val global = project
	.in(file("."))
	.settings(
		name := "ScalaProbabilisticProgramming",
		settings,
		libraryDependencies ++=commonDependencies ++Seq(
			allDependencies.scalaReflect,
			allDependencies.scalaCheck,
			allDependencies.scalaTest,
			allDependencies.specs2Core,
			allDependencies.specs2ScalaCheck,
			allDependencies.discipline_core,
			allDependencies.discipline_scalatest,
			allDependencies.discipline_specs2,
			allDependencies.spire,
			allDependencies.spire_laws,
			allDependencies.algebra,
			allDependencies.algebra_laws,
			allDependencies.cats_core,
			allDependencies.cats_kernel,
			allDependencies.cats_laws,
			allDependencies.cats_free,
			allDependencies.cats_macros,
			allDependencies.cats_testkit,
			allDependencies.kindProjector
		)
	)
	.aggregate(
		//common,
		libraryOdds
		//multi2
		//TODO include other libraries here: bayes-scala... figaro ...
	)
     .dependsOn(
		libraryOdds
	)


lazy val libraryOdds = (project in file("lib/odds"))
	.settings(
		name := "odds",
		settings,
		//assemblySettings,
		libraryDependencies ++= /*commonDependencies ++*/ Seq(
			/*allDependencies.monocleCore,
			allDependencies.monocleMacro*/
		)
	)/*
	.dependsOn(

	)*/



// This is the common sub-project on which both delivarable "multi1" and "multi2" both depend.
// Contains code common to all projects.
/*lazy val common = project
	.settings(
		name := "common",
		settings,
		libraryDependencies ++= commonDependencies
	)*/

// Original usage: "deliverable" which can depend on "common" project
// Current usage: a dependency for the parent project. Using this template since there are dependencies other than
// common which the Odds library needs.






// DEPENDENCIES

lazy val allDependencies =
	new {

		// Listing the versions as values
		val versionOfScalaReflect = "2.13.2"
		val versionOfScalaTest = "3.3.0-SNAP2"
		val versionOfScalaCheck = "1.14.3"
		val versionOfSpecs2 = "4.9.4"
		val versionOfDiscipline_core = "1.0.2"
		val versionOfDiscipline_scalatest = "1.0.1"
		val versionOfDiscipline_specs2 = "1.1.0"
		val versionOfSpire = "0.17.0-M1"
		val versionOfAlgebra = "2.0.1"
		val versionOfCats = "2.2.0-M3"
		val versionOfCats_macros = "2.1.1"
		val versionOfKindProjector = "0.10.3"
		//------------------

		// Listing the different dependencies

		val scalaReflect = "org.scala-lang" % "scala-reflect" % versionOfScalaReflect

		val scalaTest = "org.scalatest" %% "scalatest" % versionOfScalaTest % Test

		val scalaCheck = "org.scalacheck" %% "scalacheck" % versionOfScalaCheck % Test

		val specs2Core = "org.specs2" %% "specs2-core" % versionOfSpecs2 % Test
		val specs2ScalaCheck = "org.specs2" %% "specs2-scalacheck" % versionOfSpecs2 % Test

		val discipline_core = "org.typelevel" %% "discipline-core" % versionOfDiscipline_core
		val discipline_scalatest = "org.typelevel" %% "discipline-scalatest" % versionOfDiscipline_scalatest% Test
		val discipline_specs2 = "org.typelevel" %% "discipline-specs2" % versionOfDiscipline_specs2 % Test

		val spire = "org.typelevel" %% "spire" % versionOfSpire
		val spire_laws = "org.typelevel" %% "spire-laws" % versionOfSpire % Test

		val algebra = "org.typelevel" %% "algebra" % versionOfAlgebra
		val algebra_laws = "org.typelevel" %% "algebra-laws" % versionOfAlgebra % Test

		val cats_core = "org.typelevel" %% "cats-core" % versionOfCats
		val cats_kernel = "org.typelevel" %% "cats-kernel" % versionOfCats
		val cats_laws = "org.typelevel" %% "cats-laws" % versionOfCats % Test
		val cats_free = "org.typelevel" %% "cats-free" % versionOfCats
		val cats_macros = "org.typelevel" %% "cats-macros" % versionOfCats_macros
		val cats_testkit = "org.typelevel" %% "cats-testkit" % versionOfCats % Test

		//Shapeless ...

		// Kind projector plugin
		// technicalities here = https://github.com/typelevel/kind-projector
		val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % versionOfKindProjector )
	}




lazy val commonDependencies = Seq(
	/*dependencies.logback,
	dependencies.logstash,
	dependencies.scalaLogging,
	dependencies.slf4j,
	dependencies.typesafeConfig,
	dependencies.akka,
	dependencies.scalatest  % "test",
	dependencies.scalacheck % "test"*/
)





// SETTINGS

lazy val settings =
	commonSettings /*++
		wartremoverSettings ++
		scalafmtSettings*/

lazy val compilerOptions = Seq(
	"-deprecation",
	"-unchecked",
	"-feature",
	"-language:existentials",
	"-language:higherKinds",
	"-language:implicitConversions",
	"-language:postfixOps",
	"-Ypartial-unification"
	//"-encoding",
	//"utf8"
)

lazy val commonSettings = Seq(
	scalacOptions ++= compilerOptions,
	resolvers ++= Seq(
		//"Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
		Resolver.sonatypeRepo("releases"),
		Resolver.sonatypeRepo("snapshots")
	)
)