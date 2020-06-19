name := "ScalaProbabilisticProgramming"

version := "0.1"

scalaVersion := "2.11.0" //"2.13.2"




// PROJECTS

//lazy val OddsLibrary = (project in file("lib/OddsLibrary"))


// global is the parent project, which aggregates all the other projects
lazy val global = project
	.in(file("."))
	.settings(
		name := "ScalaProbabilisticProgramming",
		settings,
		libraryDependencies ++= commonDependencies ++ Seq(
			allDependencies.scalaLibrary,
			allDependencies.scalaCompiler,
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
			allDependencies.kindProjector,

			allDependencies.breeze,
			allDependencies.breeze_natives,
			allDependencies.figaro,
			allDependencies.rainier_core,
			allDependencies.rainier_base,
			allDependencies.rainier_compute,
			allDependencies.rainier_plot,
			allDependencies.rainier_cats,
			allDependencies.rainier_sampler,
			allDependencies.rainier_scalacheck,
			allDependencies.evilplot,
			allDependencies.bayesScala
		)
	)
	.aggregate(
		//common,
		//OddsLibrary
		//multi2
		//TODO include other libraries here: bayes-scala... figaro ...
	)
     .dependsOn(
		//OddsLibrary
	)




// DEPENDENCIES

lazy val allDependencies =
	new {

		// Listing the versions as values
		val versionOfScala = "2.11.0" //"2.13.2"
		val versionOfScalaTest = "3.3.0-SNAP2"
		val versionOfScalaCheck = "1.14.3"
		val versionOfSpecs2 = "4.9.4"
		val versionOfDiscipline_core = "1.0.2"
		val versionOfDiscipline_scalatest = "1.0.1"
		val versionOfDiscipline_specs2 = "1.1.0"
		val versionOfSpire = "0.17.0-M1"
		val versionOfAlgebra =  "2.0.0" //"2.0.1"
		val versionOfCats = "2.0.0" // "2.2.0-M3"
		val versionOfCats_macros = "2.1.1"
		val versionOfKindProjector = "0.10.3"

		val versionOfFigaro = "5.0.0.0"
		//val versionOfRainier = "0.3.2"
		val versionOfRainier = "0.3.0"
		val versionOfEvilPlot = "0.6.0"
		val versionOfBayesScala = "0.6" // "0.7-SNAPSHOT"
		val versionOfBreeze = "1.0-RC4"

		//------------------

		// Listing the different dependencies
		val scalaLibrary = "org.scala-lang" % "scala-library" % versionOfScala
		val scalaCompiler = "org.scala-lang" % "scala-compiler" % versionOfScala
		val scalaReflect = "org.scala-lang" % "scala-reflect" % versionOfScala

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
		val cats_macros = "org.typelevel" %% "cats-macros" % versionOfCats //versionOfCats_macros
		val cats_testkit = "org.typelevel" %% "cats-testkit" % versionOfCats % Test

		//Shapeless ...

		// Kind projector plugin
		// technicalities here = https://github.com/typelevel/kind-projector
		val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % versionOfKindProjector )

		// Probabilistic programming libraries
		val figaro = "com.cra.figaro" %% "figaro" % versionOfFigaro
		//val rainier = "com.stripe" %% "rainier-core" % versionOfRainier
		val rainier_core = "com.stripe" %% "rainier-core" % versionOfRainier
		val rainier_base ="com.stripe" %% "rainier-base" % versionOfRainier
		val rainier_compute ="com.stripe" %% "rainier-compute" % versionOfRainier
		val rainier_plot = "com.stripe" %% "rainier-plot" % "0.2.3"
		val rainier_cats = "com.stripe" %% "rainier-cats" % "0.2.3"
		val rainier_sampler = "com.stripe" %% "rainier-sampler" % versionOfRainier
		val rainier_scalacheck = "com.stripe" %% "rainier-scalacheck" % "0.2.3"

		val evilplot = "com.cibo" %% "evilplot" % versionOfEvilPlot

		val bayesScala = "com.github.danielkorzekwa" %% "bayes-scala" % versionOfBayesScala

		val breeze = "org.scalanlp" %% "breeze" % versionOfBreeze
		val breeze_natives = "org.scalanlp" %% "breeze-natives" % versionOfBreeze
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
	"-language:postfixOps"
	//"-Ypartial-unification" //todo got error in sbt compilation " error: bad option" why?
	//"-encoding",
	//"utf8"
)

lazy val commonSettings = Seq(
	scalacOptions ++= compilerOptions,
	resolvers ++= Seq(
		//"Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
		Resolver.sonatypeRepo("releases"),
		Resolver.sonatypeRepo("snapshots"),
		// Resolver for Rainier library
		Resolver.bintrayRepo("rainier", "maven"),
		// Resolver for evilplot (dependency of Rainier)
		Resolver.bintrayRepo("cibotech", "public")
	)
)