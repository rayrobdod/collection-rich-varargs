val scala3Ver = "3.3.1"

lazy val base = (project in file("."))
	.settings(
		scalaVersion := scala3Ver,
		libraryDependencies ++= Seq(
			"org.scalameta" %% "munit" % "0.7.29" % Test,
		),
	)
