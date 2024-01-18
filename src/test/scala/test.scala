package name.rayrobdod.collectionRichVarargs

final class Suite extends munit.FunSuite:
	test ("all simple statements"):
		val result = RichVarargsList(1, 2, 3)
		val expect = List(1, 2, 3)
		assertEquals(result, expect)

	Seq(true, false).foreach: b =>
		test (s"with an `if ($b)`"):
			val result = RichVarargsList(1, if (b) 2, 3)
			val expect = if (b) List(1, 2, 3) else List(1, 3)
			assertEquals(result, expect)

	Seq(true, false).foreach: b =>
		test (s"Treats an `if/else` as a simple statement ($b)"):
			val result = RichVarargsList(if (b) 42 else 151)
			val expect = if (b) List(42) else List(151)
			assertEquals(result, expect)

	test ("rejects the statement `()`"):
		assertNoDiff(
			compileErrors("""RichVarargsList(1, (), 3 )"""),
			"""|error: Unknown tree
				|RichVarargsList(1, (), 3 )
				|                  ^
				|""".stripMargin
		)
