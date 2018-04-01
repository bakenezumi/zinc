package xsbt

import sbt.internal.inc.UnitSpec
import xsbti.UseScope

class ExtractUsedNamesSpecification extends UnitSpec {

  "Used names extraction" should "extract imported name" in {
    val src = """package a { class A }
                |package b {
                | import a.{A => A2}
                |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, _, _) = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("a", "A", "A2", "b")
    // names used at top level are attributed to the first class defined in a compilation unit

    assert(usedNames("a.A") === expectedNames)
  }

  // test covers https://github.com/gkossakowski/sbt/issues/6
  it should "extract names in type tree" in {
    val srcA = """|package a {
                  |  class A {
                  |    class C { class D }
                  |  }
                  |  class B[T]
                  |}
                  |package c {
                  |  class BB
                  |}
                  |
                  |""".stripMargin
    val srcB = """|package b {
                  | abstract class X {
                  |     def foo: a.A#C#D
                  |     def bar: a.B[c.BB]
                  |   }
                  |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("a", "c", "A", "B", "C", "D", "b", "X", "BB")
    assert(usedNames("b.X") === expectedNames)
    val expectedUsedNamePositions = Set((0, 8, "b"),
                                        (2, 14, "a"),
                                        (2, 16, "a.A"),
                                        (2, 18, "a.A.C"),
                                        (2, 20, "a.A.C.D"),
                                        (3, 14, "a"),
                                        (3, 16, "a.B"),
                                        (3, 18, "c"),
                                        (3, 20, "c.BB"))
    assert(expectedUsedNamePositions.forall(usedNamePositions("b.X").contains))
    val expectedDefinedNamePositions = Set((1, 16, "b.X"), (2, 9, "b.X.foo"), (3, 9, "b.X.bar"))
    assert(expectedDefinedNamePositions.forall(definedNamePositions("b.X").contains))
  }

  // test for https://github.com/gkossakowski/sbt/issues/5
  it should "extract symbolic names" in {
    val srcA = """|class A {
                  |  def `=`: Int = 3
                  |}""".stripMargin
    val srcB = """|class B {
                  |  def foo(a: A) = a.`=`
                  |}""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("A", "a", "B", "=", "Int")
    assert(usedNames("B") === expectedNames)
    val expectedUsedNamePositions =
      Set((1, 13, "A"), (1, 18, "B.a"), (1, 20, "A.$eq"))
    assert(expectedUsedNamePositions.forall(usedNamePositions("B").contains))
    val expectedDefinedNamePositions =
      Set((0, 6, "B"), (1, 6, "B.foo"), (1, 10, "B.a"))
    assert(expectedDefinedNamePositions.forall(definedNamePositions("B").contains))
  }

  it should "extract type names for objects depending on abstract types" in {
    val srcA =
      """abstract class A {
        | type T
        | object X {
        |    def foo(x: T): T = x
        |  }
        |}
      """.stripMargin
    val srcB = "class B extends A { type T = Int }"
    val srcC = "object C extends B"
    val srcD = "object D { C.X.foo(12) }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(srcA, srcB, srcC, srcD)
    val scalaVersion = scala.util.Properties.versionNumberString
    // TODO: Find out what's making these types appear in 2.10
    // They don't come from type dependency traverser, but from `addSymbol`
    val versionDependentNames =
      if (scalaVersion.contains("2.10")) Set("Nothing", "Any") else Set()
    val namesA = standardNames ++ Set("A") ++ versionDependentNames
    val namesAX = standardNames ++ Set("X", "x", "T", "A")
    val namesB = Set("B", "A", "Int", "A;init;", "scala")
    val namesC = Set("B;init;", "C", "B")
    val namesD = standardNames ++ Set("D", "C", "X", "foo", "Int", "T")
    assert(usedNames("A") === namesA)
    assert(usedNames("A.X") === namesAX)
    assert(usedNames("B") === namesB)
    assert(usedNames("C") === namesC)
    assert(usedNames("D") === namesD)

    val expectedUsedNamePositionsAX =
      Set((3, 15, "A.T"), (3, 19, "A.T"), (3, 23, "A.X.x"))
    val expectedUsedNamePositionsB = Set((0, 16, "A"), (0, 29, "scala.Int"))
    val expectedUsedNamePositionsC = Set((0, 17, "B"))
    val expectedUsedNamePositionsD =
      Set((0, 11, "C"), (0, 13, "A.X"), (0, 15, "A.X.foo"))
    assert(expectedUsedNamePositionsAX.forall(usedNamePositions("A.X").contains))
    assert(expectedUsedNamePositionsB.forall(usedNamePositions("B").contains))
    assert(expectedUsedNamePositionsC.forall(usedNamePositions("C").contains))
    assert(expectedUsedNamePositionsD.forall(usedNamePositions("D").contains))

    val expectedDefinedNamePositionsA = Set((0, 15, "A"), (1, 6, "A.T"), (2, 8, "A.X"))
    val expectedDefinedNamePositionsAX = Set((3, 8, "A.X.foo"), (3, 12, "A.X.x"))
    val expectedDefinedNamePositionsB = Set((0, 6, "B"), (0, 25, "B.T"))
    val expectedDefinedNamePositionsC = Set((0, 7, "C"))
    val expectedDefinedNamePositionsD = Set((0, 7, "D"))
    assert(expectedDefinedNamePositionsA.forall(definedNamePositions("A").contains))
    assert(expectedDefinedNamePositionsAX.forall(definedNamePositions("A.X").contains))
    assert(expectedDefinedNamePositionsB.forall(definedNamePositions("B").contains))
    assert(expectedDefinedNamePositionsC.forall(definedNamePositions("C").contains))
    assert(expectedDefinedNamePositionsD.forall(definedNamePositions("D").contains))

  }

  // See source-dependencies/types-in-used-names-a for an example where
  // this is required.
  it should "extract names in the types of trees" in {
    val src1 = """|class X0
                  |class X1 extends X0
                  |class Y
                  |class A {
                  |  type T >: X1 <: X0
                  |}
                  |class M
                  |class N
                  |class P0
                  |class P1 extends P0
                  |object B {
                  |  type S = Y
                  |  val lista: List[A] = ???
                  |  val at: A#T = ???
                  |  val as: S = ???
                  |  def foo(m: M): N = ???
                  |  def bar[Param >: P1 <: P0](p: Param): Param = ???
                  |}""".stripMargin
    val src2 = """|object Test_lista {
                  |  val x = B.lista
                  |}
                  |object Test_at {
                  |  val x = B.at
                  |}
                  |object Test_as {
                  |  val x = B.as
                  |}
                  |object Test_foo {
                  |  val x = B.foo(???)
                  |}
                  |object Test_bar {
                  |  val x = B.bar(???)
                  |}
                  |""".stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(src1, src2)
    val expectedNames_lista = standardNames ++ Set("Test_lista", "x", "B", "lista", "List", "A")
    val expectedNames_at = standardNames ++ Set("Test_at", "x", "B", "at", "A", "T", "X0", "X1")
    val expectedNames_as = standardNames ++ Set("Test_as", "x", "B", "as", "S", "Y")
    val expectedNames_foo = standardNames ++ Set("Test_foo",
                                                 "x",
                                                 "B",
                                                 "foo",
                                                 "M",
                                                 "N",
                                                 "Predef",
                                                 "???",
                                                 "Nothing")
    val expectedNames_bar = standardNames ++ Set("Test_bar",
                                                 "x",
                                                 "B",
                                                 "bar",
                                                 "Param",
                                                 "P1",
                                                 "P0",
                                                 "Predef",
                                                 "???",
                                                 "Nothing")
    assert(usedNames("Test_lista") === expectedNames_lista)
    assert(usedNames("Test_at") === expectedNames_at)
    assert(usedNames("Test_as") === expectedNames_as)
    assert(usedNames("Test_foo") === expectedNames_foo)
    assert(usedNames("Test_bar") === expectedNames_bar)

    val expectedUsedNamePositions_lista =
      Set((1, 10, "B"), (1, 12, "B.lista"))
    val expectedUsedNamePositions_at =
      Set((4, 10, "B"), (4, 12, "B.at"))
    val expectedUsedNamePositions_as =
      Set((7, 10, "B"), (7, 12, "B.as"))
    val expectedUsedNamePositions_foo =
      Set((10, 10, "B"), (10, 12, "B.foo"), (10, 16, "scala.Predef.$qmark$qmark$qmark"))
    val expectedUsedNamePositions_bar =
      Set((13, 10, "B"), (13, 12, "B.bar"), (13, 16, "scala.Predef.$qmark$qmark$qmark"))
    assert(expectedUsedNamePositions_lista.forall(usedNamePositions("Test_lista").contains))
    assert(expectedUsedNamePositions_at.forall(usedNamePositions("Test_at").contains))
    assert(expectedUsedNamePositions_as.forall(usedNamePositions("Test_as").contains))
    assert(expectedUsedNamePositions_foo.forall(usedNamePositions("Test_foo").contains))
    assert(expectedUsedNamePositions_bar.forall(usedNamePositions("Test_bar").contains))

    val expectedDefinedNamePositions_lista = Set((0, 7, "Test_lista"),
                                                 (1, 6, "Test_lista.x"),
                                                 (3, 7, "Test_at"),
                                                 (6, 7, "Test_as"),
                                                 (9, 7, "Test_foo"),
                                                 (12, 7, "Test_bar"))
    val expectedDefinedNamePositions_at = Set((4, 6, "Test_at.x"))
    val expectedDefinedNamePositions_as = Set((7, 6, "Test_as.x"))
    val expectedDefinedNamePositions_foo = Set((10, 6, "Test_foo.x"))
    val expectedDefinedNamePositions_bar = Set((13, 6, "Test_bar.x"))
    val expectedDefinedNamePositions_B = Set(
      (11, 7, "B.S"),
      (12, 6, "B.lista"),
      (13, 6, "B.at"),
      (14, 6, "B.as"),
      (15, 6, "B.foo"),
      (15, 10, "B.m"),
      (16, 6, "B.bar"),
      (16, 10, "B.Param"),
      (16, 29, "B.p")
    )
    assert(expectedDefinedNamePositions_lista.forall(definedNamePositions("Test_lista").contains))
    assert(expectedDefinedNamePositions_at.forall(definedNamePositions("Test_at").contains))
    assert(expectedDefinedNamePositions_as.forall(definedNamePositions("Test_as").contains))
    assert(expectedDefinedNamePositions_foo.forall(definedNamePositions("Test_foo").contains))
    assert(expectedDefinedNamePositions_bar.forall(definedNamePositions("Test_bar").contains))
    assert(expectedDefinedNamePositions_B.forall(definedNamePositions("B").contains))

  }

  it should "extract used names from an existential" in {
    val srcFoo =
      """import scala.language.existentials
      |class Foo {
      |  val foo: T forSome { type T <: Double } = ???
      |}
      """.stripMargin
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(srcFoo)
    val expectedNames = standardNames ++ Seq("Double",
                                             "Foo",
                                             "T",
                                             "foo",
                                             "scala",
                                             "language",
                                             "existentials",
                                             "Nothing",
                                             "???",
                                             "Predef")
    assert(usedNames("Foo") === expectedNames)

    val expectedUsedNamePositions =
      Set((0, 7, "scala"),
          (0, 13, "scala.language"),
          (2, 11, "Foo.T"),
          (2, 44, "scala.Predef.$qmark$qmark$qmark"))
    assert(expectedUsedNamePositions.forall(usedNamePositions("Foo").contains))

    val expectedDefinedNamePositions =
      Set((1, 6, "Foo"), (2, 6, "Foo.foo"), (2, 28, "Foo.T"))
    assert(expectedDefinedNamePositions.forall(definedNamePositions("Foo").contains))
  }

  it should "extract used names from a refinement" in {
    val srcFoo =
      "object Outer {\n  class Inner { type Xyz }\n\n  type TypeInner = Inner { type Xyz = Int }\n}"
    val srcBar = "object Bar {\n  def bar: Outer.TypeInner = null\n}"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(srcFoo, srcBar)
    val expectedNames = standardNames ++ Set("Bar", "Outer", "TypeInner", "Inner", "Xyz", "Int")
    assert(usedNames("Bar") === expectedNames)

    val expectedUsedNamePositions =
      Set((1, 11, "Outer"), (1, 17, "Outer.TypeInner"))
    assert(expectedUsedNamePositions.forall(usedNamePositions("Bar").contains))

    val expectedDefinedNamePositions = Set((0, 7, "Bar"), (1, 6, "Bar.bar"))
    assert(expectedDefinedNamePositions.forall(definedNamePositions("Bar").contains))
  }

  // test for https://github.com/gkossakowski/sbt/issues/3
  it should "extract used names from the same compilation unit" in {
    val src = "class A { def foo: Int = 0; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, usedNamePositions, definedNamePositions) =
      compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assert(usedNames("A") === expectedNames)
    val expectedUsedNamePositions =
      Set((0, 19, "scala.Int"), (0, 37, "scala.Int"), (0, 43, "A.foo"))
    assert(expectedUsedNamePositions.forall(usedNamePositions("A").contains))
    val expectedDefinedNamePositions =
      Set((0, 6, "A"), (0, 14, "A.foo"), (0, 32, "A.bar"))
    assert(expectedDefinedNamePositions.forall(definedNamePositions("A").contains))
  }

  // pending test for https://issues.scala-lang.org/browse/SI-7173
  it should "extract names of constants" in pendingUntilFixed {
    val src = "class A { final val foo = 12; def bar: Int = foo }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, _, _) = compilerForTesting.extractUsedNamesFromSrc(src)
    val expectedNames = standardNames ++ Set("A", "foo", "Int")
    assert(usedNames === expectedNames)
    ()
  }

  // test for https://github.com/gkossakowski/sbt/issues/4
  // TODO: we should fix it by having special treatment of `selectDynamic` and `applyDynamic` calls
  it should "extract names from method calls on Dynamic" in pendingUntilFixed {
    val srcA = """|import scala.language.dynamics
                  |class A extends Dynamic {
                  | def selectDynamic(name: String): Int = name.length
                  |}""".stripMargin
    val srcB = "class B { def foo(a: A): Int = a.bla }"
    val compilerForTesting = new ScalaCompilerForUnitTesting
    val (usedNames, _, _) = compilerForTesting.extractUsedNamesFromSrc(srcA, srcB)
    val expectedNames = standardNames ++ Set("B", "A", "a", "Int", "selectDynamic", "bla")
    assert(usedNames === expectedNames)
    ()
  }

  it should "extract sealed classes scope" in {
    val sealedClassName = "Sealed"
    val sealedClass =
      s"""package base
        |
        |sealed class $sealedClassName
        |object Usage extends $sealedClassName
        |object Usage2 extends $sealedClassName
      """.stripMargin

    def findPatMatUsages(in: String): Set[String] = {
      val compilerForTesting = new ScalaCompilerForUnitTesting
      val (_, callback) =
        compilerForTesting.compileSrcs(List(List(sealedClass, in)), reuseCompilerInstance = false)
      val clientNames = callback.usedNamesAndScopes.filterKeys(!_.startsWith("base."))

      val names: Set[String] = clientNames.flatMap {
        case (_, usages) =>
          usages.filter(_.scopes.contains(UseScope.PatMatTarget)).map(_.name)
      }(collection.breakOut)

      names
    }

    def classWithPatMatOfType(tpe: String = sealedClassName) =
      s"""package client
        |import base._
        |
        |class test(a: $tpe) {
        |  a match {
        |   case _ => 1
        |  }
        |}
      """.stripMargin

    findPatMatUsages(classWithPatMatOfType()) shouldEqual Set(sealedClassName)
    // Option is sealed
    findPatMatUsages(classWithPatMatOfType(s"Option[$sealedClassName]")) shouldEqual Set(
      sealedClassName,
      "Option")
    // Seq and Set is not
    findPatMatUsages(classWithPatMatOfType(s"Seq[Set[$sealedClassName]]")) shouldEqual Set(
      sealedClassName)

    def inNestedCase(tpe: String) =
      s"""package client
          |import base._
          |
          |class test(a: Any) {
          |  a match {
          |   case _: $tpe => 1
          |  }
          |}""".stripMargin

    findPatMatUsages(inNestedCase(sealedClassName)) shouldEqual Set()

    val notUsedInPatternMatch =
      s"""package client
          |import base._
          |
          |class test(a: Any) {
          |  a match {
          |   case _ => 1
          |  }
          |  val aa: $sealedClassName = ???
          |}""".stripMargin

    findPatMatUsages(notUsedInPatternMatch) shouldEqual Set()
  }

  /**
   * Standard names that appear in every compilation unit that has any class
   * definition.
   */
  private val standardNames = Set(
    "scala",
    // The default parent of a class is "AnyRef" which is an alias for "Object"
    "AnyRef",
    "Object",
    "java;lang;Object;init;"
  )

}
