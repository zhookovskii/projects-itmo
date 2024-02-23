import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import java.text.ParseException
import kotlin.math.exp

class FunctionHeadersTest {

    companion object {

        @JvmStatic
        @BeforeAll
        fun beforeAll() {
            GrannyGenerator("./src/main/kotlin-headers.txt").generate()
        }
    }

    private val parser: Parser = Parser()

    private fun testCase(input: String, expected: Tree) {
        val inS = input.byteInputStream()
        val tree = parser.parse(inS)
        inS.close()
        tree.toSvg("src/main/tree.txt")
    }

    private fun assertFailure(block: () -> Unit) {
        Assertions.assertThrows(ParseException::class.java, block)
    }

    @Test
    fun parsesTrivialSignature() {
        testCase(
            input = "fun main()",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("main"),
                Tree("("),
                Tree("A"),
                Tree(")"),
                Tree("R")
            )
        )
    }

    @Test
    fun parsesNoArguments() {
        testCase(
            input = "fun main(): Unit",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("main"),
                Tree("("),
                Tree("A"),
                Tree(")"),
                Tree(
                    "R", Tree(":"), Tree("T", Tree("Unit"), Tree("T'"))
                )
            )
        )
    }

    @Test
    fun parsesNoReturnType() {
        testCase(
            input = "fun run(x: Int)",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("run"),
                Tree("("),
                Tree(
                    "A", Tree("x"), Tree(":"), Tree("T", Tree("Int"), Tree("T'")), Tree("D"), Tree("A'")
                ),
                Tree(")"),
                Tree("R")
            )
        )
    }

    @Test
    fun parsesEverything() {
        testCase(
            input = "fun distance(x1: Double, y1: Double, x2: Double, y2: Double): Double",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("distance"),
                Tree("("),
                Tree(
                    "A", Tree("x1"), Tree(":"), Tree("T", Tree("Double"), Tree("T'")), Tree("D"),
                    Tree(
                        "A'", Tree(","),
                        Tree(
                            "A", Tree("y1"), Tree(":"), Tree("T", Tree("Double"), Tree("T'")), Tree("D"),
                            Tree(
                                "A'", Tree(","),
                                Tree(
                                    "A", Tree("x2"), Tree(":"), Tree("T", Tree("Double"), Tree("T'")), Tree("D"),
                                    Tree(
                                        "A'", Tree(","),
                                        Tree(
                                            "A", Tree("y2"), Tree(":"), Tree("T", Tree("Double"), Tree("T'")), Tree("D"),

                                            Tree("A'")
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                Tree(")"),
                Tree(
                    "R", Tree(":"), Tree("T", Tree("Double"), Tree("T'"))
                )
            )
        )
    }

    @Test
    fun parsesDefaultValues() {
        testCase(
            input = "fun defaults(a: Int = 0, b: Float = 5.4367, c: Int = -36)",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("defaults"),
                Tree("("),
                Tree(
                    "A", Tree("a"), Tree(":"), Tree("T", Tree("Int"), Tree("T'")), Tree("D", Tree("="), Tree("0")),
                    Tree(
                        "A'", Tree(","),
                        Tree(
                            "A", Tree("b"), Tree(":"), Tree("T", Tree("Float"), Tree("T'")), Tree("D", Tree("="), Tree("5.4367")),
                            Tree(
                                "A'", Tree(","),
                                Tree(
                                    "A", Tree("c"), Tree(":"), Tree("T", Tree("Int"), Tree("T'")), Tree("D", Tree("="), Tree("-36")),
                                    Tree("A'")
                                )
                            )
                        )
                    )
                ),
                Tree(")"),
                Tree("R")
            )
        )
    }


    @Test
    fun parsesMixedDefaults() {
        testCase(
            input = "fun defaults(a: String, b: Double = -1.23, c: String)",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("defaults"),
                Tree("("),
                Tree(
                    "A", Tree("a"), Tree(":"), Tree("T", Tree("String"), Tree("T'")), Tree("D"),
                    Tree(
                        "A'", Tree(","),
                        Tree(
                            "A", Tree("b"), Tree(":"), Tree("T", Tree("Double"), Tree("T'")), Tree("D", Tree("="), Tree("-1.23")),
                            Tree(
                                "A'", Tree(","),
                                Tree(
                                    "A", Tree("c"), Tree(":"), Tree("T", Tree("String"), Tree("T'")), Tree("D"),
                                    Tree("A'")
                                )
                            )
                        )
                    )
                ),
                Tree(")"),
                Tree("R")
            )
        )
    }

    @Test
    fun parsesGenerics() {
        testCase(
            input = "fun eval(a: Array<Array<Int>>): Result<Int>",
            expected = Tree(
                "S",
                Tree("fun"),
                Tree("eval"),
                Tree("("),
                Tree(
                    "A", Tree("a"), Tree(":"),
                    Tree(
                        "T", Tree("List"),
                        Tree(
                            "T'", Tree("<"),
                            Tree(
                                "G", Tree("String"), Tree("G'")
                            ),
                            Tree(">")
                        )
                    ), Tree("D"), Tree("A'")
                ),
                Tree(")"),
                Tree(
                    "R", Tree(":"),
                    Tree(
                        "T", Tree("Result"),
                        Tree(
                            "T'", Tree("<"),
                            Tree(
                                "G", Tree("Int"), Tree("G'")
                            ),
                            Tree(">")
                        )
                    )
                )
            )
        )
    }


    @Test
    fun failsWithoutFun() {
        assertFailure {
            testCase(
                input = "main(name: String)",
                expected = Tree("fail")
            )
        }
    }

    @Test
    fun failsOnIllegalName() {
        assertFailure {
            testCase(
                input = "fun startWithDigit(t1: String, 1t: Int): Boolean",
                expected = Tree("fail")
            )
        }
    }

    @Test
    fun failsOnIllegalType() {
        assertFailure {
            testCase(
                input = "fun illegalType(p1: double, p2: double): Double",
                expected = Tree("fail")
            )
        }
    }

    @Test
    fun failsOnEmpty() {
        assertFailure {
            testCase(
                input = "",
                expected = Tree("fail")
            )
        }
    }

    @Test
    fun failsOnIllegalDefaults() {
        assertFailure {
            testCase(
                input = "fun illegal(a: Int = dont)",
                expected = Tree("fail")
            )
        }
    }
}