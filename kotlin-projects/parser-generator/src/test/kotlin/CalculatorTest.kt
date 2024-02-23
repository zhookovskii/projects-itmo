import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test

class CalculatorTest {

    companion object {

        @JvmStatic
        @BeforeAll
        fun beforeAll() {
            GrannyGenerator("./src/main/calculator.txt").generate()
        }
    }


    @Test
    fun dummy() {
        "4-4-8".byteInputStream().use {
            val t = Parser().parse(it)
            Assertions.assertEquals(-8.0, t.result, 0.001)
            t.toSvg("src/main/tree.txt")
        }
    }
}