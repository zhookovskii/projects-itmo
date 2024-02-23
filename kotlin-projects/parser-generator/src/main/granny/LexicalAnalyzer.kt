import java.io.IOException
import java.io.InputStream
import java.text.ParseException

class LexicalAnalyzer(private val inS: InputStream) {

    private var curChar: Int
    var curPos: Int
        private set
    var curToken: GrannyToken
        private set

    init {
        curChar = -1
        curPos = 0
        curToken = End
        nextChar()
    }

    private fun Char.isBlank(): Boolean {
        return this == ' ' || this == '\r' || this == '\n' || this == '\t'
    }

    private fun parseError(): Nothing {
        if (curChar >= 0) {
            throw ParseException("Illegal character: ${curChar.toChar()} at $curPos", curPos)
        } else {
            throw ParseException("Unexpected EOF at $curPos", curPos)
        }
    }

    private fun nextChar() {
        curPos++
        try {
            curChar = inS.read()
        } catch (e: IOException) {
            throw ParseException(e.message, curPos)
        }
    }

    fun nextToken() {
        while (curChar.toChar().isBlank()) { nextChar() }
        if (curChar == -1) {
            curToken = End
            return
        }
        var value = ""
		val numberRegex = Regex("[0-9]+")
		while ((value + curChar.toChar()) matches numberRegex) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Number(value)
			return
		}
		while ("+".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Plus(value)
			return
		}
		while ("-".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Minus(value)
			return
		}
		while ("*".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Mul(value)
			return
		}
		while ("/".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Div(value)
			return
		}
		while ("(".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Lparen(value)
			return
		}
		while (")".startsWith(value + curChar.toChar())) {
			value += curChar.toChar()
			nextChar()
		}
		if (value.isNotBlank()) {
			curToken = Rparen(value)
			return
		}
		parseError()
	}
}
