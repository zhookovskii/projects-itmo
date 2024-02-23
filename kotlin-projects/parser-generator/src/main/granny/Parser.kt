import java.io.InputStream
import java.text.ParseException

class Parser {

    private lateinit var lex: LexicalAnalyzer

    private fun mismatchedToken(expected: String): Nothing =
        throw ParseException("expected $expected at ${lex.curPos}", lex.curPos)

    private fun unexpectedToken(): Nothing =
        throw ParseException("unexpected ${lex.curToken} at ${lex.curPos}", lex.curPos)

	private fun unwrapNumber(): String {
		return if (lex.curToken is Number) {
			(lex.curToken as Number).value
		} else {
			mismatchedToken("number")
		}
	}

	private fun unwrapPlus(): String {
		return if (lex.curToken is Plus) {
			(lex.curToken as Plus).value
		} else {
			mismatchedToken("plus")
		}
	}

	private fun unwrapMinus(): String {
		return if (lex.curToken is Minus) {
			(lex.curToken as Minus).value
		} else {
			mismatchedToken("minus")
		}
	}

	private fun unwrapMul(): String {
		return if (lex.curToken is Mul) {
			(lex.curToken as Mul).value
		} else {
			mismatchedToken("mul")
		}
	}

	private fun unwrapDiv(): String {
		return if (lex.curToken is Div) {
			(lex.curToken as Div).value
		} else {
			mismatchedToken("div")
		}
	}

	private fun unwrapLparen(): String {
		return if (lex.curToken is Lparen) {
			(lex.curToken as Lparen).value
		} else {
			mismatchedToken("lparen")
		}
	}

	private fun unwrapRparen(): String {
		return if (lex.curToken is Rparen) {
			(lex.curToken as Rparen).value
		} else {
			mismatchedToken("rparen")
		}
	}

	fun parse(inS: InputStream): Tree {
		lex = LexicalAnalyzer(inS)
		lex.nextToken()
		return E()
	}

	private fun E(): Tree {
		return when (lex.curToken) {
			is Lparen, is Number -> {
				val t = T()
				val ePrime = EPrime(t.result)
				Tree("E", t, ePrime).apply { result = ePrime.result }
			}
			else -> unexpectedToken()
		}
	}

	private fun EPrime(acc: Double): Tree {
		return when (lex.curToken) {
			is Plus -> {
				val plus = unwrapPlus()
				lex.nextToken()
				val t = T()
				val temp = acc + t.result
				val ePrime = EPrime(temp)
				Tree("E'", Tree(plus), t, ePrime).apply { result = ePrime.result }
			}
			is Minus -> {
				val minus = unwrapMinus()
				lex.nextToken()
				val t = T()
				val temp = acc - t.result
				val ePrime = EPrime(temp)
				Tree("E'", Tree(minus), t, ePrime).apply { result = ePrime.result }
			}
			else -> Tree("E'").apply { result = acc }
		}
	}

	private fun T(): Tree {
		return when (lex.curToken) {
			is Lparen, is Number -> {
				val f = F()
				val tPrime = TPrime(f.result)
				Tree("T", f, tPrime).apply { result = tPrime.result }
			}
			else -> unexpectedToken()
		}
	}

	private fun TPrime(acc: Double): Tree {
		return when (lex.curToken) {
			is Mul -> {
				val mul = unwrapMul()
				lex.nextToken()
				val f = F()
				val temp = acc * f.result
				val tPrime = TPrime(temp)
				Tree("T'", Tree(mul), f, tPrime).apply { result = tPrime.result }
			}
			is Div -> {
				val div = unwrapDiv()
				lex.nextToken()
				val f = F()
				val temp = acc / f.result
				val tPrime = TPrime(temp)
				Tree("T'", Tree(div), f, tPrime).apply { result = tPrime.result }
			}
			else -> Tree("T'").apply { result = acc }
		}
	}

	private fun F(): Tree {
		return when (lex.curToken) {
			is Lparen -> {
				val lparen = unwrapLparen()
				lex.nextToken()
				val e = E()
				val rparen = unwrapRparen()
				lex.nextToken()
				Tree("F", Tree(lparen), e, Tree(rparen)).apply { result = e.result }
			}
			is Number -> {
				val number = unwrapNumber()
				lex.nextToken()
				Tree("F", Tree(number)).apply { result = number.toDouble() }
			}
			else -> unexpectedToken()
		}
	}

}
