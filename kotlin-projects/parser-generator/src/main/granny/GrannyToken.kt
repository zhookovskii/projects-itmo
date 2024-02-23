sealed interface GrannyToken

data class Number(val value: String) : GrannyToken
data class Plus(val value: String) : GrannyToken
data class Minus(val value: String) : GrannyToken
data class Mul(val value: String) : GrannyToken
data class Div(val value: String) : GrannyToken
data class Lparen(val value: String) : GrannyToken
data class Rparen(val value: String) : GrannyToken
object End : GrannyToken { override fun toString() = "EOF" }
