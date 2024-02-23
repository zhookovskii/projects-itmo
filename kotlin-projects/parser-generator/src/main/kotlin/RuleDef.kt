data class RuleDef(
    val rules: List<Rule>,
    val params: List<Pair<String, String>>? = null,
    var synthesize: String? = null
)