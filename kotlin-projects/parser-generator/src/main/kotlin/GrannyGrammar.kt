class GrannyGrammar {

    sealed interface TokenDef
    data class StringDef(val str: String) : TokenDef
    data class RegexDef(val str: String) : TokenDef

    private val tokens: MutableMap<String, TokenDef> = mutableMapOf()
    private val rules: MutableMap<String, MutableList<RuleDef>> = mutableMapOf()
    val ruleTail: MutableList<Rule> = mutableListOf()
    var initial: String? = null

    val parameters: MutableList<Pair<String, String>> = mutableListOf()
    val arguments: MutableList<String> = mutableListOf()

    var attr: Triple<String, String, String>? = null

    val synthAttrs: MutableSet<Triple<String, String, String>> = mutableSetOf()

    fun addToken(name: String, def: TokenDef) = this.apply { tokens[name] = def }
    fun addRule(name: String, def: RuleDef) = this.apply {
        if (name in rules) {
            rules[name]?.add(def)
        } else {
            rules[name] = mutableListOf(def)
        }
    }

    fun addRuleTail(
        name: String,
        tail: GrannyGrammar,
        params: List<Pair<String, String>>? = null,
        synth: String? = null
    ) = this.apply { addRule(name, RuleDef(tail.ruleTail, params, synth)) }

    fun appendRuleTail(rule: Rule) = this.apply { ruleTail.add(rule) }
    fun appendRuleTail(rules: List<Rule>) = this.apply { ruleTail.addAll(rules) }

    fun getTokens(): Map<String, TokenDef> = tokens
    fun getRules(): Map<String, List<RuleDef>> = rules

    operator fun plus(other: GrannyGrammar): GrannyGrammar {
        return this.apply {
            other.tokens.forEach { addToken(it.key, it.value) }
            other.rules.forEach { (name, defs) ->
                defs.forEach { addRule(name, it) }
            }
            initial = initial ?: other.initial
        }
    }
}