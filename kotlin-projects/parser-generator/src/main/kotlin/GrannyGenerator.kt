import java.io.File

class GrannyGenerator(private val filename: String) {

    companion object {
        private const val GRANNY_DIR = "./src/main/granny/"
        private const val NEWLINE = "\n"
        private const val TAB = "\t"
        private const val RCURL = "}"
        private const val EPS = ""
    }

    fun generate() {
        val grammar = GrannyGenVisitor().generateGrammar(filename)
        generateTokenFile(grammar)
        generateAnalyzerFile(grammar)
        generateTreeFile(grammar)
        generateParserFile(grammar)
    }

    private fun String.uppercaseFirst() = replaceFirstChar { it.uppercase() }
    private fun String.lowercaseFirst() = replaceFirstChar { it.lowercase() }

    private fun generateTokenFile(grammar: GrannyGrammar) {
        val builder = StringBuilder(GrannyUtils.TOKEN_HEADER)
        for (token in grammar.getTokens().keys) {
            builder.append("data class ${token.uppercaseFirst()}(val value: String) : GrannyToken")
            builder.append(NEWLINE)
        }
        builder.append("object End : GrannyToken { override fun toString() = \"EOF\" }")
        builder.append(NEWLINE)
        if (!File(GRANNY_DIR).exists()) {
            File(GRANNY_DIR).mkdir()
        }
        printToFile("GrannyToken.kt", builder.toString())
    }

    private fun StringBuilder.line(str: String, indent: Int) = append(TAB.repeat(indent) + str).append(NEWLINE)

    private fun generateAnalyzerFile(grammar: GrannyGrammar) {
        val builder = StringBuilder(GrannyUtils.getAnalyzerHeader())
        builder.append(NEWLINE)
        grammar.getTokens().forEach { (token, def) ->
            when (def) {
                is GrannyGrammar.RegexDef -> {
                    builder.line("val ${token}Regex = Regex(${def.str})", 2)
                        .line("while ((value + curChar.toChar()) matches ${token}Regex) {", 2)
                        .line("value += curChar.toChar()", 3)
                        .line("nextChar()", 3)
                        .line(RCURL, 2)
                }
                is GrannyGrammar.StringDef -> {
                    builder.line("while (${def.str}.startsWith(value + curChar.toChar())) {", 2)
                        .line("value += curChar.toChar()", 3)
                        .line("nextChar()", 3)
                        .line(RCURL, 2)
                }
            }
            builder.line("if (value.isNotBlank()) {", 2)
                .line("curToken = ${token.uppercaseFirst()}(value)", 3)
                .line("return", 3)
                .line(RCURL, 2)
        }
        builder.line("parseError()", 2)
            .line(RCURL, 1)
            .line(RCURL, 0)
        printToFile("LexicalAnalyzer.kt", builder.toString())
    }

    private fun String.asValue() = replace("<", "")
        .replace(">", "")
        .trim()

    private fun generateTreeFile(grammar: GrannyGrammar) {
        val code = GrannyUtils.treeSource()
        val builder = StringBuilder()
        grammar.synthAttrs.forEach { (name, type, default) ->
            builder.line("var $name: $type = ${default.asValue()}", 1)
        }
        printToFile(
            "Tree.kt",
            code.replace("%%ATTRS%%", builder.toString())
        )
    }

    private fun Map<String, List<RuleDef>>.asStrings(): Map<String, List<List<String>>> {
        val stringMap = mutableMapOf<String, List<List<String>>>()
        for (name in keys) {
            val rule = this[name]!!.map { ruleDef ->
                ruleDef.rules.map { it.name }
            }
            stringMap[name] = rule
        }
        return stringMap
    }

    private fun buildFirst(grammar: GrannyGrammar): Map<String, Set<String>> {
        val first: MutableMap<String, MutableSet<String>> = mutableMapOf()
        val rules = grammar.getRules().asStrings()
        rules.keys.forEach {
            first[it] = mutableSetOf()
        }
        for ((nonTerm, def) in rules) {
            var curN: String
            var curDef: List<List<String>> = def
            var updated = true
            while (updated) {
                for (r in curDef) {
                    if (r[0].isNotBlank() && r[0].first().isUpperCase()) {
                        curN = r[0]
                        if (curN == nonTerm) throw LeftRecursionException()
                        curDef = rules[curN]!!
                        break
                    }
                    updated = first[nonTerm]!!.add(r[0])
                    continue
                }
            }
        }
        return first
    }

    private operator fun Boolean.plus(other: Boolean) = or(other)

    private fun buildFollow(
        grammar: GrannyGrammar,
        first: Map<String, Set<String>>
    ): Map<String, Set<String>> {
        val follow: MutableMap<String, MutableSet<String>> = mutableMapOf()
        val rules = grammar.getRules().asStrings()
        rules.keys.forEach {
            follow[it] = mutableSetOf()
        }
        follow[grammar.initial]!!.add("$")
        var updated = true
        while (updated) {
            updated = false
            for ((nonTerm, def) in rules) {
                for (rule in def.filter { it.size >= 2 }) {
                    for (i in 1 until rule.size) {
                        val b = rule[i]
                        if (b.first().isLowerCase()) continue
                        val gamma = if (i + 1 < rule.size) {
                            rule[i + 1]
                        } else {
                            EPS
                        }
                        if (gamma.isBlank()) {
                            updated += follow[b]!!.addAll(follow[nonTerm]!!)
                        } else if (gamma.first().isLowerCase()) {
                            updated += follow[b]!!.add(gamma)
                        } else {
                            updated += follow[b]!!.addAll(first[gamma]!!.minus(EPS))
                            if (EPS in first[gamma]!!) {
                                updated += follow[b]!!.addAll(follow[nonTerm]!!)
                            }
                        }
                    }
                }
            }
        }
        return follow
    }

    private fun violatesLL1(
        grammar: GrannyGrammar,
        first: Map<String, Set<String>>,
        follow: Map<String, Set<String>>
    ): Boolean {
        for ((nonTerm, def) in grammar.getRules().asStrings()) {
            if (def.size < 2) continue
            for (alpha in def) {
                for (beta in def) {
                    if (alpha == beta) continue
                    val alphaFirst = if (alpha[0].isBlank() || alpha[0].first().isLowerCase()) {
                        setOf(alpha[0])
                    } else {
                        first[alpha[0]]!!
                    }
                    val betaFirst = if (beta[0].isBlank() || beta[0].first().isLowerCase()) {
                        setOf(beta[0])
                    } else {
                        first[beta[0]]!!
                    }
                    if (alphaFirst.intersect(betaFirst).isNotEmpty()) return true
                    if (EPS in alphaFirst) {
                        if (follow[nonTerm]!!.intersect(betaFirst).isNotEmpty()) return true
                    }
                }
            }
        }
        return false
    }

    private fun String.asRule() = replace("'", "Prime")

    private fun firstRule(rule: List<String>, grammar: GrannyGrammar): Set<String> {
        if (rule[0].first().isLowerCase()) {
            return setOf(rule[0])
        }
        val def = grammar.getRules().asStrings()[rule[0]]!!
        return def
            .filter { it.first().isNotBlank() }
            .fold(setOf()) { acc, r -> acc + firstRule(r, grammar) }
    }

    private fun Set<String>.asTokens() = joinToString(", is ") { it.uppercaseFirst() }

    private fun RuleDef.asStrings() = rules.map { it.name }

    private fun List<Pair<String, String>>.asParameters() = joinToString(", ") {
            (name, type) -> "$name: $type"
    }

    private fun List<String>.asArguments() = joinToString(", ")

    private fun String.asCode() = replace("[", "")
        .replace("]", "")
        .replace(Regex("[A-Z']+[.][a-z_]+")) {
            val (nonTerm, attr) = it.value.split(".", limit = 2)
            nonTerm.lowercaseFirst().asRule() + '.' + attr
        }
        .trim()

    private fun generateParserFile(grammar: GrannyGrammar) {
        val first = buildFirst(grammar)
        val follow = buildFollow(grammar, first)
        if (violatesLL1(grammar, first, follow)) throw NotLL1Grammar()

        val builder = StringBuilder()
        builder.append(GrannyUtils.getParserHeader())
            .append(NEWLINE)
            .append(NEWLINE)
        for (token in grammar.getTokens().keys) {
            builder.line("private fun unwrap${token.uppercaseFirst()}(): String {", 1)
                .line("return if (lex.curToken is ${token.uppercaseFirst()}) {", 2)
                .line("(lex.curToken as ${token.uppercaseFirst()}).value", 3)
                .line("} else {", 2)
                .line("mismatchedToken(\"${token}\")", 3)
                .line(RCURL, 2)
                .line(RCURL, 1)
                .append(NEWLINE)
        }
        builder.append(
            GrannyUtils.getParseFunction(grammar.initial!!.asRule())
        )
        for ((nonTerm, def) in grammar.getRules()) {
            val params = def.first().params ?: listOf()
            builder.line("private fun ${nonTerm.asRule()}(${params.asParameters()}): Tree {", 1)
                .line("return when (lex.curToken) {", 2)
            for (rule in def.filter { it.asStrings().first().isNotBlank() }) {
                val fst = firstRule(rule.asStrings(), grammar)
                builder.line("is ${fst.asTokens()} -> {", 3)
                val children = mutableListOf<String>()
                for (t in rule.rules) {
                    if (t.name.first().isLowerCase()) {
                        builder.line("val ${t.name} = unwrap${t.name.uppercaseFirst()}()", 4)
                            .line("lex.nextToken()", 4)
                        children.add("Tree(${t.name})")
                    } else {
                        val args = t.args ?: listOf()
                        builder.line("val ${t.name.asRule().lowercaseFirst()} = " +
                                "${t.name.asRule()}(${args.asArguments()})", 4)
                        children.add(t.name.asRule().lowercaseFirst())
                    }
                    if (t.block != null) {
                        builder.line(t.block.asCode(), 4)
                    }
                }
                var line = "Tree(\"$nonTerm\", " + children.joinToString(", ") + ")"
                if (rule.synthesize != null) {
                    line += ".apply { ${rule.synthesize?.asCode()} }"
                }
                builder.line(line, 4)
                    .line(RCURL, 3)
            }
            if (EPS in first[nonTerm]!!) {
                val r = def.find { ruleDef -> ruleDef.rules.first().name == EPS }!!
                var line = "else -> Tree(\"$nonTerm\")"
                if (r.synthesize != null) {
                    line += ".apply { ${r.synthesize?.asCode()} }"
                }
                builder.line(line, 3)
            } else {
                builder.line("else -> unexpectedToken()", 3)
            }
            builder.line(RCURL, 2)
                .line(RCURL, 1)
                .append(NEWLINE)
        }
        builder.line(RCURL, 0)
        printToFile("Parser.kt", builder.toString())

    }

    private fun printToFile(file: String, contents: String) =
        File(GRANNY_DIR + file).printWriter().use {
            it.print(contents)
        }
}