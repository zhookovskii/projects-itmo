import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File

class GrannyGenVisitor : GrannyBaseVisitor<GrannyGrammar>() {

    override fun visitProg(ctx: GrannyParser.ProgContext?): GrannyGrammar {
        return visit(ctx?.grammarBody())
    }

    override fun visitGrammarBody(ctx: GrannyParser.GrammarBodyContext?): GrannyGrammar {
        val g = visit(ctx?.token()) + visit(ctx?.rule_()) + visit(ctx?.init())
        g.apply {
            if (ctx?.synthesize() != null) {
                synthAttrs.addAll(visit(ctx.synthesize()).synthAttrs)
            }
        }
        return g
    }

    override fun visitToken(ctx: GrannyParser.TokenContext?): GrannyGrammar {
        return visit(ctx?.tokenBody())
    }

    override fun visitTokenBody(ctx: GrannyParser.TokenBodyContext?): GrannyGrammar {
        return ctx?.tokenDef()!!.fold(GrannyGrammar()) { acc, tokenDef ->
            acc + visit(tokenDef)
        }
    }

    override fun visitTokenDef(ctx: GrannyParser.TokenDefContext?): GrannyGrammar {
        return if (ctx?.REGEX() != null) {
            GrannyGrammar().addToken(
                ctx.TOKENNAME().text,
                GrannyGrammar.RegexDef(ctx.REGEX().text.drop(1))
            )
        } else if (ctx?.STRING() != null) {
            GrannyGrammar().addToken(
                ctx.TOKENNAME().text,
                GrannyGrammar.StringDef(ctx.STRING().text)
            )
        } else {
            GrannyGrammar()
        }
    }

    override fun visitRule(ctx: GrannyParser.RuleContext?): GrannyGrammar {
        return visit(ctx?.ruleBody())
    }

    override fun visitRuleBody(ctx: GrannyParser.RuleBodyContext?): GrannyGrammar {
        return ctx?.ruleDef()!!.fold(GrannyGrammar()) { acc, ruleDef ->
            acc + visit(ruleDef)
        }
    }

    override fun visitRuleDef(ctx: GrannyParser.RuleDefContext?): GrannyGrammar {
        val params = if (ctx?.parameters() != null) {
            visit(ctx.parameters()).parameters
        } else {
            null
        }
        return if (ctx?.EMPTY() != null) {
            GrannyGrammar().addRule(
                ctx.RULENAME().text,
                RuleDef(
                    listOf(Rule("", null, null)),
                    params,
                    ctx.BLOCK(0)?.text
                )
            )
        } else if (ctx?.ruleSequence() != null) {
            GrannyGrammar().addRuleTail(
                ctx.RULENAME().text,
                visit(ctx.ruleSequence()),
                params,
                ctx.BLOCK(0)?.text
            )
        } else {
            GrannyGrammar()
        }
    }

    override fun visitSynthesize(ctx: GrannyParser.SynthesizeContext?): GrannyGrammar {
        val attrs = ctx?.attrDef()!!.fold(emptySet<Triple<String, String, String>>()) { acc, attrDef ->
            acc + visit(attrDef).attr!!
        }
        return GrannyGrammar().apply { synthAttrs.addAll(attrs) }
    }

    override fun visitAttrDef(ctx: GrannyParser.AttrDefContext?): GrannyGrammar {
        return if (ctx?.TOKENNAME() != null) {
            GrannyGrammar().apply {
                attr = Triple(ctx.TOKENNAME().text, ctx.TYPE().text, ctx.DEFAULT().text)
            }
        } else {
            GrannyGrammar()
        }
    }

    override fun visitParameters(ctx: GrannyParser.ParametersContext?): GrannyGrammar {
        return visit(ctx?.paramSequence())
    }

    override fun visitParamSequence(ctx: GrannyParser.ParamSequenceContext?): GrannyGrammar {
        return if (ctx?.paramSequence() != null) {
            GrannyGrammar().apply {
                parameters.addAll(
                    listOf(ctx.TOKENNAME().text to ctx.TYPE().text) +
                            visit(ctx.paramSequence()).parameters
                )
            }
        } else if (ctx?.TOKENNAME() != null ) {
            GrannyGrammar().apply {
                parameters.add(ctx.TOKENNAME().text to ctx.TYPE().text)
            }
        } else {
            GrannyGrammar()
        }
    }

    override fun visitArguments(ctx: GrannyParser.ArgumentsContext?): GrannyGrammar {
        return visit(ctx?.argSequence())
    }

    private fun dotCall(rule: String?, attr: String): String? {
        if (rule == null) return null
        return rule.replaceFirstChar { it.lowercase() }
            .replace("'", "Prime") +
                '.' + attr
    }

    override fun visitArgSequence(ctx: GrannyParser.ArgSequenceContext?): GrannyGrammar {
        return if (ctx?.argSequence() != null) {
            GrannyGrammar().apply {
                arguments.addAll(
                    listOf(
                        dotCall(ctx.RULENAME()?.text, ctx.TOKENNAME().text) ?: ctx.TOKENNAME().text
                    ) + visit(ctx.argSequence()).arguments
                )
            }
        } else if (ctx?.TOKENNAME() != null) {
            GrannyGrammar().apply {
                arguments.add(dotCall(ctx.RULENAME()?.text, ctx.TOKENNAME().text) ?: ctx.TOKENNAME().text)
            }
        }
        else {
            GrannyGrammar()
        }
    }

    override fun visitRuleSequence(ctx: GrannyParser.RuleSequenceContext?): GrannyGrammar {
        val args = if (ctx?.arguments() != null) {
            visit(ctx.arguments()).arguments
        } else {
            null
        }
        return if (ctx?.ruleSequence() != null) {
            GrannyGrammar().appendRuleTail(
                listOf(
                    Rule(
                        ctx.TOKENNAME()?.text ?: ctx.RULENAME().text,
                        args,
                        ctx.BLOCK()?.text
                    )
                ) + visit(ctx.ruleSequence()).ruleTail
            )
        } else if (ctx?.TOKENNAME() != null) {
            GrannyGrammar().appendRuleTail(
                Rule(ctx.TOKENNAME().text, args, ctx.BLOCK()?.text)
            )
        } else if (ctx?.RULENAME() != null) {
            GrannyGrammar().appendRuleTail(
                Rule(ctx.RULENAME().text, args, ctx.BLOCK()?.text)
            )
        } else {
            GrannyGrammar()
        }
    }

    override fun visitInit(ctx: GrannyParser.InitContext?): GrannyGrammar {
        return GrannyGrammar().apply { initial = ctx?.RULENAME()?.text }
    }

    fun generateGrammar(filename: String): GrannyGrammar {
        val source = File(filename).readLines().joinToString("\n")
        val lexer = GrannyLexer(CharStreams.fromString(source))
        val tokens = CommonTokenStream(lexer)
        val parser = GrannyParser(tokens)
        return visit(parser.prog())
    }
}