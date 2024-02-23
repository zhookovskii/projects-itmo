// Generated from D:/mt-course/MTlab4/src/main/Granny.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link GrannyParser}.
 */
public interface GrannyListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link GrannyParser#prog}.
	 * @param ctx the parse tree
	 */
	void enterProg(GrannyParser.ProgContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#prog}.
	 * @param ctx the parse tree
	 */
	void exitProg(GrannyParser.ProgContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#grammarBody}.
	 * @param ctx the parse tree
	 */
	void enterGrammarBody(GrannyParser.GrammarBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#grammarBody}.
	 * @param ctx the parse tree
	 */
	void exitGrammarBody(GrannyParser.GrammarBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#synthesize}.
	 * @param ctx the parse tree
	 */
	void enterSynthesize(GrannyParser.SynthesizeContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#synthesize}.
	 * @param ctx the parse tree
	 */
	void exitSynthesize(GrannyParser.SynthesizeContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#attrDef}.
	 * @param ctx the parse tree
	 */
	void enterAttrDef(GrannyParser.AttrDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#attrDef}.
	 * @param ctx the parse tree
	 */
	void exitAttrDef(GrannyParser.AttrDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#token}.
	 * @param ctx the parse tree
	 */
	void enterToken(GrannyParser.TokenContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#token}.
	 * @param ctx the parse tree
	 */
	void exitToken(GrannyParser.TokenContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#tokenBody}.
	 * @param ctx the parse tree
	 */
	void enterTokenBody(GrannyParser.TokenBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#tokenBody}.
	 * @param ctx the parse tree
	 */
	void exitTokenBody(GrannyParser.TokenBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#tokenDef}.
	 * @param ctx the parse tree
	 */
	void enterTokenDef(GrannyParser.TokenDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#tokenDef}.
	 * @param ctx the parse tree
	 */
	void exitTokenDef(GrannyParser.TokenDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#rule}.
	 * @param ctx the parse tree
	 */
	void enterRule(GrannyParser.RuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#rule}.
	 * @param ctx the parse tree
	 */
	void exitRule(GrannyParser.RuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#ruleBody}.
	 * @param ctx the parse tree
	 */
	void enterRuleBody(GrannyParser.RuleBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#ruleBody}.
	 * @param ctx the parse tree
	 */
	void exitRuleBody(GrannyParser.RuleBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#ruleDef}.
	 * @param ctx the parse tree
	 */
	void enterRuleDef(GrannyParser.RuleDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#ruleDef}.
	 * @param ctx the parse tree
	 */
	void exitRuleDef(GrannyParser.RuleDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#parameters}.
	 * @param ctx the parse tree
	 */
	void enterParameters(GrannyParser.ParametersContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#parameters}.
	 * @param ctx the parse tree
	 */
	void exitParameters(GrannyParser.ParametersContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#paramSequence}.
	 * @param ctx the parse tree
	 */
	void enterParamSequence(GrannyParser.ParamSequenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#paramSequence}.
	 * @param ctx the parse tree
	 */
	void exitParamSequence(GrannyParser.ParamSequenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#ruleSequence}.
	 * @param ctx the parse tree
	 */
	void enterRuleSequence(GrannyParser.RuleSequenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#ruleSequence}.
	 * @param ctx the parse tree
	 */
	void exitRuleSequence(GrannyParser.RuleSequenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#arguments}.
	 * @param ctx the parse tree
	 */
	void enterArguments(GrannyParser.ArgumentsContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#arguments}.
	 * @param ctx the parse tree
	 */
	void exitArguments(GrannyParser.ArgumentsContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#argSequence}.
	 * @param ctx the parse tree
	 */
	void enterArgSequence(GrannyParser.ArgSequenceContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#argSequence}.
	 * @param ctx the parse tree
	 */
	void exitArgSequence(GrannyParser.ArgSequenceContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrannyParser#init}.
	 * @param ctx the parse tree
	 */
	void enterInit(GrannyParser.InitContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrannyParser#init}.
	 * @param ctx the parse tree
	 */
	void exitInit(GrannyParser.InitContext ctx);
}