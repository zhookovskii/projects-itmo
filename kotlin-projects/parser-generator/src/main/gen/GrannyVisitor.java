// Generated from D:/mt-course/MTlab4/src/main/Granny.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link GrannyParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface GrannyVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link GrannyParser#prog}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProg(GrannyParser.ProgContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#grammarBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGrammarBody(GrannyParser.GrammarBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#synthesize}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSynthesize(GrannyParser.SynthesizeContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#attrDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttrDef(GrannyParser.AttrDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#token}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitToken(GrannyParser.TokenContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#tokenBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenBody(GrannyParser.TokenBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#tokenDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTokenDef(GrannyParser.TokenDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#rule}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRule(GrannyParser.RuleContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#ruleBody}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleBody(GrannyParser.RuleBodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#ruleDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleDef(GrannyParser.RuleDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#parameters}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParameters(GrannyParser.ParametersContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#paramSequence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamSequence(GrannyParser.ParamSequenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#ruleSequence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuleSequence(GrannyParser.RuleSequenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#arguments}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArguments(GrannyParser.ArgumentsContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#argSequence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgSequence(GrannyParser.ArgSequenceContext ctx);
	/**
	 * Visit a parse tree produced by {@link GrannyParser#init}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInit(GrannyParser.InitContext ctx);
}