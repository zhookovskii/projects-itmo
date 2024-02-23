// Generated from D:/mt-course/MTlab4/src/main/Granny.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class GrannyParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		GRAMMAR=1, TOKEN=2, RULE=3, INIT=4, SYNTH=5, LCURL=6, RCURL=7, WS=8, SEMICOLON=9, 
		EQUALS=10, RULENAME=11, TOKENNAME=12, TYPE=13, DEFAULT=14, STRING=15, 
		REGEX=16, ARROW=17, SLIM_ARROW=18, BLOCK=19, COMMA=20, COLON=21, LPAREN=22, 
		RPAREN=23, DOT=24, EMPTY=25;
	public static final int
		RULE_prog = 0, RULE_grammarBody = 1, RULE_synthesize = 2, RULE_attrDef = 3, 
		RULE_token = 4, RULE_tokenBody = 5, RULE_tokenDef = 6, RULE_rule = 7, 
		RULE_ruleBody = 8, RULE_ruleDef = 9, RULE_parameters = 10, RULE_paramSequence = 11, 
		RULE_ruleSequence = 12, RULE_arguments = 13, RULE_argSequence = 14, RULE_init = 15;
	private static String[] makeRuleNames() {
		return new String[] {
			"prog", "grammarBody", "synthesize", "attrDef", "token", "tokenBody", 
			"tokenDef", "rule", "ruleBody", "ruleDef", "parameters", "paramSequence", 
			"ruleSequence", "arguments", "argSequence", "init"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'GRAMMAR'", "'TOKEN'", "'RULE'", "'INIT'", "'SYNTH'", "'{'", "'}'", 
			null, "';'", "'='", null, null, null, null, null, null, "'=>'", "'->'", 
			null, "','", "':'", "'('", "')'", "'.'", "'#EMPTY'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "GRAMMAR", "TOKEN", "RULE", "INIT", "SYNTH", "LCURL", "RCURL", 
			"WS", "SEMICOLON", "EQUALS", "RULENAME", "TOKENNAME", "TYPE", "DEFAULT", 
			"STRING", "REGEX", "ARROW", "SLIM_ARROW", "BLOCK", "COMMA", "COLON", 
			"LPAREN", "RPAREN", "DOT", "EMPTY"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Granny.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public GrannyParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgContext extends ParserRuleContext {
		public TerminalNode GRAMMAR() { return getToken(GrannyParser.GRAMMAR, 0); }
		public TerminalNode LCURL() { return getToken(GrannyParser.LCURL, 0); }
		public GrammarBodyContext grammarBody() {
			return getRuleContext(GrammarBodyContext.class,0);
		}
		public TerminalNode RCURL() { return getToken(GrannyParser.RCURL, 0); }
		public ProgContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_prog; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterProg(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitProg(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitProg(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ProgContext prog() throws RecognitionException {
		ProgContext _localctx = new ProgContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_prog);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(32);
			match(GRAMMAR);
			setState(33);
			match(LCURL);
			setState(34);
			grammarBody();
			setState(35);
			match(RCURL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class GrammarBodyContext extends ParserRuleContext {
		public TokenContext token() {
			return getRuleContext(TokenContext.class,0);
		}
		public RuleContext rule_() {
			return getRuleContext(RuleContext.class,0);
		}
		public InitContext init() {
			return getRuleContext(InitContext.class,0);
		}
		public SynthesizeContext synthesize() {
			return getRuleContext(SynthesizeContext.class,0);
		}
		public GrammarBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_grammarBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterGrammarBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitGrammarBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitGrammarBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final GrammarBodyContext grammarBody() throws RecognitionException {
		GrammarBodyContext _localctx = new GrammarBodyContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_grammarBody);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			token();
			setState(39);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==SYNTH) {
				{
				setState(38);
				synthesize();
				}
			}

			setState(41);
			rule_();
			setState(42);
			init();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class SynthesizeContext extends ParserRuleContext {
		public TerminalNode SYNTH() { return getToken(GrannyParser.SYNTH, 0); }
		public TerminalNode LCURL() { return getToken(GrannyParser.LCURL, 0); }
		public TerminalNode RCURL() { return getToken(GrannyParser.RCURL, 0); }
		public List<AttrDefContext> attrDef() {
			return getRuleContexts(AttrDefContext.class);
		}
		public AttrDefContext attrDef(int i) {
			return getRuleContext(AttrDefContext.class,i);
		}
		public SynthesizeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_synthesize; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterSynthesize(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitSynthesize(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitSynthesize(this);
			else return visitor.visitChildren(this);
		}
	}

	public final SynthesizeContext synthesize() throws RecognitionException {
		SynthesizeContext _localctx = new SynthesizeContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_synthesize);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(44);
			match(SYNTH);
			setState(45);
			match(LCURL);
			{
			setState(47); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(46);
				attrDef();
				}
				}
				setState(49); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==TOKENNAME );
			}
			setState(51);
			match(RCURL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AttrDefContext extends ParserRuleContext {
		public TerminalNode TOKENNAME() { return getToken(GrannyParser.TOKENNAME, 0); }
		public TerminalNode COLON() { return getToken(GrannyParser.COLON, 0); }
		public TerminalNode TYPE() { return getToken(GrannyParser.TYPE, 0); }
		public TerminalNode DEFAULT() { return getToken(GrannyParser.DEFAULT, 0); }
		public TerminalNode SEMICOLON() { return getToken(GrannyParser.SEMICOLON, 0); }
		public AttrDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_attrDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterAttrDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitAttrDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitAttrDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final AttrDefContext attrDef() throws RecognitionException {
		AttrDefContext _localctx = new AttrDefContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_attrDef);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(53);
			match(TOKENNAME);
			setState(54);
			match(COLON);
			setState(55);
			match(TYPE);
			setState(56);
			match(DEFAULT);
			setState(57);
			match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TokenContext extends ParserRuleContext {
		public TerminalNode TOKEN() { return getToken(GrannyParser.TOKEN, 0); }
		public TerminalNode LCURL() { return getToken(GrannyParser.LCURL, 0); }
		public TokenBodyContext tokenBody() {
			return getRuleContext(TokenBodyContext.class,0);
		}
		public TerminalNode RCURL() { return getToken(GrannyParser.RCURL, 0); }
		public TokenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_token; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterToken(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitToken(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitToken(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenContext token() throws RecognitionException {
		TokenContext _localctx = new TokenContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_token);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			match(TOKEN);
			setState(60);
			match(LCURL);
			setState(61);
			tokenBody();
			setState(62);
			match(RCURL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TokenBodyContext extends ParserRuleContext {
		public List<TokenDefContext> tokenDef() {
			return getRuleContexts(TokenDefContext.class);
		}
		public TokenDefContext tokenDef(int i) {
			return getRuleContext(TokenDefContext.class,i);
		}
		public TokenBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterTokenBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitTokenBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitTokenBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenBodyContext tokenBody() throws RecognitionException {
		TokenBodyContext _localctx = new TokenBodyContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_tokenBody);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(64);
				tokenDef();
				}
				}
				setState(67); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==TOKENNAME );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TokenDefContext extends ParserRuleContext {
		public TerminalNode TOKENNAME() { return getToken(GrannyParser.TOKENNAME, 0); }
		public TerminalNode EQUALS() { return getToken(GrannyParser.EQUALS, 0); }
		public TerminalNode SEMICOLON() { return getToken(GrannyParser.SEMICOLON, 0); }
		public TerminalNode STRING() { return getToken(GrannyParser.STRING, 0); }
		public TerminalNode REGEX() { return getToken(GrannyParser.REGEX, 0); }
		public TokenDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tokenDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterTokenDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitTokenDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitTokenDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final TokenDefContext tokenDef() throws RecognitionException {
		TokenDefContext _localctx = new TokenDefContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_tokenDef);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(69);
			match(TOKENNAME);
			setState(70);
			match(EQUALS);
			setState(71);
			_la = _input.LA(1);
			if ( !(_la==STRING || _la==REGEX) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(72);
			match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleContext extends ParserRuleContext {
		public TerminalNode RULE() { return getToken(GrannyParser.RULE, 0); }
		public TerminalNode LCURL() { return getToken(GrannyParser.LCURL, 0); }
		public RuleBodyContext ruleBody() {
			return getRuleContext(RuleBodyContext.class,0);
		}
		public TerminalNode RCURL() { return getToken(GrannyParser.RCURL, 0); }
		public RuleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rule; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterRule(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitRule(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitRule(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RuleContext rule_() throws RecognitionException {
		RuleContext _localctx = new RuleContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_rule);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(74);
			match(RULE);
			setState(75);
			match(LCURL);
			setState(76);
			ruleBody();
			setState(77);
			match(RCURL);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleBodyContext extends ParserRuleContext {
		public List<RuleDefContext> ruleDef() {
			return getRuleContexts(RuleDefContext.class);
		}
		public RuleDefContext ruleDef(int i) {
			return getRuleContext(RuleDefContext.class,i);
		}
		public RuleBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterRuleBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitRuleBody(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitRuleBody(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RuleBodyContext ruleBody() throws RecognitionException {
		RuleBodyContext _localctx = new RuleBodyContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_ruleBody);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(80); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(79);
				ruleDef();
				}
				}
				setState(82); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==RULENAME );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleDefContext extends ParserRuleContext {
		public TerminalNode RULENAME() { return getToken(GrannyParser.RULENAME, 0); }
		public TerminalNode ARROW() { return getToken(GrannyParser.ARROW, 0); }
		public TerminalNode SEMICOLON() { return getToken(GrannyParser.SEMICOLON, 0); }
		public TerminalNode EMPTY() { return getToken(GrannyParser.EMPTY, 0); }
		public RuleSequenceContext ruleSequence() {
			return getRuleContext(RuleSequenceContext.class,0);
		}
		public ParametersContext parameters() {
			return getRuleContext(ParametersContext.class,0);
		}
		public List<TerminalNode> SLIM_ARROW() { return getTokens(GrannyParser.SLIM_ARROW); }
		public TerminalNode SLIM_ARROW(int i) {
			return getToken(GrannyParser.SLIM_ARROW, i);
		}
		public List<TerminalNode> BLOCK() { return getTokens(GrannyParser.BLOCK); }
		public TerminalNode BLOCK(int i) {
			return getToken(GrannyParser.BLOCK, i);
		}
		public RuleDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterRuleDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitRuleDef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitRuleDef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RuleDefContext ruleDef() throws RecognitionException {
		RuleDefContext _localctx = new RuleDefContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_ruleDef);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(84);
			match(RULENAME);
			setState(86);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(85);
				parameters();
				}
			}

			setState(88);
			match(ARROW);
			setState(95);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case EMPTY:
				{
				setState(89);
				match(EMPTY);
				setState(92);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
				case 1:
					{
					setState(90);
					match(SLIM_ARROW);
					setState(91);
					match(BLOCK);
					}
					break;
				}
				}
				break;
			case RULENAME:
			case TOKENNAME:
				{
				setState(94);
				ruleSequence();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(99);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==SLIM_ARROW) {
				{
				setState(97);
				match(SLIM_ARROW);
				setState(98);
				match(BLOCK);
				}
			}

			setState(101);
			match(SEMICOLON);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParametersContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(GrannyParser.LPAREN, 0); }
		public ParamSequenceContext paramSequence() {
			return getRuleContext(ParamSequenceContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(GrannyParser.RPAREN, 0); }
		public ParametersContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_parameters; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterParameters(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitParameters(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitParameters(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParametersContext parameters() throws RecognitionException {
		ParametersContext _localctx = new ParametersContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_parameters);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(103);
			match(LPAREN);
			setState(104);
			paramSequence();
			setState(105);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParamSequenceContext extends ParserRuleContext {
		public TerminalNode TOKENNAME() { return getToken(GrannyParser.TOKENNAME, 0); }
		public TerminalNode COLON() { return getToken(GrannyParser.COLON, 0); }
		public TerminalNode TYPE() { return getToken(GrannyParser.TYPE, 0); }
		public TerminalNode COMMA() { return getToken(GrannyParser.COMMA, 0); }
		public ParamSequenceContext paramSequence() {
			return getRuleContext(ParamSequenceContext.class,0);
		}
		public ParamSequenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramSequence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterParamSequence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitParamSequence(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitParamSequence(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ParamSequenceContext paramSequence() throws RecognitionException {
		ParamSequenceContext _localctx = new ParamSequenceContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_paramSequence);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(107);
			match(TOKENNAME);
			setState(108);
			match(COLON);
			setState(109);
			match(TYPE);
			setState(112);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(110);
				match(COMMA);
				setState(111);
				paramSequence();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleSequenceContext extends ParserRuleContext {
		public TerminalNode TOKENNAME() { return getToken(GrannyParser.TOKENNAME, 0); }
		public TerminalNode RULENAME() { return getToken(GrannyParser.RULENAME, 0); }
		public ArgumentsContext arguments() {
			return getRuleContext(ArgumentsContext.class,0);
		}
		public TerminalNode BLOCK() { return getToken(GrannyParser.BLOCK, 0); }
		public TerminalNode COMMA() { return getToken(GrannyParser.COMMA, 0); }
		public RuleSequenceContext ruleSequence() {
			return getRuleContext(RuleSequenceContext.class,0);
		}
		public RuleSequenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleSequence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterRuleSequence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitRuleSequence(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitRuleSequence(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RuleSequenceContext ruleSequence() throws RecognitionException {
		RuleSequenceContext _localctx = new RuleSequenceContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_ruleSequence);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(114);
			_la = _input.LA(1);
			if ( !(_la==RULENAME || _la==TOKENNAME) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(116);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(115);
				arguments();
				}
			}

			setState(119);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==BLOCK) {
				{
				setState(118);
				match(BLOCK);
				}
			}

			setState(123);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(121);
				match(COMMA);
				setState(122);
				ruleSequence();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArgumentsContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(GrannyParser.LPAREN, 0); }
		public ArgSequenceContext argSequence() {
			return getRuleContext(ArgSequenceContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(GrannyParser.RPAREN, 0); }
		public ArgumentsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arguments; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterArguments(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitArguments(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitArguments(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgumentsContext arguments() throws RecognitionException {
		ArgumentsContext _localctx = new ArgumentsContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_arguments);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(125);
			match(LPAREN);
			setState(126);
			argSequence();
			setState(127);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ArgSequenceContext extends ParserRuleContext {
		public TerminalNode TOKENNAME() { return getToken(GrannyParser.TOKENNAME, 0); }
		public TerminalNode RULENAME() { return getToken(GrannyParser.RULENAME, 0); }
		public TerminalNode DOT() { return getToken(GrannyParser.DOT, 0); }
		public TerminalNode COMMA() { return getToken(GrannyParser.COMMA, 0); }
		public ArgSequenceContext argSequence() {
			return getRuleContext(ArgSequenceContext.class,0);
		}
		public ArgSequenceContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argSequence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterArgSequence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitArgSequence(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitArgSequence(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ArgSequenceContext argSequence() throws RecognitionException {
		ArgSequenceContext _localctx = new ArgSequenceContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_argSequence);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(133);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TOKENNAME:
				{
				setState(129);
				match(TOKENNAME);
				}
				break;
			case RULENAME:
				{
				setState(130);
				match(RULENAME);
				setState(131);
				match(DOT);
				setState(132);
				match(TOKENNAME);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(137);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(135);
				match(COMMA);
				setState(136);
				argSequence();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class InitContext extends ParserRuleContext {
		public TerminalNode INIT() { return getToken(GrannyParser.INIT, 0); }
		public TerminalNode EQUALS() { return getToken(GrannyParser.EQUALS, 0); }
		public TerminalNode RULENAME() { return getToken(GrannyParser.RULENAME, 0); }
		public InitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_init; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).enterInit(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof GrannyListener ) ((GrannyListener)listener).exitInit(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof GrannyVisitor ) return ((GrannyVisitor<? extends T>)visitor).visitInit(this);
			else return visitor.visitChildren(this);
		}
	}

	public final InitContext init() throws RecognitionException {
		InitContext _localctx = new InitContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_init);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
			match(INIT);
			setState(140);
			match(EQUALS);
			setState(141);
			match(RULENAME);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u0019\u0090\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001"+
		"\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004"+
		"\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007"+
		"\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b"+
		"\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007"+
		"\u000f\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0001\u0001\u0003\u0001(\b\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0004\u00020\b\u0002\u000b"+
		"\u0002\f\u00021\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0004\u0001\u0005\u0004\u0005B\b\u0005\u000b"+
		"\u0005\f\u0005C\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001"+
		"\b\u0004\bQ\b\b\u000b\b\f\bR\u0001\t\u0001\t\u0003\tW\b\t\u0001\t\u0001"+
		"\t\u0001\t\u0001\t\u0003\t]\b\t\u0001\t\u0003\t`\b\t\u0001\t\u0001\t\u0003"+
		"\td\b\t\u0001\t\u0001\t\u0001\n\u0001\n\u0001\n\u0001\n\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0003\u000bq\b\u000b\u0001"+
		"\f\u0001\f\u0003\fu\b\f\u0001\f\u0003\fx\b\f\u0001\f\u0001\f\u0003\f|"+
		"\b\f\u0001\r\u0001\r\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e"+
		"\u0001\u000e\u0003\u000e\u0086\b\u000e\u0001\u000e\u0001\u000e\u0003\u000e"+
		"\u008a\b\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0000\u0000\u0010\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014"+
		"\u0016\u0018\u001a\u001c\u001e\u0000\u0002\u0001\u0000\u000f\u0010\u0001"+
		"\u0000\u000b\f\u008d\u0000 \u0001\u0000\u0000\u0000\u0002%\u0001\u0000"+
		"\u0000\u0000\u0004,\u0001\u0000\u0000\u0000\u00065\u0001\u0000\u0000\u0000"+
		"\b;\u0001\u0000\u0000\u0000\nA\u0001\u0000\u0000\u0000\fE\u0001\u0000"+
		"\u0000\u0000\u000eJ\u0001\u0000\u0000\u0000\u0010P\u0001\u0000\u0000\u0000"+
		"\u0012T\u0001\u0000\u0000\u0000\u0014g\u0001\u0000\u0000\u0000\u0016k"+
		"\u0001\u0000\u0000\u0000\u0018r\u0001\u0000\u0000\u0000\u001a}\u0001\u0000"+
		"\u0000\u0000\u001c\u0085\u0001\u0000\u0000\u0000\u001e\u008b\u0001\u0000"+
		"\u0000\u0000 !\u0005\u0001\u0000\u0000!\"\u0005\u0006\u0000\u0000\"#\u0003"+
		"\u0002\u0001\u0000#$\u0005\u0007\u0000\u0000$\u0001\u0001\u0000\u0000"+
		"\u0000%\'\u0003\b\u0004\u0000&(\u0003\u0004\u0002\u0000\'&\u0001\u0000"+
		"\u0000\u0000\'(\u0001\u0000\u0000\u0000()\u0001\u0000\u0000\u0000)*\u0003"+
		"\u000e\u0007\u0000*+\u0003\u001e\u000f\u0000+\u0003\u0001\u0000\u0000"+
		"\u0000,-\u0005\u0005\u0000\u0000-/\u0005\u0006\u0000\u0000.0\u0003\u0006"+
		"\u0003\u0000/.\u0001\u0000\u0000\u000001\u0001\u0000\u0000\u00001/\u0001"+
		"\u0000\u0000\u000012\u0001\u0000\u0000\u000023\u0001\u0000\u0000\u0000"+
		"34\u0005\u0007\u0000\u00004\u0005\u0001\u0000\u0000\u000056\u0005\f\u0000"+
		"\u000067\u0005\u0015\u0000\u000078\u0005\r\u0000\u000089\u0005\u000e\u0000"+
		"\u00009:\u0005\t\u0000\u0000:\u0007\u0001\u0000\u0000\u0000;<\u0005\u0002"+
		"\u0000\u0000<=\u0005\u0006\u0000\u0000=>\u0003\n\u0005\u0000>?\u0005\u0007"+
		"\u0000\u0000?\t\u0001\u0000\u0000\u0000@B\u0003\f\u0006\u0000A@\u0001"+
		"\u0000\u0000\u0000BC\u0001\u0000\u0000\u0000CA\u0001\u0000\u0000\u0000"+
		"CD\u0001\u0000\u0000\u0000D\u000b\u0001\u0000\u0000\u0000EF\u0005\f\u0000"+
		"\u0000FG\u0005\n\u0000\u0000GH\u0007\u0000\u0000\u0000HI\u0005\t\u0000"+
		"\u0000I\r\u0001\u0000\u0000\u0000JK\u0005\u0003\u0000\u0000KL\u0005\u0006"+
		"\u0000\u0000LM\u0003\u0010\b\u0000MN\u0005\u0007\u0000\u0000N\u000f\u0001"+
		"\u0000\u0000\u0000OQ\u0003\u0012\t\u0000PO\u0001\u0000\u0000\u0000QR\u0001"+
		"\u0000\u0000\u0000RP\u0001\u0000\u0000\u0000RS\u0001\u0000\u0000\u0000"+
		"S\u0011\u0001\u0000\u0000\u0000TV\u0005\u000b\u0000\u0000UW\u0003\u0014"+
		"\n\u0000VU\u0001\u0000\u0000\u0000VW\u0001\u0000\u0000\u0000WX\u0001\u0000"+
		"\u0000\u0000X_\u0005\u0011\u0000\u0000Y\\\u0005\u0019\u0000\u0000Z[\u0005"+
		"\u0012\u0000\u0000[]\u0005\u0013\u0000\u0000\\Z\u0001\u0000\u0000\u0000"+
		"\\]\u0001\u0000\u0000\u0000]`\u0001\u0000\u0000\u0000^`\u0003\u0018\f"+
		"\u0000_Y\u0001\u0000\u0000\u0000_^\u0001\u0000\u0000\u0000`c\u0001\u0000"+
		"\u0000\u0000ab\u0005\u0012\u0000\u0000bd\u0005\u0013\u0000\u0000ca\u0001"+
		"\u0000\u0000\u0000cd\u0001\u0000\u0000\u0000de\u0001\u0000\u0000\u0000"+
		"ef\u0005\t\u0000\u0000f\u0013\u0001\u0000\u0000\u0000gh\u0005\u0016\u0000"+
		"\u0000hi\u0003\u0016\u000b\u0000ij\u0005\u0017\u0000\u0000j\u0015\u0001"+
		"\u0000\u0000\u0000kl\u0005\f\u0000\u0000lm\u0005\u0015\u0000\u0000mp\u0005"+
		"\r\u0000\u0000no\u0005\u0014\u0000\u0000oq\u0003\u0016\u000b\u0000pn\u0001"+
		"\u0000\u0000\u0000pq\u0001\u0000\u0000\u0000q\u0017\u0001\u0000\u0000"+
		"\u0000rt\u0007\u0001\u0000\u0000su\u0003\u001a\r\u0000ts\u0001\u0000\u0000"+
		"\u0000tu\u0001\u0000\u0000\u0000uw\u0001\u0000\u0000\u0000vx\u0005\u0013"+
		"\u0000\u0000wv\u0001\u0000\u0000\u0000wx\u0001\u0000\u0000\u0000x{\u0001"+
		"\u0000\u0000\u0000yz\u0005\u0014\u0000\u0000z|\u0003\u0018\f\u0000{y\u0001"+
		"\u0000\u0000\u0000{|\u0001\u0000\u0000\u0000|\u0019\u0001\u0000\u0000"+
		"\u0000}~\u0005\u0016\u0000\u0000~\u007f\u0003\u001c\u000e\u0000\u007f"+
		"\u0080\u0005\u0017\u0000\u0000\u0080\u001b\u0001\u0000\u0000\u0000\u0081"+
		"\u0086\u0005\f\u0000\u0000\u0082\u0083\u0005\u000b\u0000\u0000\u0083\u0084"+
		"\u0005\u0018\u0000\u0000\u0084\u0086\u0005\f\u0000\u0000\u0085\u0081\u0001"+
		"\u0000\u0000\u0000\u0085\u0082\u0001\u0000\u0000\u0000\u0086\u0089\u0001"+
		"\u0000\u0000\u0000\u0087\u0088\u0005\u0014\u0000\u0000\u0088\u008a\u0003"+
		"\u001c\u000e\u0000\u0089\u0087\u0001\u0000\u0000\u0000\u0089\u008a\u0001"+
		"\u0000\u0000\u0000\u008a\u001d\u0001\u0000\u0000\u0000\u008b\u008c\u0005"+
		"\u0004\u0000\u0000\u008c\u008d\u0005\n\u0000\u0000\u008d\u008e\u0005\u000b"+
		"\u0000\u0000\u008e\u001f\u0001\u0000\u0000\u0000\u000e\'1CRV\\_cptw{\u0085"+
		"\u0089";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}