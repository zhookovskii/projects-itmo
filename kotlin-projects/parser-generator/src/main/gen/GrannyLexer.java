// Generated from D:/mt-course/MTlab4/src/main/Granny.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class GrannyLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		GRAMMAR=1, TOKEN=2, RULE=3, INIT=4, SYNTH=5, LCURL=6, RCURL=7, WS=8, SEMICOLON=9, 
		EQUALS=10, RULENAME=11, TOKENNAME=12, TYPE=13, DEFAULT=14, STRING=15, 
		REGEX=16, ARROW=17, SLIM_ARROW=18, BLOCK=19, COMMA=20, COLON=21, LPAREN=22, 
		RPAREN=23, DOT=24, EMPTY=25;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"GRAMMAR", "TOKEN", "RULE", "INIT", "SYNTH", "LCURL", "RCURL", "WS", 
			"SEMICOLON", "EQUALS", "RULENAME", "TOKENNAME", "TYPE", "DEFAULT", "STRING", 
			"REGEX", "ARROW", "SLIM_ARROW", "BLOCK", "COMMA", "COLON", "LPAREN", 
			"RPAREN", "DOT", "EMPTY"
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


	public GrannyLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Granny.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\u0019\u00aa\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002"+
		"\u0001\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002"+
		"\u0004\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002"+
		"\u0007\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002"+
		"\u000b\u0007\u000b\u0002\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e"+
		"\u0002\u000f\u0007\u000f\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011"+
		"\u0002\u0012\u0007\u0012\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014"+
		"\u0002\u0015\u0007\u0015\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017"+
		"\u0002\u0018\u0007\u0018\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003"+
		"\u0001\u0003\u0001\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004"+
		"\u0001\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006"+
		"\u0001\u0007\u0004\u0007W\b\u0007\u000b\u0007\f\u0007X\u0001\u0007\u0001"+
		"\u0007\u0001\b\u0001\b\u0001\t\u0001\t\u0001\n\u0004\nb\b\n\u000b\n\f"+
		"\nc\u0001\u000b\u0004\u000bg\b\u000b\u000b\u000b\f\u000bh\u0001\f\u0001"+
		"\f\u0005\fm\b\f\n\f\f\fp\t\f\u0001\r\u0001\r\u0004\rt\b\r\u000b\r\f\r"+
		"u\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0004\u000e|\b\u000e\u000b\u000e"+
		"\f\u000e}\u0001\u000e\u0001\u000e\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0004\u000f\u0086\b\u000f\u000b\u000f\f\u000f\u0087\u0001"+
		"\u000f\u0001\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0001\u0011\u0001"+
		"\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0004\u0012\u0094\b\u0012\u000b"+
		"\u0012\f\u0012\u0095\u0001\u0012\u0001\u0012\u0001\u0013\u0001\u0013\u0001"+
		"\u0014\u0001\u0014\u0001\u0015\u0001\u0015\u0001\u0016\u0001\u0016\u0001"+
		"\u0017\u0001\u0017\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001"+
		"\u0018\u0001\u0018\u0001\u0018\u0004u}\u0087\u0095\u0000\u0019\u0001\u0001"+
		"\u0003\u0002\u0005\u0003\u0007\u0004\t\u0005\u000b\u0006\r\u0007\u000f"+
		"\b\u0011\t\u0013\n\u0015\u000b\u0017\f\u0019\r\u001b\u000e\u001d\u000f"+
		"\u001f\u0010!\u0011#\u0012%\u0013\'\u0014)\u0015+\u0016-\u0017/\u0018"+
		"1\u0019\u0001\u0000\u0005\u0003\u0000\t\n\r\r  \u0002\u0000\'\'AZ\u0002"+
		"\u0000__az\u0001\u0000AZ\u0001\u0000az\u00b1\u0000\u0001\u0001\u0000\u0000"+
		"\u0000\u0000\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000"+
		"\u0000\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000"+
		"\u0000\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000"+
		"\u000f\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000"+
		"\u0013\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0000"+
		"\u0017\u0001\u0000\u0000\u0000\u0000\u0019\u0001\u0000\u0000\u0000\u0000"+
		"\u001b\u0001\u0000\u0000\u0000\u0000\u001d\u0001\u0000\u0000\u0000\u0000"+
		"\u001f\u0001\u0000\u0000\u0000\u0000!\u0001\u0000\u0000\u0000\u0000#\u0001"+
		"\u0000\u0000\u0000\u0000%\u0001\u0000\u0000\u0000\u0000\'\u0001\u0000"+
		"\u0000\u0000\u0000)\u0001\u0000\u0000\u0000\u0000+\u0001\u0000\u0000\u0000"+
		"\u0000-\u0001\u0000\u0000\u0000\u0000/\u0001\u0000\u0000\u0000\u00001"+
		"\u0001\u0000\u0000\u0000\u00013\u0001\u0000\u0000\u0000\u0003;\u0001\u0000"+
		"\u0000\u0000\u0005A\u0001\u0000\u0000\u0000\u0007F\u0001\u0000\u0000\u0000"+
		"\tK\u0001\u0000\u0000\u0000\u000bQ\u0001\u0000\u0000\u0000\rS\u0001\u0000"+
		"\u0000\u0000\u000fV\u0001\u0000\u0000\u0000\u0011\\\u0001\u0000\u0000"+
		"\u0000\u0013^\u0001\u0000\u0000\u0000\u0015a\u0001\u0000\u0000\u0000\u0017"+
		"f\u0001\u0000\u0000\u0000\u0019j\u0001\u0000\u0000\u0000\u001bq\u0001"+
		"\u0000\u0000\u0000\u001dy\u0001\u0000\u0000\u0000\u001f\u0081\u0001\u0000"+
		"\u0000\u0000!\u008b\u0001\u0000\u0000\u0000#\u008e\u0001\u0000\u0000\u0000"+
		"%\u0091\u0001\u0000\u0000\u0000\'\u0099\u0001\u0000\u0000\u0000)\u009b"+
		"\u0001\u0000\u0000\u0000+\u009d\u0001\u0000\u0000\u0000-\u009f\u0001\u0000"+
		"\u0000\u0000/\u00a1\u0001\u0000\u0000\u00001\u00a3\u0001\u0000\u0000\u0000"+
		"34\u0005G\u0000\u000045\u0005R\u0000\u000056\u0005A\u0000\u000067\u0005"+
		"M\u0000\u000078\u0005M\u0000\u000089\u0005A\u0000\u00009:\u0005R\u0000"+
		"\u0000:\u0002\u0001\u0000\u0000\u0000;<\u0005T\u0000\u0000<=\u0005O\u0000"+
		"\u0000=>\u0005K\u0000\u0000>?\u0005E\u0000\u0000?@\u0005N\u0000\u0000"+
		"@\u0004\u0001\u0000\u0000\u0000AB\u0005R\u0000\u0000BC\u0005U\u0000\u0000"+
		"CD\u0005L\u0000\u0000DE\u0005E\u0000\u0000E\u0006\u0001\u0000\u0000\u0000"+
		"FG\u0005I\u0000\u0000GH\u0005N\u0000\u0000HI\u0005I\u0000\u0000IJ\u0005"+
		"T\u0000\u0000J\b\u0001\u0000\u0000\u0000KL\u0005S\u0000\u0000LM\u0005"+
		"Y\u0000\u0000MN\u0005N\u0000\u0000NO\u0005T\u0000\u0000OP\u0005H\u0000"+
		"\u0000P\n\u0001\u0000\u0000\u0000QR\u0005{\u0000\u0000R\f\u0001\u0000"+
		"\u0000\u0000ST\u0005}\u0000\u0000T\u000e\u0001\u0000\u0000\u0000UW\u0007"+
		"\u0000\u0000\u0000VU\u0001\u0000\u0000\u0000WX\u0001\u0000\u0000\u0000"+
		"XV\u0001\u0000\u0000\u0000XY\u0001\u0000\u0000\u0000YZ\u0001\u0000\u0000"+
		"\u0000Z[\u0006\u0007\u0000\u0000[\u0010\u0001\u0000\u0000\u0000\\]\u0005"+
		";\u0000\u0000]\u0012\u0001\u0000\u0000\u0000^_\u0005=\u0000\u0000_\u0014"+
		"\u0001\u0000\u0000\u0000`b\u0007\u0001\u0000\u0000a`\u0001\u0000\u0000"+
		"\u0000bc\u0001\u0000\u0000\u0000ca\u0001\u0000\u0000\u0000cd\u0001\u0000"+
		"\u0000\u0000d\u0016\u0001\u0000\u0000\u0000eg\u0007\u0002\u0000\u0000"+
		"fe\u0001\u0000\u0000\u0000gh\u0001\u0000\u0000\u0000hf\u0001\u0000\u0000"+
		"\u0000hi\u0001\u0000\u0000\u0000i\u0018\u0001\u0000\u0000\u0000jn\u0007"+
		"\u0003\u0000\u0000km\u0007\u0004\u0000\u0000lk\u0001\u0000\u0000\u0000"+
		"mp\u0001\u0000\u0000\u0000nl\u0001\u0000\u0000\u0000no\u0001\u0000\u0000"+
		"\u0000o\u001a\u0001\u0000\u0000\u0000pn\u0001\u0000\u0000\u0000qs\u0005"+
		"<\u0000\u0000rt\t\u0000\u0000\u0000sr\u0001\u0000\u0000\u0000tu\u0001"+
		"\u0000\u0000\u0000uv\u0001\u0000\u0000\u0000us\u0001\u0000\u0000\u0000"+
		"vw\u0001\u0000\u0000\u0000wx\u0005>\u0000\u0000x\u001c\u0001\u0000\u0000"+
		"\u0000y{\u0005\"\u0000\u0000z|\t\u0000\u0000\u0000{z\u0001\u0000\u0000"+
		"\u0000|}\u0001\u0000\u0000\u0000}~\u0001\u0000\u0000\u0000}{\u0001\u0000"+
		"\u0000\u0000~\u007f\u0001\u0000\u0000\u0000\u007f\u0080\u0005\"\u0000"+
		"\u0000\u0080\u001e\u0001\u0000\u0000\u0000\u0081\u0082\u0005r\u0000\u0000"+
		"\u0082\u0083\u0005\"\u0000\u0000\u0083\u0085\u0001\u0000\u0000\u0000\u0084"+
		"\u0086\t\u0000\u0000\u0000\u0085\u0084\u0001\u0000\u0000\u0000\u0086\u0087"+
		"\u0001\u0000\u0000\u0000\u0087\u0088\u0001\u0000\u0000\u0000\u0087\u0085"+
		"\u0001\u0000\u0000\u0000\u0088\u0089\u0001\u0000\u0000\u0000\u0089\u008a"+
		"\u0005\"\u0000\u0000\u008a \u0001\u0000\u0000\u0000\u008b\u008c\u0005"+
		"=\u0000\u0000\u008c\u008d\u0005>\u0000\u0000\u008d\"\u0001\u0000\u0000"+
		"\u0000\u008e\u008f\u0005-\u0000\u0000\u008f\u0090\u0005>\u0000\u0000\u0090"+
		"$\u0001\u0000\u0000\u0000\u0091\u0093\u0005[\u0000\u0000\u0092\u0094\t"+
		"\u0000\u0000\u0000\u0093\u0092\u0001\u0000\u0000\u0000\u0094\u0095\u0001"+
		"\u0000\u0000\u0000\u0095\u0096\u0001\u0000\u0000\u0000\u0095\u0093\u0001"+
		"\u0000\u0000\u0000\u0096\u0097\u0001\u0000\u0000\u0000\u0097\u0098\u0005"+
		"]\u0000\u0000\u0098&\u0001\u0000\u0000\u0000\u0099\u009a\u0005,\u0000"+
		"\u0000\u009a(\u0001\u0000\u0000\u0000\u009b\u009c\u0005:\u0000\u0000\u009c"+
		"*\u0001\u0000\u0000\u0000\u009d\u009e\u0005(\u0000\u0000\u009e,\u0001"+
		"\u0000\u0000\u0000\u009f\u00a0\u0005)\u0000\u0000\u00a0.\u0001\u0000\u0000"+
		"\u0000\u00a1\u00a2\u0005.\u0000\u0000\u00a20\u0001\u0000\u0000\u0000\u00a3"+
		"\u00a4\u0005#\u0000\u0000\u00a4\u00a5\u0005E\u0000\u0000\u00a5\u00a6\u0005"+
		"M\u0000\u0000\u00a6\u00a7\u0005P\u0000\u0000\u00a7\u00a8\u0005T\u0000"+
		"\u0000\u00a8\u00a9\u0005Y\u0000\u0000\u00a92\u0001\u0000\u0000\u0000\t"+
		"\u0000Xchnu}\u0087\u0095\u0001\u0006\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}