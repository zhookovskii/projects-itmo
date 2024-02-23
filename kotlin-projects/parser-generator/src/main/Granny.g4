grammar Granny;

prog: GRAMMAR LCURL grammarBody RCURL ;

grammarBody: token synthesize? rule init ;

synthesize: SYNTH LCURL (attrDef+) RCURL ;

attrDef: TOKENNAME COLON TYPE DEFAULT SEMICOLON ;

token: TOKEN LCURL tokenBody RCURL ;

tokenBody: tokenDef+ ;

tokenDef: TOKENNAME EQUALS (STRING|REGEX) SEMICOLON ;

rule: RULE LCURL ruleBody RCURL ;

ruleBody: ruleDef+ ;

ruleDef: RULENAME parameters? ARROW (EMPTY (SLIM_ARROW BLOCK)?|ruleSequence) (SLIM_ARROW BLOCK)? SEMICOLON ;

parameters: LPAREN paramSequence RPAREN ;

paramSequence: TOKENNAME COLON TYPE (COMMA paramSequence)? ;

ruleSequence: (TOKENNAME|RULENAME) arguments? BLOCK? (COMMA ruleSequence)? ;

arguments: LPAREN argSequence RPAREN ;

argSequence: (TOKENNAME|RULENAME DOT TOKENNAME) (COMMA argSequence)? ;

init: INIT EQUALS RULENAME ;

GRAMMAR: 'GRAMMAR' ;
TOKEN: 'TOKEN' ;
RULE: 'RULE' ;
INIT: 'INIT' ;
SYNTH: 'SYNTH' ;
LCURL: '{' ;
RCURL: '}' ;
WS: [ \t\n\r]+ -> skip ;
SEMICOLON: ';' ;
EQUALS: '=' ;
RULENAME: [A-Z']+ ;
TOKENNAME: [a-z_]+ ;
TYPE: [A-Z][a-z]* ;
DEFAULT: '<'.+?'>' ;
STRING: '"'.+?'"' ;
REGEX: 'r"'.+?'"' ;
ARROW: '=>' ;
SLIM_ARROW: '->' ;
BLOCK: '['.+?']' ;
COMMA: ',' ;
COLON: ':' ;
LPAREN: '(' ;
RPAREN: ')' ;
DOT: '.' ;
EMPTY: '#EMPTY' ;