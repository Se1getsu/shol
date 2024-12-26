lexer grammar SholLexer;
/*
                    *** NOTE ***
            When adding new terminators,
    modify nqs1_char & nqs2_char in SholParser.g4.
*/

IS    : ':';

MUL   : '*';
DIV   : '/';
MOD   : '%';

ADD   : '+';
SUB   : '-';

EQ    : '=';
NEQ   : '!=';
LT    : '<';
GT    : '>';
LEQ   : '<=';
GEQ   : '>=';

L_P   : '(';
R_P   : ')';
L_B   : '{';
R_B   : '}';
L_SB  : '[';
R_SB  : ']';

DOT   : '.';
PIPE  : '|';
COMMA : ',';
DOL   : '$';
HASH  : '#';

INT_T : 'int';
STR_T : 'str';

NL    : [\r\n;];

IDENT : ID_START ID_CONTINUE*;
N     : '0' | [1-9][0-9]*;
S     : '"' ( ~["\r\n] )* '"';

BLOCK_COMMENT : '/*' .*? '*/' -> channel(HIDDEN);
LINE_COMMENT : '//' ~[\r\n]* -> channel(HIDDEN);
SP    : [ \t];

UNKNOWN : . ;

fragment ID_START:
    '_'
    | [\p{L}]
;
fragment ID_CONTINUE:
    ID_START
    | [\p{Nd}]
;
