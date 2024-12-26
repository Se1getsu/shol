parser grammar SholParser;
options { tokenVocab=SholLexer; }
program
    : l (declarations l)? EOF
    ;
declarations
    : decl_colony (lx decl_colony)*
    ;
decl_colony
    : MUL IDENT (lx colony_stmts)?
    | MOD IDENT (lx colony_stmts)?
    ;
colony_stmts
    : colony_stmt (lx colony_stmt)*
    ;
colony_stmt
    : rule
    | resource // lowest priority (generates non_quoted_string1)
    ;
resource
    : N
    | S
    | non_quoted_string1 // lowest priority
    ;
rule
    : DOT sx conditions (l outputs)* (l parallel_rule)*
    ;
parallel_rule
    : PIPE sx conditions (l outputs)*
    ;
outputs
    : destination s expression (s COMMA s expression)*
    ;
conditions
    : condition (s COMMA s condition)*
    ;
condition
    : expression
    ;
destination
    : HASH IDENT?
    ;

expression
    : expr_comp
    ;
expr_comp
    : expr_add (s op_comp s expr_add)*
    ;
expr_add
    : expr_mul (s op_add s expr_mul)*
    ;
expr_mul
    : expr_is (s op_mul s expr_is)*
    ;
expr_is
    : factor (s IS s factor)?
    ;
factor
    : L_P s expression s R_P
    | type
    | capture
    | literal // lowest priority (generates non_quoted_string2)
    ;
literal
    : N
    | S
    | non_quoted_string2 // lowest priority
    ;
capture
    : DOL
    | DOL N
    ;
type
    : INT_T
    | STR_T
    ;

// String without ""
non_quoted_string1
    : nqs1_char ((nqs1_char|head_char|SP)* (nqs1_char|head_char))?
    ;
non_quoted_string2
    : nqs2_char+
    ;
// terminators with special meaning at the beginning of a line
head_char: DOT|PIPE|MUL|MOD;
// all terminators except NL, SP, head_char
nqs1_char: IS|DIV|ADD|SUB|EQ|NEQ|LT|GT|LEQ|GEQ|L_P|R_P|L_B|R_B|L_SB|R_SB|COMMA|DOL|HASH|INT_T|STR_T|IDENT|N|S|UNKNOWN;
// non-confusing terminators when used in flags and producers
nqs2_char: IDENT|N|S|UNKNOWN;

// Operators
op_comp: EQ | NEQ | LT | GT | LEQ | GEQ | IS ;
op_add: ADD | SUB ;
op_mul: MUL | DIV | MOD ;

s: SP* ;         // Spaces allowed
sx: SP+ ;        // Spaces required
l: (NL | SP)* ;  // New lines allowed
lx: s NL l ;     // New lines required
