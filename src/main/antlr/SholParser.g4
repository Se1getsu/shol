parser grammar SholParser;
options { tokenVocab=SholLexer; }
program
    : l (declarations l)? EOF
    ;
declarations
    : decl_colony (lx decl_colony)*
    ;
decl_colony
    : L_SB s IDENT s R_SB (lx colony_stmts)?
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
    : flag (l prtr)*
    ;
prtr
    : producer
    | transformer
    ;
flag
    : DOT IDENT? sx conditions
    | PIPE IDENT? sx conditions
    ;
conditions
    : condition (s COMMA s condition)*
    ;
condition
    : expression
    ;
producer
    : (dest_c s)? PROD s expression (s COMMA s expression)*
    ;
transformer
    : (dest_c s)? TRNS s expression (s COMMA s expression)*
    ;
dest_c
    : colony_ref
    ;
colony_ref
    : HASH IDENT
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
    : factor (s op_mul s factor)*
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
    : nqs1_char ((nqs1_char|SP|DOT|PIPE)* (nqs1_char|DOT|PIPE))?
    ;
non_quoted_string2
    : (nqs2_char|DOT|PIPE) ((nqs2_char|SP|DOT|PIPE)* (nqs1_char|DOT|PIPE))?
    ;
// all terminators except NL, SP, DOT, PIPE
nqs1_char: MUL|DIV|MOD|ADD|SUB|EQ|NEQ|LT|GT|LEQ|GEQ|IS|L_P|R_P|L_B|R_B|L_SB|R_SB|TRNS|PROD|COMMA|DOL|HASH|INT_T|STR_T|IDENT|N|S|UNKNOWN;
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
