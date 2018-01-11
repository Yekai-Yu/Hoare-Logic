%{
    open Hlcommon
%}

/* Define the tokens of the language: */
%token <int> INT
%token <string> IDENT
%token TRUE FALSE AND NOT       /* B */
       LT GT LEQ GEQ EQUALS NEQ /* E */
       NEG PLUS MINUS TIMES DIV /* E */
       MOD REM QUOT ARROW
       SKIP SEMI ASSIGN IF THEN ELSE FI WHILE DO OD /* C */
       LPAREN RPAREN LBRACE RBRACE EOF

/* Define the precedence and associativity: */
%nonassoc SKIP
%right SEMI
%nonassoc IF THEN ELSE FI ASSIGN WHILE DO OD
%left AND
%left EQUALS NEQ LT GT LEQ GEQ ARROW
%left PLUS MINUS
%left TIMES DIV MOD REM QUOT
%left NOT NEG  /* not sure about this precedence, but associativity should be 'to left' */
%nonassoc LPAREN RPAREN


/*%start bool_expression exp_expression com_expression*/
%start condition_exp com_expression
%type <Hlcommon.condition_exp> condition_exp
%type <Hlcommon.command> com_expression

%%

condition_exp:
  | LBRACE bool_expression RBRACE               { Bool_exp($2) }
  | LBRACE exp_expression RBRACE                { Exp_exp($2) }
  | bool_expression ARROW bool_expression       { ImpExp($1,$3) }

/*  Condition - Pre & Post */
bool_expression:
  | exp_expression LT exp_expression       { LessExp($1,$3) }
  | exp_expression GT exp_expression       { GreaterExp($1,$3) }
  | exp_expression GEQ exp_expression      { GreaterEqExp($1,$3) }
  | exp_expression LEQ exp_expression      { LessEqExp($1,$3) }
  | exp_expression EQUALS exp_expression   { EqExp($1,$3) }
  | bool_expression AND bool_expression    { AndExp($1,$3) }
  | NOT bool_expression                    { NotExp($2) }
  | TRUE                                   { BoolConst true }
  | FALSE                                  { BoolConst false }
  | LPAREN bool_expression RPAREN          { $2 }

exp_expression:
  | INT                                    { IntConst($1) }
  | IDENT                                  { Ident($1) }
  | exp_expression PLUS exp_expression     { BinOpAppExp(IntPlusOp,$1,$3) }
  | exp_expression MINUS exp_expression    { BinOpAppExp(IntMinusOp,$1,$3) }
  | exp_expression TIMES exp_expression    { BinOpAppExp(IntTimesOp,$1,$3) }
  | exp_expression DIV exp_expression      { BinOpAppExp(IntDivOp,$1,$3) }
  | exp_expression MOD exp_expression      { BinOpAppExp(ModOp,$1,$3) }    
  | exp_expression REM exp_expression      { BinOpAppExp(RemOp,$1,$3) }     
  | exp_expression QUOT exp_expression     { BinOpAppExp(QuotOp,$1,$3) }    
  | NEG exp_expression                     { MonOpAppExp(IntNegOp,$2) }
  | LPAREN exp_expression RPAREN           { $2 }

/*  Command  */

com_expression:
  | SKIP 											 { SkipCommand }
  | com_expression SEMI com_expression               { SeqCommand($1,$3) }
  | IDENT ASSIGN exp_expression                      { AssignCommand($1,$3) }
  | IF bool_expression THEN com_expression ELSE com_expression FI { IfCommand($2,$4,$6) }
  | WHILE bool_expression DO com_expression OD       { WhileCommand($2,$4) }
