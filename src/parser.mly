%{
open Ast
%}

%token <float> FLOAT
%token <string> VAR
%token TIMES  
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token EOF
%token DERIV
%token POW
%token DIV
%token SIN
%token TAN
%token COS
%token LOGB
%token FACT
%token SUB


%nonassoc IN
%left PLUS SUB
%left TIMES DIV
%left FACT

%left POW
%nonassoc LOGB
%nonassoc SIN COS TAN

%left DERIV

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = FLOAT { Float i }
	| x = VAR { Var x }
	| e1 = expr; SUB; e2 = expr {Binop (Add, e1, Binop (Mult, e2, Float ~-.1.))}
 	| SUB; e1 = expr {Binop (Mult, e1, Float ~-.1.)}
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; POW; e2 = expr { Binop (Pow, e1, e2) }	
	| LOGB; e1 = expr; e2 = expr { Binop (LogBase, e1, e2) }	
	| LET; x = VAR; EQUALS; e1 = expr; IN ; e2 = expr { Let (x, e1, e2) }
	| LPAREN; e=expr; RPAREN {e} 
	| e1 = expr; DERIV {Deriv e1}
	| SIN; e1 = expr  {Sin e1}
	| COS; e1 = expr {Cos e1}
	| TAN; e1 = expr {Tan e1}
	| e1 = expr; FACT {Fact e1}
	;
	
