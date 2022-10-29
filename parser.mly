%token LPAREN RPAREN EQUAL
%token EOL EOF
%token <string> NAME
%token <int> INT
%token <float> FLOAT
%token TOINT TOFLOAT PLUS PLUSF MINUS MINUSF TIMES TIMESF DIV MOD FACT POWER
%left PLUS PLUSF MINUS MINUSF      
%left TIMES TIMESF DIV MOD 
%left POWER
%nonassoc FACT     
%start parse
%type <Asyntax.sexp> parse
%%
parse :
  NAME EQUAL expr_int EOL parse {Asyntax.Vardef($1,1,$3,$5)}
| NAME EQUAL expr_float EOL parse {Asyntax.Vardef($1,0,$3,$5)}
| expr_int EOL { $1 }
| expr_float EOL { $1 }
| expr_int EOF { $1 }
| expr_float EOF { $1 }
;


expr_int:
  INT  { Asyntax.Atom(Int($1))}
| PLUS INT { Asyntax.Atom(Int($2)) }
| MINUS INT { Asyntax.Unaire (Moinsu,Asyntax.Atom(Int($2))) }
| NAME  { Asyntax.Atom(Var($1,-1))}
| PLUS LPAREN NAME RPAREN { Asyntax.Atom(Var($3,-1))}
| MINUS LPAREN NAME RPAREN { Asyntax.Unaire (Moinsu,Asyntax.Atom(Var($3,-1))) }
| expr_int FACT { Asyntax.Unaire(Fact,$1)}
| expr_int POWER expr_int { Asyntax.Cons(Power,$1,$3)}
| TOINT LPAREN expr_float RPAREN { Asyntax.Unaire (Toint,$3) }
| LPAREN expr_int RPAREN { $2 }
| expr_int PLUS expr_int { Asyntax.Cons (Plus,$1,$3) }
| expr_int MINUS expr_int { Asyntax.Cons (Moins,$1,$3) }
| expr_int TIMES expr_int { Asyntax.Cons (Prod,$1,$3) }
| expr_int DIV expr_int { Asyntax.Cons (Div,$1,$3) }
| expr_int MOD expr_int { Asyntax.Cons (Mod,$1,$3) }
| MINUS LPAREN expr_int RPAREN { Asyntax.Unaire (Moinsu,$3) }
| PLUS LPAREN expr_int RPAREN { $3 }
;

expr_float:
  FLOAT { Asyntax.Atom(Float($1))}
| PLUS FLOAT { Asyntax.Atom(Float($2)) }
| MINUS FLOAT { Asyntax.Unaire (Moinsu,Asyntax.Atom(Float($2))) }
| NAME  { Asyntax.Atom(Var($1,-1))}
| PLUS LPAREN NAME RPAREN { Asyntax.Atom(Var($3,-1))}
| MINUS LPAREN NAME RPAREN { Asyntax.Unaire (Moinsu,Asyntax.Atom(Var($3,-1))) }
| expr_float PLUSF expr_float { Asyntax.Cons (Plusf,$1,$3) }
| TOFLOAT LPAREN expr_int RPAREN { Asyntax.Unaire (Tofloat,$3) }
| LPAREN expr_float RPAREN { $2 }
| expr_float MINUSF expr_float { Asyntax.Cons (Moinsf,$1,$3) }
| expr_float TIMESF expr_float { Asyntax.Cons (Prodf,$1,$3) }
| MINUS LPAREN expr_float RPAREN { Asyntax.Unaire (Moinsu,$3) }
| PLUS LPAREN expr_float RPAREN { $3 }
;