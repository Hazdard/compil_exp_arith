%token LPAREN RPAREN
%token EOL
%token <int> INT
%token <float> FLOAT
%token TOINT TOFLOAT PLUS PLUSF MINUS MINUSF TIMES TIMESF DIV MOD
%left PLUS PLUSF MINUS MINUSF      
%left TIMES TIMESF DIV MOD      
%start parse
%type <Asyntax.sexp> parse
%%
parse:  
  bob EOL  { $1 }
;
bob:
  INT  { Asyntax.Atom(Int($1))}
| FLOAT { Asyntax.Atom(Float($1))}
| TOFLOAT LPAREN bob RPAREN { Asyntax.Unaire (Tofloat,$3) }
| TOINT LPAREN bob RPAREN { Asyntax.Unaire (Toint,$3) }
| LPAREN bob RPAREN { $2 }
| bob PLUS bob { Asyntax.Cons (Plus,$1,$3) }
| bob PLUSF bob { Asyntax.Cons (Plusf,$1,$3) }
| bob MINUS bob { Asyntax.Cons (Moins,$1,$3) }
| bob MINUSF bob { Asyntax.Cons (Moinsf,$1,$3) }
| bob TIMES bob { Asyntax.Cons (Prod,$1,$3) }
| bob TIMESF bob { Asyntax.Cons (Prodf,$1,$3) }
| bob DIV bob { Asyntax.Cons (Div,$1,$3) }
| bob MOD bob { Asyntax.Cons (Mod,$1,$3) }
| MINUS LPAREN bob RPAREN { Asyntax.Unaire (Moinsu,$3) }
| PLUS LPAREN bob RPAREN { $3 }
;