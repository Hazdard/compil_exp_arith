%token LPAREN RPAREN
%token EOL EOF
%token <int> INT
%token <float> FLOAT
%token TOINT TOFLOAT PLUS PLUSF MINUS MINUSF TIMES TIMESF DIV MOD FACT
%left PLUS PLUSF MINUS MINUSF      
%left TIMES TIMESF DIV MOD 
%nonassoc FACT     
%start parse
%type <Asyntax.sexp> parse
%%
parse:  
  bob EOF  { $1 }
;

bob:
  expr EOF { $1 }
| expr EOL { $1 } 
;

expr:
  INT  { Asyntax.Atom(Int($1))}
| FLOAT { Asyntax.Atom(Float($1))}
| expr FACT { Asyntax.Unaire(Fact,$1)}
| TOFLOAT LPAREN expr RPAREN { Asyntax.Unaire (Tofloat,$3) }
| TOINT LPAREN expr RPAREN { Asyntax.Unaire (Toint,$3) }
| LPAREN expr RPAREN { $2 }
| expr PLUS expr { Asyntax.Cons (Plus,$1,$3) }
| expr PLUSF expr { Asyntax.Cons (Plusf,$1,$3) }
| expr MINUS expr { Asyntax.Cons (Moins,$1,$3) }
| expr MINUSF expr { Asyntax.Cons (Moinsf,$1,$3) }
| expr TIMES expr { Asyntax.Cons (Prod,$1,$3) }
| expr TIMESF expr { Asyntax.Cons (Prodf,$1,$3) }
| expr DIV expr { Asyntax.Cons (Div,$1,$3) }
| expr MOD expr { Asyntax.Cons (Mod,$1,$3) }
| MINUS LPAREN expr RPAREN { Asyntax.Unaire (Moinsu,$3) }
| PLUS LPAREN expr RPAREN { $3 }
| MINUS INT { Asyntax.Unaire (Moinsu,Asyntax.Atom(Int($2))) }
| MINUS FLOAT { Asyntax.Unaire (Moinsu,Asyntax.Atom(Float($2))) }
| MINUS LPAREN bob RPAREN { Asyntax.Unaire (Moinsu,$3) }
| PLUS INT { Asyntax.Atom(Int($2)) }
| PLUS FLOAT { Asyntax.Atom(Float($2)) }
;