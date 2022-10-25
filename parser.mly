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
  sexp EOL  { $1 }
;
sexp:
  INT  { Asyntax.Atom(Int($1))}
| FLOAT { Asyntax.Atom(Float($1))}
| TOFLOAT LPAREN sexp RPAREN { Asyntax.Tofloat ($3) }
| TOINT LPAREN sexp RPAREN { Asyntax.Toint ($3) }
| LPAREN sexp RPAREN { $2 }
| sexp PLUS sexp { Asyntax.Cons (Plus,$1,$3) }
| sexp PLUSF sexp { Asyntax.Cons (Plusf,$1,$3) }
| sexp MINUS sexp { Asyntax.Cons (Moins,$1,$3) }
| sexp MINUSF sexp { Asyntax.Cons (Moinsf,$1,$3) }
| sexp TIMES sexp { Asyntax.Cons (Prod,$1,$3) }
| sexp TIMESF sexp { Asyntax.Cons (Prodf,$1,$3) }
| sexp DIV sexp { Asyntax.Cons (Div,$1,$3) }
| sexp MOD sexp { Asyntax.Cons (Mod,$1,$3) }
| MINUS LPAREN sexp RPAREN { Asyntax.Cons (Moins,Asyntax.Atom(Int(0)),$3) }
| MINUSF LPAREN sexp RPAREN { Asyntax.Cons (Moins,Asyntax.Atom(Float(0.)),$3) }
| MINUS INT { Asyntax.Cons (Moins,Asyntax.Atom(Int(0)),Asyntax.Atom(Int($2))) }
| MINUSF FLOAT { Asyntax.Cons (Moins,Asyntax.Atom(Float(0.)),Asyntax.Atom(Float($2))) }
| PLUS LPAREN sexp RPAREN { Asyntax.Cons (Moins,Asyntax.Atom(Int(0)),$3) }
| PLUSF LPAREN sexp RPAREN { Asyntax.Cons (Moins,Asyntax.Atom(Float(0.)),$3) }
| PLUS INT { Asyntax.Atom(Int($2)) }
| PLUSF FLOAT {Asyntax.Atom(Float($2)) }
;