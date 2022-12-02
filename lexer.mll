{
  open Parser
  exception SyntaxError of string
}

rule token = parse
 [' ' '\t']      { token lexbuf }
|['\n']         { EOL }   
|['0'-'9']+ as lxm { INT(int_of_string lxm) }
|['0'-'9']+ '.' ['0'-'9']* as lxm { FLOAT(float_of_string lxm) }
|"float"         {TOFLOAT}
|"int"           {TOINT}
|['A'-'z']+ as nom_var { NAME(nom_var) }  
| "+."           { PLUSF }
| '+'            { PLUS }
| "-."           { MINUSF }
| '-'            { MINUS }
| "*."           { TIMESF } 
| '*'            { TIMES }
| '/'            { DIV }
| '%'            { MOD }
| '!'            { FACT }
| "**"           { POWER }
| '='            { EQUAL }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { EOF }
| _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }