
{
  open Parser        
  exception Eof
  exception SyntaxError of string
}
rule token = parse
    [' ' '\t']     { token lexbuf }
  | ['\n' ]        { EOL }   
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| "+."           { PLUSF }
| '+'            { PLUS }
| "-."           { MINUSF }
| '-'            { MINUS }
| "*."           { TIMESF } 
| '*'            { TIMES }
| '/'            { DIV }
| '%'            { MOD }
| '('            { LPAREN }
| ')'            { RPAREN }
| _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof            { raise Eof }