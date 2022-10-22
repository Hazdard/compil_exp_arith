open Asyntax
open Lexer
open Parser

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let read () = Parser.parse Lexer.token lexbuf in
  while true do
    try
      let sexp = read () in
      Asyntax.afficher_sexp sexp;
      flush stdout
    with
    | Lexer.Eof -> exit 0
  done
