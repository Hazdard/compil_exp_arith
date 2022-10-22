open Asyntax
open Lexer
open Parser

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.parse Lexer.token lexbuf in
  Asyntax.afficher_sexp ast
