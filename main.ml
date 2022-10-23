open Asyntax
open Lexer
open Parser

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.parse Lexer.token lexbuf in
  if not (Asyntax.bien_typee ast) then failwith "Erreur de typage"
  else Asyntax.afficher_sexp ast

(* doit encore soccuper de la conversion flottants <-> entiers ; et de faire respecter le typage *)
