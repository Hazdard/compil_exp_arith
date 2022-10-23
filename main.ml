open Asyntax
open Lexer
open Parser

let _ =
  let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in  (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  if not (Asyntax.bien_typee ast) then failwith "Erreur de typage"
  else Asyntax.afficher_sexp ast

 