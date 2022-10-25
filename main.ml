open Asyntax
open Lexer
open Parser

  (*reparer syntaxe*)
  (*reparer le moins : pb avec les floats*)

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  if not (Asyntax.bien_typee ast) then failwith "Erreur de typage"
  (* else Asyntax.afficher_sexp ast; print_string "\n"*)
(*else let rec aux = function 
  |Asyntax.Cons(Plus,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Moins,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Prod,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Plusf,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Moinsf,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Prodf,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Div,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Mod,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Toint(s) ->(aux s)^
  |Asyntax.Tofloat(s) ->(aux s)^
  |Asyntax.Atom(Int(ent)) ->(aux s)^
  |Asyntax.Atom(Float(flot)) ->(aux s)^

  (* https://stackoverflow.com/questions/10161911/push-xmm-register-to-the-stack *)


let write_in file str = 
  let out_channel = open_out file in 
  output_string out_channel str

  *)

open Lexer 

