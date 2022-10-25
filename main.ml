open Asyntax
open Lexer
open Parser

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  if not (Asyntax.bien_typee ast) then failwith "Erreur de typage"
  (*else Asyntax.afficher_sexp ast; print_string "\n" *)
else let rec aux = function 
  |Asyntax.Cons(Plus,s1,s2) ->((aux s1)^(aux s2))^("popq %rsi \n popq %rdi \n addq %rsi, %rdi \n pushq %rdi \n")
  |Asyntax.Cons(Moins,s1,s2) ->((aux s1)^(aux s2))^("popq %rsi \n popq %rdi \n subq %rsi, %rdi \n pushq %rdi \n")
  |Asyntax.Cons(Prod,s1,s2) ->((aux s1)^(aux s2))^("popq %rsi \n popq %rdi \n imultq %rsi, %rdi \n pushq %rdi \n")
  |Asyntax.Cons(Div,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Cons(Mod,s1,s2) ->((aux s1)^(aux s2))^("")
  |Asyntax.Atom(Int(ent)) ->("pushq $"^(string_of_int(ent)))^"\n"

(* METTRE RAX A 0 *)
(* https://stackoverflow.com/questions/10161911/push-xmm-register-to-the-stack *)


let write_in file str = 
  let out_channel = open_out file in 
  output_string out_channel str


