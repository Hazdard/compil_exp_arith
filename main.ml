open Asyntax
open Lexer
open Parser

(* reparer le cas MINUS LPAREN bob RPAREN si bob est flottant *)

let write_in file str =
  let out_channel = open_out file in
  output_string out_channel str

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  let check, _ = Asyntax.bien_typee ast in
  if not check then failwith "Erreur de typage"
    (* else Asyntax.afficher_sexp ast; print_string "\n" *)
  else
    let data_float = ref "" in
    let rec aux = function
      | Asyntax.Unaire (Moinsu, s1) when snd (Asyntax.bien_typee s1) = 1 ->
          aux s1 ^ "popq %rdi \nmovq $0, %rsi \nsubq %rdi, %rsi \npushq %rsi \n"
      | Asyntax.Cons (Plus, s1, s2) ->
          (aux s1 ^ aux s2)
          ^ "popq %rsi \npopq %rdi \naddq %rdi, %rsi \npushq %rsi \n"
      | Asyntax.Cons (Moins, s1, s2) ->
          (aux s1 ^ aux s2)
          ^ "popq %rsi \npopq %rdi \nsubq %rsi, %rdi \npushq %rdi \n"
      | Asyntax.Cons (Prod, s1, s2) ->
          (aux s1 ^ aux s2)
          ^ "popq %rsi \npopq %rdi \nimulq %rdi, %rsi \npushq %rsi \n"
      | Asyntax.Cons (Div, s1, s2) ->
          (aux s1 ^ aux s2)
          ^ "popq %rsi \npopq %rax \nmovq $0, %rdx \nidivq %rsi \npushq %rax \n"
      | Asyntax.Cons (Mod, s1, s2) ->
          (aux s1 ^ aux s2)
          ^ "popq %rsi \npopq %rax \nmovq $0, %rdx \nidivq %rsi \npushq %rdx \n"
      | Asyntax.Atom (Int ent) -> ("pushq $" ^ string_of_int ent) ^ "\n"
    in
    write_in "retour.s"
      ((".global main \n \nmain : \n" ^ aux ast)
      ^ "movq $message, %rdi \n\
         popq %rsi \n\
         movq $0, %rax \n\
         call printf \n\
         ret \n\
        \ \n\
         .data \n\
         message: \n\
         .string \"%d \\n\"")
(* METTRE RAX A 0 juste avant de call printf *)
(* https://stackoverflow.com/questions/10161911/push-xmm-register-to-the-stack *)
