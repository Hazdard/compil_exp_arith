open Asyntax
open Lexer
open Parser

let write_in file str =
  let out_channel = open_out file in
  output_string out_channel str

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  let check, est_entier = Asyntax.bien_typee ast in
  if not check then failwith "Erreur de typage"
    (* else Asyntax.afficher_sexp ast; print_string "\n" *)
  else
    let rec aux (ast, compteur) =
      match ast with
      (*Invariant : a la fin d'un appel, le resultat est seul dans la pile et la tete de la pile est au debut du resultat *)
      (*F0 est attribue a -1.0*)
      (*aux retourne (code qui calcule, code qui defini les flottants, indice du dernier flottant)*)
      | Asyntax.Unaire (Moinsu, s) when snd (Asyntax.bien_typee s) = 1 ->
          let a, b, nbf = aux (s, 0) in
          ( a ^ "popq %rdi \nmovq $0, %rsi \nsubq %rdi, %rsi \npushq %rsi \n",
            b,
            nbf )
      | Asyntax.Cons (Plus, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, 0) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \naddq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Moins, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, 0) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \nsubq %rsi, %rdi \npushq %rdi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Prod, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, 0) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "popq %rsi \npopq %rdi \nimulq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Div, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, 0) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "popq %rsi \n\
               popq %rax \n\
               movq $0, %rdx \n\
               idivq %rsi \n\
               pushq %rax \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Mod, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, 0) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "popq %rsi \n\
               popq %rax \n\
               movq $0, %rdx \n\
               idivq %rsi \n\
               pushq %rdx \n",
            b1 ^ b2,
            nbf1 )
      | Asyntax.Atom (Int ent) -> (("pushq $" ^ string_of_int ent) ^ "\n", "", 0)
      | Asyntax.Atom (Float flott) ->
          ( "movsd .F"
            ^ string_of_int (compteur + 1)
            ^ "(%rip), %xmm0 \nmovsd %xmmo, -8(%rsp) \nsubq $8, %rsp\n",
            (("\n.F" ^ string_of_int (compteur + 1)) ^ ":\n.double ")
            ^ string_of_float flott,
            compteur + 1 )
      | Asyntax.Unaire (Moinsu, s) ->
          let a, b, c = aux s in
          ( a
            ^ "movsd (%rsp), %xmm0 \n\
               movsd .F0(%rip), %xmm1 \n\
               mulsd %xmm1, %xmm0 \n\
               pushq %xmm0 \n",
            b,
            c )
      (* | Asyntax.Cons (Plusf, s1, s2) -> "aled"
         | Asyntax.Cons (Moinsf, s1, s2) -> "aled"
         | Asyntax.Cons (Prodf, s1, s2) -> "aled"
         | Asyntax.Unaire (Tofloat, ent) -> "aled"
         | Asyntax.Unaire (Toint, flott) -> "aled" *)
    in
    let code, var, _ = aux (ast, 0) in
    write_in "retour.s"
      (if est_entier = 1 then
       (".global main \n \nmain : \n" ^ code)
       ^ "movq $message, %rdi \n\
          popq %rsi \n\
          movq $0, %rax \n\
          call printf \n\
          ret \n\
         \ \n\
          .string \"%d \\n\" \n\
         \ \n\
          F0: \n\
          .double -1.0 \n\
          .data \n\
          message: \n\
          .string \"%d \\n\""
      else (".global main \n \nmain : \n" ^ code)
      ^ "movq $message, %rdi \n\
         popq %rsi \n\
         movq $0, %rax \n\
         call printf \n\
         ret \n\
        \ \n\
         .string \"%d \\n\" \n\
        \ \n\
         F0: \n\
         .double -1.0 \n\
         .data \n\
         message: \n\
         .string \"%d \\n\"")

(* https://stackoverflow.com/questions/10161911/push-xmm-register-to-the-stack *)
