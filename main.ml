open Asyntax
open Lexer
open Parser

(* CORRIGER LES IDIVQ NEGATIFS 0xffffffffffffffff dans rdx si strictement negatif et 0 sinon*)
(*jne : not equal ; jz : equal zero ; jnz : not equal zero *)

let write_in file str =
  let out_channel = open_out file in
  output_string out_channel str

let nom str =
  let n = String.length str in
  if n < 5 then failwith "Nom de fichier problematique"
  else if String.sub str (n - 4) 4 <> ".exp" then failwith "Mauvaise extension"
  else String.sub str 0 (n - 3) ^ "s"

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
          let a, b, nbf = aux (s, compteur) in
          ( a ^ "popq %rdi \nmovq $0, %rsi \nsubq %rdi, %rsi \npushq %rsi \n",
            b,
            nbf )
      | Asyntax.Cons (Plus, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \naddq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Moins, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \nsubq %rsi, %rdi \npushq %rdi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Prod, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "popq %rsi \npopq %rdi \nimulq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Div, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2) (*corr_div : si la value de %rax sauvee dans %rcx est negative, alors on ajoute 1*signe(%rsi) *)
            ^ "popq %rsi \n\
               popq %rax \n\
               cqto \n\
               idivq %rsi \n\
               pushq %rax \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Mod, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "popq %rsi \n\
               popq %rax \n\
               cqto \n\
               idivq %rsi \n\
               pushq %rdx \n",
            b1 ^ b2,
            nbf1 )
      | Asyntax.Atom (Int ent) ->
          (("pushq $" ^ string_of_int ent) ^ "\n", "", compteur)
      | Asyntax.Atom (Float flott) ->
          ( "movsd .F"
            ^ string_of_int (compteur + 1)
            ^ "(%rip), %xmm0 \nmovsd %xmm0, -8(%rsp) \nsubq $8, %rsp\n",
            (("\n.F" ^ string_of_int (compteur + 1)) ^ ":\n.double ")
            ^ string_of_float flott,
            compteur + 1 )
      | Asyntax.Unaire (Moinsu, s) ->
          let a, b, c = aux (s, compteur) in
          ( a
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd .F0(%rip), %xmm1 \n\
               mulsd %xmm1, %xmm0 \n\
               movq %xmm0, -8(%rsp) \n\
               subq $8, %rsp\n\
              \ ",
            b,
            c )
      | Asyntax.Cons (Plusf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               addsd %xmm1, %xmm0 \n\
               movq %xmm0, -8(%rsp) \n\
               subq $8, %rsp\n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Moinsf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               subsd %xmm0, %xmm1 \n\
               movq %xmm1, -8(%rsp) \n\
               subq $8, %rsp\n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Prodf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur) in
          let a2, b2, nbf2 = aux (s2, nbf1) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               mulsd %xmm1, %xmm0 \n\
               movq %xmm0, -8(%rsp) \n\
               subq $8, %rsp\n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Unaire (Tofloat, s) ->
          let a, b, nbf = aux (s, compteur) in
          ( a
            ^ "popq %rdi \n\
               cvtsi2sdq %rdi, %xmm0 \n\
               movq %xmm0, -8(%rsp) \n\
               subq $8, %rsp\n",
            b,
            nbf )
      | Asyntax.Unaire (Toint, s) ->
          let a, b, nbf = aux (s, compteur) in
          ( a
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               cvttsd2siq %xmm0, %rdi \n\
               push %rdi \n",
            b,
            nbf )
    in
    let code, var, _ = aux (ast, 0) in
    write_in
      (nom Sys.argv.(1))
      (if est_entier = 1 then
       ((".global main \n \nmain : \n" ^ code)
       ^ "movq $message, %rdi \n\
          popq %rsi \n\
          movq $0, %rax \n\
          call printf \n\
          ret \n\
         \ \n\
          .F0: \n\
          .double -1.0")
       ^ var ^ "\n \n.data \nmessage: \n.string \"%d \\n\" \n"
      else
        (".global main \n \nmain : \n" ^ code)
        ^ "movq (%rsp), %xmm0 \n\
           movq $1, %rax \n\
           addq $8, %rsp\n\
           movq %xmm0, %rdi \n\
           call print_float \n\
           ret \n\
          \ \n\
           .F0: \n\
           .double -1.0" ^ var
        ^ "\n\
          \ \n\
           print_float : \n\
           movq %rsp, %rbp \n\
           movq %rdi, %xmm0 \n\
           movl $convfloat, %edi \n\
           movl $1, %eax \n\
           call printf \n\
          \ \n\
           .data \n\
           convfloat : \n\
           .string \"%g \\n\"\n")
