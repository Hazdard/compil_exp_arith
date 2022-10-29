open Asyntax
open Lexer
open Parser

(* FAIRE UN DOSSIER TEST ; Probleme : corriger la div ; Detailler implementation puissance et fact *)
(* seg fault sur x=1.0 ; y=2.0 +. x ; x*)

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
  let ast_nontypee = Parser.parse Lexer.token lexbuf in
  let ast = Asyntax.attrib_var ast_nontypee in
  let check, est_entier = Asyntax.bien_typee ast in
  if not check then failwith "Erreur de typage"
    (* else Asyntax.afficher_sexp ast; print_string "\n" *)
  else
    let rec aux (ast, compteur, lvar) =
      match ast with
      (*Invariant : a la fin d'un appel, le resultat est seul dans la pile et la tete de la pile est au debut du resultat *)
      (*F0 est attribue a -1.0*)
      (*aux retourne (code qui calcule, code qui defini les flottants, indice du dernier flottant)*)
      | Vardef (nom, valtype, sdef, suite) ->
          let adef, bdef, nbfdef = aux (sdef, compteur, lvar) in
          let dejavu, numero = Asyntax.appart nom lvar in
          let n = List.length lvar in
          let ainter, binter, newlvar, newcompteur =
            if dejavu then
              if valtype = 1 then
                ( ((adef ^ "popq %rdi \nmovq %rdi, .X") ^ string_of_int numero)
                  ^ "(%rip) \n",
                  bdef,
                  lvar,
                  compteur )
              else
                ( ((adef ^ "movq (%rsp), %xmm0 \naddq $8, %rsp \nmovsd %xmm0, .X"
                   )
                  ^ string_of_int numero)
                  ^ "(%rip) \n",
                  bdef,
                  lvar,
                  compteur )
            else if valtype = 1 then
              ( ((adef ^ "popq %rdi \nmovq %rdi, .X") ^ string_of_int n)
                ^ "(%rip) \n",
                ((bdef ^ ".X") ^ string_of_int n) ^ ": \n.int 42 \n",
                (nom, valtype) :: lvar,
                compteur )
            else
              ( ((adef ^ "movq (%rsp), %xmm0 \nmovsd %xmm0, .X")
                ^ string_of_int n)
                ^ "(%rip) \naddq $8, %rsp \n",
                ((bdef ^ ".X") ^ string_of_int n)
                ^ ": \n.double 0.31415926535 \n",
                (nom, valtype) :: lvar,
                compteur )
          in
          let asuite, bsuite, nbfsuite = aux (suite, nbfdef, newlvar) in
          (ainter ^ asuite, binter ^ bsuite, nbfsuite)
      | Atom (Var (nom, vartype)) ->
          if vartype = 1 then
            ( ("movq .X" ^ string_of_int (Asyntax.numero nom (List.rev lvar)))
              ^ "(%rip), %rdi \npushq %rdi \n",
              "",
              compteur )
          else
            ( ("movsd .X" ^ string_of_int (Asyntax.numero nom (List.rev lvar)))
              ^ "(%rip), %xmm0 \nsubq $8, %rsp \nmovq %xmm0, (%rsp) \n",
              "",
              compteur )
      | Asyntax.Unaire (Moinsu, s) when snd (Asyntax.bien_typee s) = 1 ->
          let a, b, nbf = aux (s, compteur, lvar) in
          ( a ^ "popq %rdi \nmovq $0, %rsi \nsubq %rdi, %rsi \npushq %rsi \n",
            b,
            nbf )
      | Asyntax.Cons (Plus, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \naddq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Unaire (Fact, s) ->
          let a, b, nbf = aux (s, compteur, lvar) in
          ( a ^ "popq %rdi \nmovq $1, %rsi \ncall factorielle \npushq %rax \n",
            b,
            nbf )
      | Asyntax.Cons (Moins, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \nsubq %rsi, %rdi \npushq %rdi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Prod, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "popq %rsi \npopq %rdi \nimulq %rdi, %rsi \npushq %rsi \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Power, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "popq %rsi \n\
               popq %rdi \n\
               movq $1, %rdx \n\
               call puissance \n\
               pushq %rax \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Div, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "popq %rsi \npopq %rax \ncqto \nidivq %rsi \npushq %rax \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Mod, s1, s2) ->
          (* si de signes differents, il faut ajouter le diviseur au reste *)
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2) ^ "popq %rsi \npopq %rdi \ncall modulo \npushq %rax \n",
            b1 ^ b2,
            nbf1 )
      | Asyntax.Atom (Int ent) ->
          (("pushq $" ^ string_of_int ent) ^ "\n", "", compteur)
      | Asyntax.Atom (Float flott) ->
          ( "movsd .F"
            ^ string_of_int (compteur + 1)
            ^ "(%rip), %xmm0 \nsubq $8, %rsp \nmovsd %xmm0, (%rsp) \n",
            (("\n.F" ^ string_of_int (compteur + 1)) ^ ":\n.double ")
            ^ string_of_float flott ^ " \n",
            compteur + 1 )
      | Asyntax.Unaire (Moinsu, s) ->
          let a, b, c = aux (s, compteur, lvar) in
          ( a
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd .F0(%rip), %xmm1 \n\
               mulsd %xmm1, %xmm0 \n\
               subq $8, %rsp \n\
               movq %xmm0, (%rsp) \n",
            b,
            c )
      | Asyntax.Cons (Plusf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               addsd %xmm1, %xmm0 \n\
               subq $8, %rsp \n\
               movq %xmm0, (%rsp) \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Moinsf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               subsd %xmm0, %xmm1 \n\
               subq $8, %rsp \n\
               movq %xmm1, (%rsp) \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Cons (Prodf, s1, s2) ->
          let a1, b1, nbf1 = aux (s1, compteur, lvar) in
          let a2, b2, nbf2 = aux (s2, nbf1, lvar) in
          ( (a1 ^ a2)
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               movsd (%rsp), %xmm1 \n\
               addq $8, %rsp\n\
               mulsd %xmm1, %xmm0 \n\
               subq $8, %rsp \n\
               movq %xmm0, (%rsp) \n",
            b1 ^ b2,
            nbf2 )
      | Asyntax.Unaire (Tofloat, s) ->
          let a, b, nbf = aux (s, compteur, lvar) in
          ( a
            ^ "popq %rdi \n\
               cvtsi2sdq %rdi, %xmm0 \n\
               subq $8, %rsp \n\
               movq %xmm0, (%rsp) \n",
            b,
            nbf )
      | Asyntax.Unaire (Toint, s) ->
          let a, b, nbf = aux (s, compteur, lvar) in
          ( a
            ^ "movsd (%rsp), %xmm0 \n\
               addq $8, %rsp \n\
               cvttsd2siq %xmm0, %rdi \n\
               push %rdi \n",
            b,
            nbf )
    in
    let data =
      "\n\
      \ \n\
       print_float : \n\
       movq %rsp, %rbp \n\
       movq %rdi, %xmm0 \n\
       movl $convfloat, %edi \n\
       movl $1, %eax \n\
       call printf \n\
       ret \n\
      \ \n\
       modulo : \n\
       movq %rdi, %rax \n\
       cqto \n\
       idivq %rsi \n\
       imulq %rsi, %rdi \n\
       movq $0, %rcx \n\
       cmp %rdi, %rcx \n\
       js rien \n\
       addq %rsi, %rdx \n\
       movq %rdx, %rax \n\
       ret \n\
      \ \n\
       rien : \n\
       movq %rdx, %rax \n\
       ret \n\
      \ \n\
       factorielle : \n\
       movq $2, %rcx \n\
       movq %rdi, %rdx \n\
       cmp %rcx, %rdx \n\
       js retour \n\
       imulq %rdi, %rsi \n\
       addq $-1, %rdi \n\
       jmp factorielle \n\
      \ \n\
       retour: \n\
       movq %rsi, %rax \n\
       ret \n\
      \ \n\
       puissance : \n\
       movq $1, %rcx \n\
       movq %rsi, %r8 \n\
       cmp %rcx, %r8 \n\
       js retour_pui \n\
       imulq %rdi, %rdx \n\
       addq $-1, %rsi \n\
       jmp puissance \n\
      \ \n\
       retour_pui: \n\
       movq %rdx, %rax \n\
       ret \n\
      \ \n\
       .data \n\
       message :\n\
       .string \"%d \\n\" \n\
       convfloat : \n\
       .string \"%g \\n\"\n"
    in
    let code, var, _ = aux (ast, 0, []) in
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
          .double -1.0 \n")
       ^ data ^ var
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
           .double -1.0" ^ data ^ var)
