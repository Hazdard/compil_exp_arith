exception Error of string

type noeud_bin = Plus | Moins | Prod | Plusf | Moinsf | Prodf | Div | Mod
type noeud_una = Toint | Tofloat | Moinsu
type feuille = Int of int | Float of float

type sexp =
  | Atom of feuille
  | Cons of noeud_bin * sexp * sexp
  | Unaire of noeud_una * sexp

let rec afficher_sexp = function
  | Unaire (Toint, exp) ->
      print_string " Toint (";
      afficher_sexp exp;
      print_char ')'
  | Unaire (Moinsu, exp) ->
      print_string " - (";
      afficher_sexp exp;
      print_char ')'
  | Unaire (Tofloat, exp) ->
      print_string " Tofloat (";
      afficher_sexp exp;
      print_char ')'
  | Atom (Int ent) ->
      print_string " Atom (";
      print_int ent;
      print_char ')'
  | Atom (Float flott) ->
      print_string " Atom (";
      print_float flott;
      print_char ')'
  | Cons (Plus, s1, s2) ->
      print_string " Cons (";
      print_string " + ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Moins, s1, s2) ->
      print_string " Cons (";
      print_string " - ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Prod, s1, s2) ->
      print_string " Cons (";
      print_string " * ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Div, s1, s2) ->
      print_string " Cons (";
      print_string " / ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Mod, s1, s2) ->
      print_string " Cons (";
      print_string " % ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Plusf, s1, s2) ->
      print_string " Cons (";
      print_string " +. ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Moinsf, s1, s2) ->
      print_string " Cons (";
      print_string " -. ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'
  | Cons (Prodf, s1, s2) ->
      print_string " Cons (";
      print_string " *. ,";
      afficher_sexp s1;
      print_string " , ";
      afficher_sexp s2;
      print_char ')'

let bien_typee ast =
  let rec aux = function
    (* La deuxieme composante vaut 1 si l'ast a un type entier et 0 si il a le type flottant *)
    | Atom (Int _) -> (true, 1)
    | Atom (Float _) -> (true, 0)
    | Unaire (Toint, s1) ->
        let a, b = aux s1 in
        (a && b = 0, 1)
    | Unaire (Tofloat, s1) ->
        let a, b = aux s1 in
        (a && b = 1, 0)
    | Unaire (Moinsu, s1) ->
        let a, b = aux s1 in
        (a && b = 1, b)
    | Cons (Plus, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Cons (Plusf, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Cons (Prod, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Cons (Prodf, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Cons (Moins, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Cons (Moinsf, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 0, b1)
    | Cons (Mod, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
    | Cons (Div, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        ((a1 && a2) && b1 = b2 && b1 = 1, b1)
  in
  aux ast
