exception Error of string

type noeud = Plus | Moins | Prod | Plusf | Moinsf | Prodf | Div | Mod
type feuille = Nil | Int of int | Float of float
type sexp = Atom of feuille | Cons of noeud * sexp * sexp

let rec afficher_sexp = function
  | Atom Nil -> ()
  | Atom (Int ent) -> print_char(' ') ; print_int ent
  | Atom (Float flott) -> print_char(' ') ; print_float flott
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
