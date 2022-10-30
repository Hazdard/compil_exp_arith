exception Error of string

type noeud_bin =
  | Plus
  | Moins
  | Prod
  | Plusf
  | Moinsf
  | Prodf
  | Div
  | Mod
  | Power

type noeud_una = Toint | Tofloat | Moinsu | Fact
type feuille = Int of int | Float of float | Var of string * int

type sexp =
  | Vide
  | Atom of feuille
  | Cons of noeud_bin * sexp * sexp
  | Unaire of noeud_una * sexp
  | Vardef of string * int * sexp * sexp
  | Retour of sexp * sexp

let rec appart x l =
  (* permet de tester si une variable est deja definie, est renvoie en seconde composante le type de la variable : 1 pour entier, 0 pour flottant, -1 si inconnu *)
  match l with
  | [] -> (false, -1)
  | a :: q when fst a = x -> (true, snd a)
  | _ :: q -> appart x q

let rec modif l cpl =
  (* permet de mettre a jour le type d'une variable *)
  match l with
  | [] -> []
  | a :: q when fst cpl = fst a -> cpl :: q
  | a :: q -> a :: modif q cpl

let attrib_var ast =
  (* va parcourir l'ast en attribuant les types a chaque variable, et eventuellement le met a jour si il change*)
  (*n'est utilise qu'a l'initialisation de l'arbre*)
  let rec aux ast liste =
    match ast with
    | Vardef (nom, valtype, sdef, suite) when not (fst (appart nom liste)) ->
        Vardef
          (nom, valtype, aux sdef liste, aux suite ((nom, valtype) :: liste))
    | Vardef (nom, valtype, sdef, suite) ->
        Vardef
          (nom, valtype, aux sdef liste, aux suite (modif liste (nom, valtype)))
    | Atom (Var (nom, valtype)) when fst (appart nom liste) ->
        let newvaltype = snd (appart nom liste) in
        Atom (Var (nom, newvaltype))
    | Atom (Var (nom, valtype)) -> failwith "Variable non definie"
    | Atom a -> Atom a
    | Cons (op, s1, s2) -> Cons (op, aux s1 liste, aux s2 liste)
    | Unaire (op, s) -> Unaire (op, aux s liste)
    | Retour (sretour, suite) -> Retour (aux sretour liste, aux suite liste)
    | Vide -> Vide
  in
  aux ast []

let rec numero x l =
  match l with
  | [] -> failwith "Pas dans la liste donc pas de numero"
  | a :: q when fst a = x -> 0
  | _ :: q -> 1 + numero x q

let bien_typee ast =
  let rec aux = function
    (* La deuxieme composante vaut 1 si l'ast a un type entier et 0 si il a le type flottant *)
    | Vide -> (true, -1)
    | Retour (sretour, suite) ->
        let aret, bret = aux sretour in
        let asuite, bsuite = aux suite in
        (aret && asuite, bret)
    | Atom (Int _) -> (true, 1)
    | Atom (Float _) -> (true, 0)
    | Vardef (_, valtype, s1, s2) ->
        let a1, b1 = aux s1 in
        let a2, b2 = aux s2 in
        (a1 && a2 && valtype = b1, b2)
    | Atom (Var (nom, valtype)) -> (true, valtype)
    | Unaire (Toint, s1) ->
        let a, b = aux s1 in
        (a && b = 0, 1)
    | Unaire (Tofloat, s1) ->
        let a, b = aux s1 in
        (a && b = 1, 0)
    | Unaire (Moinsu, s1) ->
        let a, b = aux s1 in
        (a, b)
    | Unaire (Fact, s) ->
        let a, b = aux s in
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
    | Cons (Power, s1, s2) ->
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

let rec afficher_sexp = function
  | Vide -> ()
  | Retour (s1, s2) ->
      afficher_sexp s1;
      print_string " \n";
      afficher_sexp s2
  | Unaire (Toint, exp) ->
      print_string " Toint (";
      afficher_sexp exp;
      print_char ')'
  | Unaire (Moinsu, exp) ->
      print_string " Moinsu (";
      afficher_sexp exp;
      print_char ')'
  | Unaire (Tofloat, exp) ->
      print_string " Tofloat (";
      afficher_sexp exp;
      print_char ')'
  | Unaire (Fact, exp) ->
      print_char '(';
      afficher_sexp exp;
      print_string " ! ) "
  | Vardef (nom, valtype, s1, s2) ->
      print_string (nom ^ " de type : ");
      print_int valtype;
      print_string " , vaut : ";
      afficher_sexp s1;
      print_string "\n";
      afficher_sexp s2
  | Atom (Int ent) ->
      print_string " Atom (";
      print_int ent;
      print_char ')'
  | Atom (Var (nom, valtype)) ->
      print_string "( ";
      print_string nom;
      print_string " , ";
      print_int valtype;
      print_string " )"
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
  | Cons (Power, s1, s2) ->
      print_string " (";
      print_string " ^ ,";
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