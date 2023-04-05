(* EL MEJJAD Walid *)

#use "projet.ml";;

(*Creations de fonctions qui nous permettront de faire des tests sur les arbre d'expressions polonaise 1 fonction par opérateur*)

let additionner (tree1 : tree)(tree2 : tree) : tree =
  BINOP(PLUS,tree1,tree2)
;;

let soustraire (tree1 : tree)(tree2 : tree) : tree =
  BINOP(MOINS,tree1,tree2)
;;

let diviser (tree1 : tree)(tree2 : tree) : tree =
  BINOP(DIVISER,tree1,tree2)
;;

let multiplier (tree1 : tree)(tree2 : tree) : tree =
  BINOP(MULT,tree1,tree2)
;;

let unop (tree1 : tree) : tree =
  UNOP(OPPOSER,tree1)
;;


(* Test sur la fonction parse *)

let parse1 = parse(["17";"13";"+";"3";"*"]);;                (*                   17 + 13 * 3                  *)
let parse2 = parse(["10";"1";"-";"3";"/"]);;                 (*                   10 - 1 / 3                  *)
let parse3 = parse(["13";"2";"5";"*";"0";"1";"/";"-";"+"]);; (*                  13 + (2 × 5 − 0/1)                *)
let parse5 = parse(["x";"y";"*";"10";"+"]);;                 (*                   x*y+10                  *)
(*Resultats*)

let res_parse1 = BINOP (MULT, CONSTANTE 3, BINOP (PLUS, CONSTANTE 13, CONSTANTE 17));;
let res_parse2 = BINOP (DIVISER, CONSTANTE 3, BINOP (MOINS, CONSTANTE 1, CONSTANTE 10));;
let res_parse3 = BINOP (PLUS,BINOP (MOINS, BINOP (DIVISER, CONSTANTE 1, CONSTANTE 0), BINOP (MULT, CONSTANTE 5, CONSTANTE 2)),CONSTANTE 13);;
let res_parse4 = BINOP (PLUS, CONSTANTE 10, BINOP (MULT, VARIABLE "y", VARIABLE "x"));;

(* Comparaison *)
parse1 = res_parse1;;
parse2 = res_parse2;;
parse3 = res_parse3;;
parse4 = res_parse4;;


(* Cas des failwith *)
(*Pour manque d'operande*)
parse(["13";"2";"5";"*";"1";"0";"/";"-";"+";"+"]);;
(*Pour exces d'operande*)
parse(["13";"2";"5";"*";"1";"0";"/";"-"]);;
(*Pour liste vide*)
parse([]);;

(* Test de la fonction simplification *)

 (* on peut faire la meme avec multiplier ou additionner ou diviser ca marche tout aussi bien*)
let tree_simple : tree = multiplier(CONSTANTE(10)) (VARIABLE("x"));;
let zero : tree = CONSTANTE(0);;
let un : tree = CONSTANTE(1);;

(* Test des fonctions *)
(* On teste toutes les possibilités des règles mathématiques et on vérifie que l'on respecte bien les règles avec chaque opérateur*)
simplification(additionner(zero) (tree_simple) ) = tree_simple;;  (*           0+x = x           *)
simplification(soustraire(zero) (tree_simple) ) = unop tree_simple;; (*           0-x = -x           *)
simplification(diviser(zero) (tree_simple) ) = zero;;      (*           0/x = 0           *)
simplification(multiplier(zero) (tree_simple) ) = zero;;   (*           0*x = 0           *)
simplification(multiplier(un) (tree_simple) ) = tree_simple;; (*           1*x = x           *)
simplification(soustraire(tree_simple) (tree_simple) ) = zero;;   (*           x-x = 0            *)
simplification(diviser(tree_simple) (tree_simple) ) = un;; (*           x/x = 1           *)


