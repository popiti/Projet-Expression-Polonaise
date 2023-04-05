(* EL MEJJAD Walid *)

(*Question 1*)

(*types*)

(*opérateurs respectifs :  addition soustraction division multiplication *)
type operation = PLUS | MOINS | DIVISER | MULT
;;

(* opérateur unaire pour nombre négatif *)
type opposer = OPPOSER
;;

type tree = CONSTANTE of int | VARIABLE of string | UNOP of opposer*tree | BINOP of operation*tree*tree
;;

(*fonctions de test des elements a l'interieur de la list passé en parametre*)

(*verifie si l'element est une variable, j'ai decide d'obliger à mettre une chaine de caractere de longueur 1 pour eviter que l'utilisateur 
   ne mette pas des abcds seul a par exemple est accepte *)
let is_var (elem : string) : bool =
    elem>="a"&& elem<="z" && String.length(elem) = 1
;;

(*verifie si l'element est un signe negatif *)
let is_Unop(elem : string) : bool =
  elem="~"
;;

(*verifie si l'element est un operateur *)
let is_Binop(elem : string ) : bool =
  elem = "+" || elem ="-" || elem = "*" || elem = "/"
;;

(*verifie si l'element est est une constante, j'avais fait une vérification, on est obligé de mettre en condition en plus qu'il ne doit pas
   prendre l'element vide sinon il retourne true*)
let is_constante (elem : string) : bool =
  (String.fold_left(fun acc var -> acc && (var>='0'&& var<='9')) true elem) && not(elem ="")
;;

(*fonctions qui permet de convertir les elements à l'intérieur de la liste*)

(*convertit l'element en operateur unaire*)
let signeOpposer(elem : string) : opposer = 
  match elem with 
  | "~" -> OPPOSER
  | _ -> failwith("ce n'est pas un opérateur unaire")
;;

(*convertit l'element en operateur binaire *)
let signeOperation(elem : string) : operation =
  match elem with 
  | "+" -> PLUS
  | "-" -> MOINS
  | "/" -> DIVISER
  | "*" -> MULT
  | _ -> failwith("ce n'est pas un opérateur binaire")
;;

(*On inverse la liste pour obtenir une pile qui sera lu par le parse*)
let inverserExpr (expr : string list) : string list = 
  List.fold_left(fun acc elem -> elem::acc  ) [] expr
;;

(*la fonction permet de disposer les elements de maniere logique et permet de savoir quel instruction est place dans l'arbre tree*)
let rec parse_aux(expr: string list ) : (string list * tree) = 
  match expr with
  |[] -> failwith "l'expression n'est pas cohérente manque d'operande" 
  | elem :: suiv -> 
                    if is_constante(elem) (*si c'est une constante on la convertit en constante*)
                    then (suiv,CONSTANTE(int_of_string(elem)))
                    else
                      if is_var(elem) (* idem quand c'est le cas d'une variable on la met en variable*)
                      then (suiv,VARIABLE(elem))
                      else
                          if is_Unop(elem) (*si c'est un unaire on prend l'element suivant qui devient lui le fils de l'operateur unaire*)
                          then let (expr_suiv,res) : string list * tree = parse_aux(suiv) in 
                               (expr_suiv,UNOP(signeOpposer(elem),res)) 
                          else
                            if is_Binop(elem) (*si c'est un operateur binaire on doit prendre en considération que c'est un cas d'arbre je m'explique : on prend les 2 elements suivants et on les place comme fils gauche et droit de ce meme operateur*)
                            then let(expr_suiv1,res1) : string list * tree = parse_aux(suiv) in
                                 let(expr_suiv2,res2) : string list * tree = parse_aux(expr_suiv1) in 
                                    (expr_suiv2,BINOP(signeOperation(elem),res1,res2))
                            else failwith "erreur l'expression qui a ete rentre ne peut pas etre evaluer" (*exception si les elements a l'interieur est impossible a evaluer c'est a dire que l'element n'est ni une variable ni une constante ni un operateur unaire ou binaire*)
;;

(*cette fonction lit la liste passe en paremetre et l'inverse avec la fonction iverserExpr pour build l'arbre*)
let parse (expr : string list) : tree = 
  match expr with 
  | [] -> failwith("La liste est vide impossible de l'evaluer")
  | _ -> let expr_inverser : string list = inverserExpr(expr) in 
         let (expr_suiv, res ) = parse_aux(expr_inverser) in 
         match expr_suiv with 
         | [] -> res
         | _  -> failwith "l'expression n'est pas cohérente exces d'operande"
;;

(*Question 2*)

(*fonctions qui met en place les regles mathématiques pour chaque operateur binaire et simplifie les arbres*)

(*simplifie l'operation unaire *)
let simple_opposer(tree1 : tree) : tree = 
    match tree1 with 
    | CONSTANTE(x) -> UNOP(OPPOSER,CONSTANTE(x))
    | _ -> UNOP(OPPOSER,tree1)
;;

(*simplifie l'operation des additions *)
let simple_plus(tree1 : tree)(tree2 : tree) : tree = 
    match (tree1,tree2) with 
    | ( CONSTANTE(x), CONSTANTE(y)) -> CONSTANTE(x+y)
    | ( _, CONSTANTE(0)) -> tree1
    | ( CONSTANTE(0), _ ) -> tree2
    | ( _, _) -> BINOP(PLUS, tree1, tree2)
;;

(*simplifie l'operation des soustractions *)
let simple_moins(tree1 : tree) (tree2 : tree) : tree =
    match (tree1,tree2) with 
    | ( CONSTANTE(x), CONSTANTE(y)) -> if x < y then UNOP(OPPOSER,CONSTANTE(-(x-y))) else CONSTANTE(x-y)
    | ( _, CONSTANTE(0)) -> tree1
    | ( CONSTANTE(0), _ ) -> UNOP(OPPOSER,tree2)
    | ( _, _) -> if tree1 = tree2 then CONSTANTE(0) else BINOP(MOINS, tree1, tree2)
;;

(*simplifie l'operation des divisions *)
let simple_diviser(tree1 : tree) (tree2 : tree) : tree =
    match (tree1,tree2) with 
    | ( _, CONSTANTE(0)) -> failwith("Interdit de diviser par 0")
    | ( CONSTANTE(x), CONSTANTE(y)) -> CONSTANTE(x/y)
    | ( CONSTANTE(0), _ ) -> CONSTANTE(0)
    | ( _, _) -> if tree1 = tree2 then CONSTANTE(1) else BINOP(DIVISER, tree1, tree2)
;;

(*simplifie l'operation des multiplications *)
let simple_mult(tree1 : tree) (tree2 : tree) : tree =
    match (tree1,tree2) with 
    | ( CONSTANTE(x), CONSTANTE(y)) -> CONSTANTE(x*y)
    | ( _, CONSTANTE(0)) -> CONSTANTE(0)
    | ( CONSTANTE(0), _ ) -> CONSTANTE(0)
    | ( _, CONSTANTE(1)) -> tree1
    | ( CONSTANTE(1), _ ) -> tree2
    | ( _, _) -> BINOP(MULT, tree1, tree2)
;;

let rec simplification (tr : tree) : tree = 
  match tr with 
  | CONSTANTE(x) -> CONSTANTE(x) (*comme pour la fonction parse si c'est des constantes on retourne la constante*)
  | VARIABLE(y) -> VARIABLE(y) (*idem on retourne la variable *)
  | UNOP(opposer,tree_) -> let abr_unop : tree = simplification(tree_) in (* on appelle a nouveau la fonction simplification pour simplifier l'expression de l'arbre   *)
                          (
                            match opposer with (* On appelle simple_opposer pour gerer l'operateur unaire *)
                          | OPPOSER -> simple_opposer abr_unop
                          )
  | BINOP(operateur,tree1,tree2) ->  let abr1_binop : tree = simplification(tree1) in 
                                     let abr2_binop : tree = simplification(tree2) in 
                                     (
                                      match operateur with (* On appelle chaque fonction pour chaque operateur contenu dans l'operation binaire qui lui correspond *)
                                     | PLUS    -> simple_plus abr1_binop abr2_binop
                                     | MOINS   -> simple_moins abr1_binop abr2_binop
                                     | DIVISER -> simple_diviser abr1_binop abr2_binop
                                     | MULT    -> simple_mult abr1_binop abr2_binop
                                     )
;;
