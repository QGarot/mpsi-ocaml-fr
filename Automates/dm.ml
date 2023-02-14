(* -- Fonctions utilitaires -- *)

(*Q1*)
(*mem prend en paramètre une list l d'éléments de type a et un élément x
de type a. Retourne true si x est élément de l.*)
let rec mem (l: 'a list) (x: 'a) : bool =
	match l with
	| [] -> false
	| h::q -> h = x || mem q x;;
(*Retourne l'union de deux listes (d'éléments tous différents), sans 
doublons. Si l'une d'entre elle (l2) est vide, renvoyer l'autre (l1).
Sinon, l2 possède une tête et une queue. Si la tête est élément de l1,
renvoyer récursivement l'union de l1 et de la queue. Sinon, renvoyer
l'union de l1 et de la queue en conservant la tête.*)
let rec union (l1: 'a list) (l2: 'a list) : 'a list =
	match l2 with
	| [] -> l1
	| h::q -> if mem l1 h then
					union l1 q
				else
					h :: (union l1 q);;
(*test*)
union [5;6] [7;8;5];;

(*Q2*)
(*Retourne l'intersection de deux listes.
Si une d'entre elle (l2) est vide, alors l'intersection des deux listes est
vide.
Sinon, décomposer l2 en une tête suivie d'une queue. Si la queue
est dans l'autre liste (l1), alors l'ajouter à l'intersection de l1
et de la queue de l2. Sinon renvoyer l'intersection de l1 et la queue
de l2*)
let rec intersection (l1: 'a list) (l2: 'a list) : 'a list =
	match l2 with
	| [] -> []
	| h::q -> if mem l1 h then
					h :: (intersection l1 q)
				else
					intersection l1 q;;
(*test*)
intersection [5;6;8] [7;8;5];;

(*Q3*)
(*Retourne une liste pour laquelle chaque élément d'indice i est
l'image de l'élément d'indice i de la liste l par la fonction f*)
let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
	match l with
	| [] -> []
	| h::q -> f h :: (map f q);;
(*Retourne une liste pour laquelle chaque élément d'indice i est un
couple de premier élément x et de second élément l'élément d'indice i
de la liste l*)
let get_list_of_pairs (x: 'b) (l: 'a list) : ('b * 'a) list =
	let pair (y: 'a) : ('b * 'a) = (x, y) in
	map pair l;;
(*Retourne le produit des listes l1 et l2.
Si l1 est vide, alors la liste des couples possibles est vide.
Sinon, on concatène la liste des couples formés à partir de la tête
et de l2, et du produit des listes q et l2*)
let rec product (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
	match l1 with
	| [] -> []
	| h::q -> (get_list_of_pairs h l2) @ product q l2;;
(*test*)
product [1;2] ['a';'b'];;

(*Q4*)
(*Retourne 0 si x = y, -1 si x < y, 1 si x > y*)
let compare (x: 'a) (y: 'a) : int =
	if x = y then 0
	else if x > y then 1
	else -1;;
let sort (l: 'a list) : 'a list = 
	List.sort compare l;;
(*test*)
sort [5;4;2;3;8;88];;