(* -- Fonctions utilitaires -- *)

(*Q1*)
(*mem prend en paramètre une liste l d'éléments de type a et un élément x
de type a. Retourne true si x est élément de l, false sinon*)
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
	| h::q -> if mem l1 h then union l1 q
				 else h :: (union l1 q);;
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
	| h::q -> if mem l1 h then  h :: (intersection l1 q)
				 else intersection l1 q;;
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


(* -- Langages locaux et linéaires -- *)

type 'a regexp = 
| Eps
| Letter of 'a
| Union of 'a regexp * 'a regexp
| Concat of 'a regexp * 'a regexp
| Star of 'a regexp;;
(*Q5*)
(*Récuperer de manière récursive le préfixe d'une expression régulière en la déconstruisant*)
let rec get_prefix (e: 'a regexp) : 'a list = 
	match e with
	| Eps -> []
	| Letter (a) -> [a]
	| Union (a, b) -> (get_prefix a) @ (get_prefix b)
	| Concat (a, b) -> (get_prefix a) @ (if a = Eps then get_prefix b else [])
	| Star (a) -> [] :: (get_prefix a);;
(*De même pour récupérer les suffixes*)
let rec get_suffix (e: 'a regexp) : 'a list =
	match e with
	| Eps -> []
	| Letter (a) -> [a]
	| Union (a, b) -> (get_suffix a) @ (get_suffix b)
	| Concat (a, b) -> (if b = Eps then get_suffix a else []) @ (get_suffix b)
	| Star (a) -> [] :: (get_suffix a);;
(*Facteurs?*)

(* -- Automates -- *)

type ('a, 'b) automaton = {
	initial: 'a list;
	accepting: 'a list;
	delta: (('a * 'b) * 'a) list
};;
(*Q8*)
(*Retourne la liste triée contenant les images de chaque élément de 'states' par la fonction delta.
Pour déterminer l'image d'un état 'state' et d'une lettre 'letter', chercher l'élément de delta de la forme
((state, letter),x) pour retourner x. Si cet élément n'est pas trouvé, alors delta n'est pas définie pour
les valeurs 'state' et 'letter'*)
let delta_set (aut: ('a, 'b) automaton) (states: 'a list) (letter: 'b) : 'a list =
	let delta = aut.delta in
	let delta_transition (state: 'a) : 'a = 
		match delta with
		| [] -> failwith "fonction non définie pour ces valeurs"
		| ((s1, l), s2)::q -> if s1 = state && l = letter then s2 else delta_transition q state letter
	in sort(map delta_transition states);;
(*test*)
let autom = {
	initial = [];
	accepting = [];
	delta = [((1, "a"), 3); ((2, "a"), 1); ((3, "a"), 2)]
} and
states = [1;2;3;3] and
letter = "a" in
delta_set autom states letter;;