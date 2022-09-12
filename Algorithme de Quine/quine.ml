type formula =
| Var of char
| Not of formula
| And of formula * formula
| Or of formula * formula;;

(* Q1 *)
let rec is_clause (f: formula) : bool =
	match f with
	| Var (_) -> true
	| Not (Var (_)) -> true
	| Or (f1, f2) -> is_clause f1 && is_clause f2
	| _ -> false;;
	
	
let formula = Not(Var('c')) in is_clause formula;;

let rec is_cnf (f: formula) : bool =
	match f with
	| And (f1, f2) -> is_cnf f1 && is_cnf f2
	| _ -> is_clause f;;
	
let formula = Not (And(Var('a'), Var('b'))) in is_cnf formula;;

type literal =
| LitVar of char
| LitNotVar of char

type clause = literal list
type cnf = clause list;;

(* Q2 *)

(* Q3 *)
let rec mem (x: 'a) (l: 'a list) : bool =
	match l with
	| [] -> false
	| h::q -> h = x || mem x q;;
	
(* Q4 *)
let rec map (f : 'a->'b) (l : 'a list) : 'b list =
	match l with
	| [] -> []
	| h::q -> f h :: map f q
;;

(* Q5 *)
let rec filter (p: 'a -> bool) (l: 'a list) : 'a list = 
	match l with
	| [] -> []
	| h::q -> if p h then h :: filter p q else filter p q;;
	
	
(* Q6 *
Quelle est la complexité de l'algo de Quine dans le pire des cas ?*)

(* Q7 *)
let rec quine (f : cnf) : bool =
	match f with
	| [] -> true
	| h::q -> (*todo*)
