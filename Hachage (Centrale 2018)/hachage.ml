(* Définition du type *)

type ('a, 'b) table_hachage = {
	hache: 'a -> int;
	donnees: ('a * 'b) list array;
	largeur: int
};;

(* Implantation de la structure dictionnaire *)

let creer (h : 'a -> int) (w : int) : ('a, 'b) table_hachage =
	{
	hache = h;
	donnees = Array.make w [];
	largeur = w
	} ;;

let recherche2 (t : ('a, 'b) table_hachage) (k : 'a) : bool =
	let rec r (l : ('a * 'b) list) (k : 'a) : bool =
		match l with
		| [] -> false
		| t::q -> fst t = k || r q k
	in r t.donnees.(t.hache k) k;;

let element (t : ('a, 'b) table_hachage) (k : 'a) : 'b =
	let rec el (l : ('a * 'b) list) (k : 'a) : bool =
		match l with
		| [] -> failwith "Non trouvé"
		| t::q -> if fst t = k then snd t else el q k
	in el t.donnees.(t.hache k) k;;
