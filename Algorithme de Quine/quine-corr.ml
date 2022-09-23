type formula =
  | Var of char
  | Not of formula
  | And of formula * formula
  | Or of formula * formula

let rec is_clause (f : formula) : bool =
  match f with
  (* literals case *)
  | Var _ | Not (Var _) -> true
  | Or (f, g) -> is_clause f && is_clause g
  | _ -> false

let rec is_cnf (f : formula) : bool =
  match f with
  | And (f, g) -> is_cnf f && is_cnf g
  | _ -> is_clause f

type literal =
  | LitVar of char
  | LitNotVar of char

type clause = literal list

type cnf = clause list

let cnf_of_formula (f : formula) : cnf =
  let rec aux_clause (f : formula) (acc : clause) : clause =
    match f with
    | Var x -> LitVar x :: acc
    | Not (Var x) -> LitNotVar x :: acc
    | Or (f, g) -> aux_clause g (aux_clause f acc)
    | _ -> failwith "Not a clause"
  in
  let rec aux_cnf (f : formula) (acc : cnf) : cnf =
    match f with
    | And (f, g) -> aux_cnf g (aux_cnf f acc)
    | _ -> aux_clause f [] :: acc
  in
  aux_cnf f []

let rec mem (x : 'a) (l : 'a list) : bool =
  match l with
  | [] -> false
  | y :: q -> x = y || mem x q

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | t :: q -> f t :: map f q

let rec filter (p : 'a -> bool) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | t :: q when p t -> t :: filter p q
  | _ :: q -> filter p q

let neg_lit (l : literal) : literal =
  match l with
  | LitVar x -> LitNotVar x
  | LitNotVar x -> LitVar x

let rec quine (f : cnf) : bool =
  match f with
  | [] -> true
  | _ when mem [] f -> false
  | c :: _ ->
     let l = List.hd c in
     (* l is true *)
     quine (map (filter (fun l' -> l' <> neg_lit l))
              (filter (fun c -> not (mem l c)) f))
     ||
       (* l is false *)
       quine (map (filter (fun l' -> l' <> l))
              (filter (fun c -> not (mem (neg_lit l) c)) f))
(* complexité exponentielle *)

let val_lit (l : literal) : char * bool =
  match l with
  | LitVar x -> (x, true)
  | LitNotVar x -> (x, false)

let quine_model (f : cnf) : (char * bool) list option =
  let rec aux (f : cnf) (acc : (char * bool) list) : (char * bool) list option =
    match f with
    | [] -> Some acc
    | _ when mem [] f -> None
    | c :: _ ->
       let l = List.hd c in
       match aux
               (map (filter (fun l' -> l' <> neg_lit l))
                  (filter (fun c -> not (mem l c)) f))
               (val_lit l :: acc) with
       | Some v -> Some v
       | None ->
          aux
            (map (filter (fun l' -> l' <> l))
               (filter (fun c -> not (mem (neg_lit l) c)) f))
            (val_lit (neg_lit l) :: acc)
  in
  aux f []

let sat : formula =
  And (Or (Var 'x', Var 'y'),
        And (Or (Var 'x', Or (Not (Var 'y'), Var 'z')),
             And (Or (Not (Var 'x'), Or (Var 'y', Var 't')),
                  Or (Not (Var 'z'), Not (Var 't'))
               )
          )
    )
let _ : bool = is_cnf sat
let sat_cnf : cnf = cnf_of_formula sat
let _ : bool = quine sat_cnf
let _ : (char * bool) list option = quine_model sat_cnf

let unsat : formula =
  And (Var 'x',
       And (Or (Not (Var 'x'), Var 'y'),
            Or (Not (Var 'x'), Not (Var 'y'))
         )
    )
let _ : bool = is_cnf unsat
let unsat_cnf : cnf = cnf_of_formula unsat
let _ : bool = quine unsat_cnf
let _ : (char * bool) list option = quine_model unsat_cnf
