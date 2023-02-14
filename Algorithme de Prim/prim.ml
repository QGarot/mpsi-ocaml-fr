type graphe = (int * float) list array

let prim (g : graphe) : graphe =
  let n = Array.length g in
  let t = Array.make n [] in
  (* tableau d'appartenance à t *)
  let dans_t = Array.make n false in
  dans_t.(0) <- true;
  (* tableau d'arêtes minimales vers t *)
  let vers_t = Array.make n (-1, infinity) in
  List.iter (fun (j, w) -> vers_t.(j) <- (0, w)) g.(0);
  (* on lance les itérations *)
  for _ = 1 to n - 1 do
    (* recherche de l'arête min *)
    let i = ref 0 in
    Array.iteri
      (fun j (_, w) -> if not dans_t.(j) && w < snd vers_t.(!i) then i := j)
      vers_t;
    (* ajout de l'arête min *)
    dans_t.(!i) <- true;
    let (j, w) = vers_t.(!i) in
    t.(!i) <- (j , w) :: t.(!i);
    t.(j) <- (!i, w) :: t.(j);
    (* mise à jour des voisins de i *)
    List.iter
      (fun (k, w) ->
        if not dans_t.(k) && w < snd vers_t.(k)
        then vers_t.(k) <- (!i, w))
      g.(!i)
  done;
  t

let g : graphe =
  [| [(1, 4.); (4, 5.); (5, 1.)];
     [(0, 4.); (2, 3.); (3, 1.); (5, 2.)];
     [(1, 3.); (3, 4.); (4, 5.)];
     [(1, 1.); (2, 4.); (4, 4.); (5, 3.)];
     [(0, 5.); (2, 5.); (3, 4.)];
     [(0, 1.); (1, 2.); (3, 3.)]
  |]

let _ : graphe = prim g

type tas = {mutable nb_elt : int; tab : (int * float) array; pos : int array}

let cree_tas_vide (n : int) : tas =
  {nb_elt = 0; tab = Array.make n (-1, infinity); pos = Array.make n (-1)}

let est_vide (t : tas) : bool =
  t.nb_elt = 0

let echange (i : int) (j : int) (t : tas) : unit =
  let (k, w) = t.tab.(i) in
  t.tab.(i) <- t.tab.(j);
  t.tab.(j) <- (k, w);
  t.pos.(k) <- j;
  t.pos.(fst t.tab.(i)) <- i

let rec percole_haut (i : int) (t : tas) : unit =
  let j = (i - 1) / 2 in
  if i > 0 && snd t.tab.(j) > snd t.tab.(i) then begin
      echange i j t;
      percole_haut j t
    end

let rec percole_bas (i : int) (t : tas) : unit =
  let fils_g (i : int) : int = 2 * i + 1 in
  let fils_d (i : int) : int = 2 * i + 2 in
  let j = ref i in
  if fils_g i < t.nb_elt && snd t.tab.(fils_g i) < snd t.tab.(i)
  then j := fils_g i;
  if fils_d i < t.nb_elt && snd t.tab.(fils_d i) < snd t.tab.(!j)
  then j := fils_d i;
  if !j <> i then begin
      echange i !j t;
      percole_bas !j t
    end

let ajoute (i : int) (w : float) (t : tas) : unit =
  t.tab.(t.nb_elt) <- (i, w);
  t.pos.(i) <- t.nb_elt;
  percole_haut t.nb_elt t;
  t.nb_elt <- t.nb_elt + 1

let diminue_poids (i : int) (w : float) (t : tas) : bool =
  let k = t.pos.(i) in
  let (_, w') = t.tab.(k) in
  if w' < w then false
  else begin
      t.tab.(k) <- (i, w);
      percole_haut k t;
      true
    end

let suppr_min (t : tas) : int * float =
  echange 0 (t.nb_elt - 1) t;
  t.nb_elt <- t.nb_elt - 1;
  percole_bas 0 t;
  t.tab.(t.nb_elt)

let prim_tas (g : graphe) : graphe =
  let n = Array.length g in
  let t = Array.make n [] in
  (* tableau d'appartenance à t *)
  let dans_t = Array.make n false in
  (* tableau indiquant pour chaque sommet quel est son père potentiel dans
     l'arbre *)
  let pere = Array.make n (-1) in
  (* création du tas *)
  let h = cree_tas_vide n in
  ajoute 0 0. h;
  for i = 1 to n - 1 do
    ajoute i infinity h
  done;
  (* itérations *)
  while not (est_vide h) do
    let (i, w) = suppr_min h in
    dans_t.(i) <- true;
    if i <> 0 then begin
        let j = pere.(i) in
        t.(i) <- (j , w) :: t.(i);
        t.(j) <- (i, w) :: t.(j);
      end;
    List.iter
      (fun (k, w) ->
        if not dans_t.(k) && diminue_poids k w h then pere.(k) <- i)
      g.(i)
  done;
  t

let _ : graphe = prim_tas g
