(* Fonction fusion *)

let fusion (a : int array) (b : int array) : int array =
	let n1 = Array.length a and n2 = Array.length b in
	let t = Array.make (n1 + n2) 0 in
	let i = ref 0 and j = ref 0 in
	for k = 0 to n1 + n2 - 1 do
	  if !i = n1 then
	    begin
	      t.(k) <- b.(!j);
	      j := !j + 1;
	    end
	  else if !j = n2 then
	    begin
	      t.(k) <- a.(!i);
	      i := !i + 1;
	    end
	  else
	    begin
	      let (e1, e2) = (a.(!i), b.(!j)) in
	      if e1 <= e2 then
	        begin
	          t.(k) <- e1;
	          i := !i + 1;
	        end
	      else
	        begin
	         t.(k) <- e2;
	          j := !j + 1;
	        end
	      end
	done;
	t;;

fusion [| 1; 2; 5|] [| 3; 4 |];;

let division (t : int array) : int array * int array =
  let n = Array.length t in
  let t1 = Array.make (n / 2) 0 and t2 = Array.make (n - n / 2) 0 in
  for i = 0 to n - 1 do
    if i < n / 2 then t1.(i) <- t.(i) else t2.(i - n / 2) <- t.(i);
  done;
  (t1, t2);;
	
division [| 1; 2; 3; 4; 5; 6; 7; 5; 8 |];;

let rec tri_fusion (t : int array) : int array =
  if Array.length t <= 1 then t
  else let (t1, t2) = division t in fusion (tri_fusion t1) (tri_fusion t2);;

tri_fusion [| 1500;3000;18000;7;5;9;95;21;350000;12 |];;