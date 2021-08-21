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