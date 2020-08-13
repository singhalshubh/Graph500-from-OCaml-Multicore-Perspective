(*Kronecker is using the following algorithm : 
 Function Kronecker generator(scale, edgefactor) :
 	N = 2^scale
 	M = edgefactor * N (No of edges)
 	[A,B,C] = [0.57, 0.19, 0.19]
 	ijw = {	{1,1,1,1,1,...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			{1,1,1,1,1...Mtimes};
 			}
 	ab = A + B;
  	c_norm = C/(1 - (A + B));
  	a_norm = A/(A + B);
  	for i in (0, scale) :
  		ii_bit = rand(1,M) > ab;
  		jj_bit = rand (1, M) > ( c_norm * ii_bit + a_norm * not (ii_bit) );(not a is basically a xor 0)
  		ijw(1:2,:) = ijw(1:2,:) + 2^(ib-1) * [ii_bit; jj_bit];

  	ijw(3,:) = unifrnd(0, 1, 1, M);//produce values from 0 to 1 for 1*M array.
  	
  	p = randperm (N);	
  	ijw(1:2,:) = p(ijw(1:2,:));
  	p = randperm (M);
  	ijw = ijw(:, p);
  	ijw(1:2,:) = ijw(1:2,:) - 1;
	Here, the labels are from 0 to N-1.
*)

(*(*<-------OCaml Kronecker Kernel inspired from https://graph500.org/?page_id=12---------->
Written by support of PRISM Lab, IIT Madras and OCaml Labs*)*)

let rec listGenerator1D list m = 
	if m = 0 then list else listGenerator1D (list@[1]) (m-1)
;;

let rec listGenerator m list =
	if List.length list = 3 then list else listGenerator m (list@[listGenerator1D [] m])
;;

let rec modifyIJW ijwRow scale index bit =
	if index = scale then ijwRow else begin 
	let rec modifyElem ijwRow modifiedIjwRow = 
		match  ijwRow with 
		[] -> modifiedIjwRow |
		head::tail -> modifyElem tail (modifiedIjwRow@[head + ( int_of_float( float_of_int(2) **float_of_int (index-1) ) * bit )])
	in modifyIJW (modifyElem ijwRow []) scale (index+1) bit
end
;;

let rec randomWghtGen len list =
	if len = 0 then list else randomWghtGen (len-1) (list@[Random.float 1.])
;;

let rec compareWithPr index m n ab a_norm c_norm ijw scale = 
	let ii_bit = if float_of_int (Random.int m) > ab then 1 else 0 in
	let jj_bit = if float_of_int (Random.int m) > (c_norm *. float_of_int(ii_bit) +. a_norm *. float_of_int (ii_bit lxor 0) ) then 1 else 0 in
	let firstRowIJW = modifyIJW (List.nth ijw 0) scale 0 ii_bit in
	let secondRowIJW = modifyIJW (List.nth ijw 1) scale 0 jj_bit in
	let thirdRow = randomWghtGen m []
	in ([List.map float_of_int (firstRowIJW)] @ [List.map float_of_int (secondRowIJW)] @[thirdRow])
;; 

let rec randomNumberList list index n = 
	if index = 0 then list else randomNumberList (list@[Random.int n]) (index-1) n
;;

let rec floatToInt list index newList = 
	if index = 2 then newList else floatToInt list (index+1) (newList@ [List.map int_of_float (List.nth list index)]) 
;;

let rec intToFloat list index newList = 
	if index = 2 then newList else intToFloat list (index+1) (newList@ [List.map float_of_int (List.nth list index)]) 
;;

let rec permuteVertice list randomList newList row m =
	if row = 2 then newList else
	let rec produceNewRow m newList1 row column = 
		if column = m then newList1 else produceNewRow m (newList1@[ List.nth randomList (List.nth (List.nth list row) column)] ) row (column+1) 
	in permuteVertice list randomList (newList@[produceNewRow m [] row 0] ) (row+1) m
;;

let rec permuteEdgeList list randomList newList m index= 
	if index = m then newList 
else begin 
	let v = List.nth randomList index in let l = newList@[[ (List.nth (List.nth list 0) v ) ; (List.nth (List.nth list 1) v ) ; (List.nth (List.nth list 2) v ) ]] in permuteEdgeList list randomList l m (index+1)
end
;; 

let rec transpose list col newList = 
	if col = 3 then newList else 
		let rec transposeRow row rowList =
			if row = List.length list then rowList else transposeRow (row+1) (rowList@[ List.nth (List.nth list row) col ])
		in transpose list (col+1) (newList@[(transposeRow 0 [])])  
;;

let rec printList list = 
	let _ = Printf.printf "\n" in
	match list with
	[] -> Printf.printf "END" | 
	head::tail -> List.iter print_float head; printList tail
;;


let kronecker scale edgefactor = 
	let n = int_of_float(2.**float_of_int (scale)) in
	let m = edgefactor * n in 
	let a,b,c = 0.57, 0.19, 0.19 in 
	let ijw = listGenerator m [] in
	let ab = a +. b in
	let c_norm = c /. (1. -. (a +. b)) in
	let a_norm = a /. (a +. b) in
	let ijw = compareWithPr scale m n ab a_norm c_norm ijw scale in
	let randomPermute = randomNumberList [] n n in
	let twoRowsijw = floatToInt ijw 0 [] in
	let permuteVerticeIntegerList = permuteVertice twoRowsijw randomPermute [] 0 m in 
	let permuteVerticeFloatList = intToFloat permuteVerticeIntegerList 0 [] in
	let ijw = permuteVerticeFloatList @ [List.nth ijw 2] in
	let randomPermute = randomNumberList [] m m in
	let ijw = permuteEdgeList ijw randomPermute [] m 0 in
	let ijw = transpose ijw 0 [] in 
	let _ = printList ijw in ijw 
;;