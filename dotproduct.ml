let dot_product (matrix_1:int list) (matrix_2:int list) =  
	let rec dot_product product matrix_1 matrix_2 iteration =  	
		match matrix_1, matrix_2 with
		[],[] -> (iteration, product) |
		head1::tail1,[] -> dot_product 0 [] [] false |
		[],head2::tail2 -> dot_product 0 [] [] false |
		head1::tail1, head2::tail2 -> dot_product (product + (head2 * head1)) (tail1) (tail2) true
	in dot_product 0  matrix_1 matrix_2 true 
;;

let main matrix_1 matrix_2 = 
	let it, product = dot_product matrix_1 matrix_2 in
	if it = false then let _ = Printf.printf "Error : Size not compatible" in product else let _ = Printf.printf "Valid dot_product" in product
;;