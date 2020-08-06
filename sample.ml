let calculator a b option1 = 
	match option1 with 
		1 ->a+b | 
		2 ->a-b |
		3 -> a*b |
		4 -> if b=0 then -1 else a/b |
		_ -> -1

let rec main () =
	Printf.printf "Choose an option amongst - \n Option 1: ADD\n Option 2: SUB\nOption 3: MUL\nOption 4: DIV\n";
	let choice1 = Scanf.scanf "%d\n" (fun x->x) in
	let a = Scanf.scanf "%d\n" (fun x->x) in
	let b = Scanf.scanf "%d\n" (fun x->x) in 
	let result = calculator a b choice1 in
	if result = -1 then print_endline "Cannot" 
	else Printf.printf "\nResult is :%d\n" result; 
	let choice2 = Scanf.scanf "%c\n" (fun x -> x) in 
	if choice2 = 'y' then main ()
	else Printf.printf "End";
;;

main ();;