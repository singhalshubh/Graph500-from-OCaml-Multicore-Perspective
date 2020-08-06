let rec printElements array1 = 
	match array1 with
	[] -> None |
	h::t -> Printf.printf "%d " h ; printElements t;;

printElements [1;2;3;4;5];;
print_endline "";;