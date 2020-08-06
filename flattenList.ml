let rec printList list2 = 
	match list2 with
	[] -> None |
	h::t -> begin Printf.printf "%c" h; printList t; end
;;


let rec compressList list1 list2 = 
	let _ = Printf.printf "\n" in
	let _ = printList list2 in
	let _ = Printf.printf " " in
	match list1 with
	[] -> None |
	h::t -> begin printList t; if List.length list2 = 0 || ( List.length list2 >= 1 && h != (List.nth list2 (List.length list2 -1 )) ) then compressList t (list2@[h]) else compressList t list2 end
;; 

(*List can be of unequal lengths*)
let list3 = ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e'] in compressList list3 [];;