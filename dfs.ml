let rec replaceElement vertex visited array = 
	match visited with
	[] -> visited |
	h::t -> if List.length array = vertex then (array@[1])@t else replaceElement vertex t (array@[h])  
;;

let rec printList list = 
	match list with
	[] -> Printf.printf "\n" |
	h::t -> begin Printf.printf "%d " h; printList t end
;;

let rec dfs adj visited vertex array =
	let array = array@[vertex] in
	let _ = printList array in
 	(*The function makes the visited[vertex] = 1*)
	let visited = replaceElement vertex visited [] in
	let rec func i = 
		if i < List.length visited then 
		if List.nth (List.nth adj vertex) i = 1 then begin
			let _ = Printf.printf "ADJ(1)%d\n" i in
			if List.nth visited i = 0 then 
				begin 
					let _ = Printf.printf "Visiting adj\n" in
					dfs adj visited i array;	
					func (i+1)
				end else func (i+1)
		end else func (i+1)
	else array
	in func 0
;;



