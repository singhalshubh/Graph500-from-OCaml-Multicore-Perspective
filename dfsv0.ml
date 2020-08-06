let rec replaceElement vertex visited array = 
	match visited with
	[] -> visited |
	h::t -> if List.length array = vertex then (array@[1])@t else replaceElement vertex t (array@[h])  
;;

let rec dfs adj visited vertex array =
	let _ = Printf.printf "%d" vertex in
 	(*The function makes the visited[vertex] = 1*)
	let visited = replaceElement vertex visited [] in
	let rec func i = 
		(*let _ = Printf.printf "%d\n" i in*)
		if i < List.length visited then 
		if List.nth (List.nth adj vertex) i = 1 then begin
			(*let _ = Printf.printf "ADJ(1)%d\n" i in*)
			if List.nth visited i = 0 then 
				begin 
					(*let _ = Printf.printf "Visiting adj" in*)
					dfs adj visited i (array@[i]);
					func (i+1)
				end else func (i+1)
		end else func (i+1)
	in func 0
;;



