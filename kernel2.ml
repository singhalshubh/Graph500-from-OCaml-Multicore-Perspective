let rec appendVerticesToQueue parentVertex queue adjacentVertices (parentArray:int array) visited =
	match adjacentVertices with
	[] -> queue,parentArray |
	head::tail -> if visited.(fst(head)) = 0 then let _ = parentArray.(fst(head)) <- parentVertex in appendVerticesToQueue parentVertex (queue @ [fst(head)]) tail parentArray visited
					else appendVerticesToQueue parentVertex queue tail parentArray visited
;; 

(*let rec printList list = 
	match list with
	[] -> Printf.printf "Empty/END" |
	head::tail -> let _ = Printf.printf "%d" head in printList tail
;;*)

let rec bfs adjMatrix queue bfsTree parentArray visited =
	match queue with
	[] -> bfsTree,parentArray |
	head::tail -> if visited.(head) = 0 then
					let _ = visited.(head) <- 1 in
					let adjacentVertices = Hashtbl.find adjMatrix head in 
					let queue, parentArray = appendVerticesToQueue head tail adjacentVertices parentArray visited in
					(*let _ = printList queue in*)
					let _ = Hashtbl.remove adjMatrix head in bfs adjMatrix queue (bfsTree@[head]) parentArray visited
				else bfs adjMatrix tail bfsTree parentArray visited
;;

let main adjMatrixHash startVertex =
	let len = (Hashtbl.length adjMatrixHash) in
	let adjMatrix = adjMatrixHash in
	let parentArray = Array.make len (-1) in
	let visited = Array.make len 0 in
	let bfsTree,parentArray = bfs adjMatrixHash [startVertex] [] parentArray visited in (bfsTree, parentArray)
;;
