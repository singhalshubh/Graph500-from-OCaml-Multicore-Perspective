let rec appendVerticesToQueue queue adjacentVertices =
	match adjacentVertices with
	[] -> queue |
	head::tail -> appendVerticesToQueue (queue @ [fst(head)]) tail
;; 

let rec bfs adjMatrix queue bfsTree =
	match queue with
	[] -> bfsTree |
	head::tail -> if Hashtbl.mem adjMatrix head = true then 
					let adjacentVertices = Hashtbl.find adjMatrix head in 
					let queue = appendVerticesToQueue tail adjacentVertices in
					let _ = Hashtbl.remove adjMatrix head in bfs adjMatrix queue (bfsTree@[head])
				else bfs adjMatrix tail bfsTree
;; 

let main adjMatrixHash startVertex =
	let adjMatrix = adjMatrixHash in
	let bfsTree = bfs adjMatrixHash [startVertex] [] in bfsTree
;;
