let minimumDistance verticesInspected distanceArray = 
	let rec minimumDistance verticesInspected distanceArray minimumVal index minimumVertice =
		if index = Array.length verticesInspected then minimumVal, minimumVertice else
		if minimumVal > distanceArray.(verticesInspected.(index)) then minimumDistance verticesInspected distanceArray (distanceArray.(verticesInspected.(index))) (index+1) (verticesInspected.(index))
	else minimumDistance verticesInspected distanceArray minimumVal (index+1) (minimumVertice)
in minimumDistance verticesInspected distanceArray infinity 0 0
;;

let rec adjustDistance vertex adjacentList distanceArray parentArray visited =
	match adjacentList with
	[] -> distanceArray, parentArray |
	head::tail -> if visited.(fst(head)) = 1 then adjustDistance vertex tail distanceArray parentArray visited else
					let adjvertex = fst(head) in if distanceArray.(adjvertex) < (distanceArray.(vertex) +. snd(head) ) then adjustDistance vertex tail distanceArray parentArray visited
					else let _ = parentArray.(adjvertex) <- vertex in let _ = distanceArray.(adjvertex) <- (distanceArray.(vertex) +. snd(head)) in adjustDistance vertex tail distanceArray parentArray visited
;;  

let rec changeVerticeInspected vertex (list:int list) (newList:int list) =
	match list with
	[] -> Array.of_list newList |
	head::tail -> if head = vertex then changeVerticeInspected vertex tail newList else changeVerticeInspected vertex tail (newList@[head])
;;

let rec dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited = 
	if Array.length verticesInspected = 0 then distanceArray, parentArray 
	else
		let minDist, minVerticeId = minimumDistance verticesInspected distanceArray in
		let adjacentVerticeList = Hashtbl.find adjMatrix minVerticeId in
		let distanceArray, parentArray = adjustDistance minVerticeId adjacentVerticeList distanceArray parentArray visited in
		let verticesInspected = changeVerticeInspected minVerticeId (Array.to_list (verticesInspected)) [] in 
		let _ = visited.(minVerticeId) <- 1 in dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited
;;

let main adjMatrix startVertex = 
	let size = Hashtbl.length adjMatrix in
	let parentArray = Array.make size 0 in
	let distanceArray = Array.make size infinity in
	let _ = parentArray.(startVertex) <- startVertex in
	let _ = distanceArray.(startVertex) <- 0. in
	let verticesInspected = Array.init size (fun(x)->x) in
	let visited = Array.make size 0 in
	let distanceArray,parentArray = dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited in distanceArray, parentArray
;;