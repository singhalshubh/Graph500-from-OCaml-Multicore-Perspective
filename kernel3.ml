(*Kernel 3 works on the dijkstars algorithm which is basically the shortest path
from the node to all the nodes. As expected the main function here takes the
HashMap and  the source node from where the shortest distnaces have to be
calculated.*) (*INPUTS : Adjacency HashMap and Start Vertex*)

(*<-------OCaml Kernel 3 inspired from https://graph500.org/?page_id=12---------->
Written by support of PRISM Lab, IIT Madras and OCaml Labs*)

(*Minimum Distance function computes the vertex which is at the min distance from the node which is currently under study. Only the nodes which have not been visited
are only considered. This function is not checking the visited as the verticesInspected already takes care of it in other function (changeVerticeInspected).
E.g. N1 is under study and now the nodes N2, N3 are at 2, inf distance in distanceArray, so N2 will be selected as the min vertex.*)

let minimumDistance verticesInspected distanceArray = 
	let rec minimumDistance verticesInspected distanceArray minimumVal index minimumVertice =
		if index = Array.length verticesInspected then minimumVal, minimumVertice else
		if minimumVal > distanceArray.(verticesInspected.(index)) then minimumDistance verticesInspected distanceArray (distanceArray.(verticesInspected.(index))) (index+1) (verticesInspected.(index))
	else minimumDistance verticesInspected distanceArray minimumVal (index+1) (minimumVertice)
in minimumDistance verticesInspected distanceArray infinity 0 0
;;

(*After determining the min vertex (obtained from prev func), we haveto adjust dist such that, for all node, if dist[node] > dist[min vertex] + weight_node_minVertex
then update the distance else continue with other nodes. Parent Array is also updated here.*)

let rec adjustDistance vertex adjacentList distanceArray parentArray visited =
	match adjacentList with
	[] -> distanceArray, parentArray |
	head::tail -> if visited.(fst(head)) = 1 then adjustDistance vertex tail distanceArray parentArray visited else
					let adjvertex = fst(head) in if distanceArray.(adjvertex) < (distanceArray.(vertex) +. snd(head) ) then adjustDistance vertex tail distanceArray parentArray visited
					else parentArray.(adjvertex) <- vertex; distanceArray.(adjvertex) <- (distanceArray.(vertex) +. snd(head)); adjustDistance vertex tail distanceArray parentArray visited
;;  

(*This basically removes the nodes visited from the list verticesInspected*)

let rec changeVerticeInspected vertex (list:int list) (newList:int list) =
	match list with
	[] -> Array.of_list newList |
	head::tail -> if head = vertex then changeVerticeInspected vertex tail newList else changeVerticeInspected vertex tail (newList@[head])
;;

(*Dijkstra's algorithm which calculates the min vertex, then adjust the distance , update the visited list and iterate till all nodes have been inspected.*)
let rec dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited = 
	if Array.length verticesInspected = 0 then distanceArray, parentArray 
	else
		let minDist, minVerticeId = minimumDistance verticesInspected distanceArray in
		let adjacentVerticeList = Hashtbl.find adjMatrix minVerticeId in
		let distanceArray, parentArray = adjustDistance minVerticeId adjacentVerticeList distanceArray parentArray visited in
		let verticesInspected = changeVerticeInspected minVerticeId (Array.to_list (verticesInspected)) [] in 
		let _ = visited.(minVerticeId) <- 1 in dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited
;;

(*ALl intialisation done in main function. Weights are float and vertices are int*)
let main adjMatrix startVertex = 
	let size = Hashtbl.length adjMatrix in
	let parentArray = Array.make size 0 in
	let distanceArray = Array.make size infinity in
	parentArray.(startVertex) <- startVertex;
	distanceArray.(startVertex) <- 0.;
	let verticesInspected = Array.init size (fun(x)->x) in
	let visited = Array.make size 0 in
	let distanceArray,parentArray = dijkstraAlgorithm adjMatrix parentArray distanceArray verticesInspected visited in distanceArray, parentArray
;;