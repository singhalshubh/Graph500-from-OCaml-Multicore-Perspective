let rec transpose list col newList = 
	if col = 3 then newList else 
		let rec transposeRow row rowList =
			if row = List.length list then rowList else transposeRow (row+1) (rowList@[ List.nth (List.nth list row) col ])
		in transpose list (col+1) (newList@[(transposeRow 0 [])])  
;;

let rec sortVerticeList list newList =
	let rec sortVerticeList list newList maximum = 
		match list with
		[] -> (newList,maximum) |
		head::tail -> let x = List.nth head 0 and y = List.nth head 1 in if x > y then sortVerticeList tail (newList@[[x;y;List.nth head 2]]) (max maximum x)     
		else sortVerticeList tail (newList@[[y;x;List.nth head 2]]) (max maximum y)
	in sortVerticeList list newList 0
;;

let rec removeSelfLoops ijw newList col m =
	if col = m then newList else begin
		if  List.nth (List.nth ijw 0) col = List.nth (List.nth ijw 1) col 
		then removeSelfLoops ijw newList (col+1) m 
	else removeSelfLoops ijw (newList@[ [(List.nth (List.nth ijw 0) col) ; (List.nth (List.nth ijw 1) col) ; (List.nth (List.nth ijw 2) col) ] ]) (col+1) m
	end 
;;

(*let constructionAdjMatrix list maxLabel =
	 let matrix = Array.make_matrix maxLabel maxLabel 0. in
	 let rec fillMatrix matrix list =
	 	match list with
	 	[] -> matrix |
	 	head::tail -> let _ = matrix.(int_of_float(List.nth head 0)).(int_of_float(List.nth head 1)) <- (List.nth head 2) in  
	 				let _ = matrix.(int_of_float(List.nth head 1)).(int_of_float(List.nth head 0)) <- (List.nth head 2) in fillMatrix matrix tail
	in fillMatrix matrix list
;;*)

let addEdge startVertex endVertex weight hashTable =
	if Hashtbl.mem hashTable startVertex = false
	then let _= Hashtbl.add hashTable startVertex [(endVertex,weight)] in hashTable
	else let _ = Hashtbl.replace hashTable startVertex ( (Hashtbl.find hashTable startVertex) @ [(endVertex,weight)]) in hashTable
;;


let rec constructionAdjHash list hashTable = 
	match list with 
	[] -> hashTable |
	head::tail -> let startVertex = List.nth head 0 and endVertex = List.nth head 1 and weight = List.nth head 2 in
					let h = addEdge startVertex endVertex weight hashTable in let hashTable = addEdge endVertex startVertex weight h in constructionAdjHash tail hashTable 
;;

let rec kernel1 ijw m = 
	let list = removeSelfLoops ijw [] 0 m in
	let list,maximumEdgeLabel = sortVerticeList list [] in
	let hashTable = Hashtbl.create size in
	let adjMatrix = constructionAdjHash list hashTable in adjMatrix
;;