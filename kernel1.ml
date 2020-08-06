let rec transpose list col newList = 
	if col = 3 then newList else 
		let rec transposeRow row rowList =
			if row = List.length list then rowList else transposeRow (row+1) (rowList@[ List.nth (List.nth list row) col ])
		in transpose list (col+1) (newList@[(transposeRow 0 [])])  
;;

let rec removeSelfLoops ijw newList col m =
	if col = m then (transpose newList 0 []) else begin
		if  List.nth (List.nth ijw 0) col = List.nth (List.nth ijw 1) col 
		then removeSelfLoops ijw newList (col+1) m 
	else removeSelfLoops ijw (newList@[ [(List.nth (List.nth ijw 0) col) ; (List.nth (List.nth ijw 1) col) ; (List.nth (List.nth ijw 2) col) ] ]) (col+1) m
	end 
;;

let rec kernel1 ijw = 
