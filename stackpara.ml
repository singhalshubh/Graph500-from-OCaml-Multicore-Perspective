let rec paraCheck st s index = 
	if index = String.length s then 
		if Stack.is_empty st then true
		else false
	else
		if s.[index] = '(' 
			then begin
			Stack.push s.[index] st;
			paraCheck st s (index+1) end
		
		else if s.[index] = ')' 
		then 
			if Stack.is_empty st = false && Stack.top st = '(' 
				then begin
				Stack.pop st;
				paraCheck st s (index+1) end
			
			else false
		else false
;;

let str = "(())";;
let st = Stack.create ();;
paraCheck str st 0;;