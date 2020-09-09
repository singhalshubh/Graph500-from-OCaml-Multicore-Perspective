let max_label_proc matrix = 
	let rec max_label_proc matrix res index =
		if index = Array.length (matrix.(0)) then res else
			if res < matrix.(0).(index) then max_label_proc matrix (matrix.(0).(index)) (index+1)
		else max_label_proc matrix res (index+1)
in max_label_proc matrix 0 0


let spmv matrix vector = 
	let rec spmv matrix vector result index = 
		if index = Array.length (matrix.(0)) then result 
	else
		let _ = result.(matrix.(0).(index)) <- result.(matrix.(0).(index)) + vector.(matrix.(1).(index)) * matrix.(2).(index) in
		spmv matrix vector result (index+1)
	in spmv matrix vector (Array.make (max_label_proc matrix + 1) 0) 0
;;

