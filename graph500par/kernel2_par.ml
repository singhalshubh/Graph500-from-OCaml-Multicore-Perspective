let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 10

let startVertex = try int_of_string Sys.argv.(3) with _ -> 1

let num_domains = try int_of_string Sys.argv.(4) with _ -> 1

module T = Domainslib.Task

(*let rec printList lst = 
	match lst with
	[] -> None |
	hd::tl -> Printf.printf "%d" (fst hd); printList tl
*)

let rec findlst lst col row root index = 
	if index = (row.(root+1)+1) then () 
else
	if col.(index) = root then 
	findlst lst col row root (index+1) else
	begin Lockfree.List.push lst col.(index);
	findlst lst col row root (index+1) end

let rec bfs col row level queue pool l = 
	if Lockfree.List.is_empty queue = true then ()
else
	(*THis is basically accumulating all neighbours of all nodes in the queue*)
	let adjlistlevel = Array.of_list (Lockfree.List.elem_of queue) in
	let queue1 = Lockfree.List.create () in
	let x = Lockfree.List.create () in 
	T.parallel_for pool ~start:0 ~finish:(Array.length adjlistlevel - 1)
	~body:( fun i -> 
			findlst x col row adjlistlevel.(i) (row.(adjlistlevel.(i) ) ) 
		);
	let x = Lockfree.List.elem_of x in
	T.parallel_for pool ~start:0 ~finish:(List.length x - 1) 
	~body:(	fun i -> 
				(*Printf.printf "INdex : %d " i;*)
				if level.(List.nth x i) != (-1) then ()
						 else begin 
							(*Printf.printf "efjewfjef\n";*)
							level.(List.nth x i) <- l + 1;
							(*Printf.printf "jk";*)
							Lockfree.List.push queue1 (List.nth x i)
							end 
		);
		bfs col row level queue1 pool (l+1)

let kernel2 () = 
  	let (_, col, row, n) = Kernel1_csr.linkKronecker () in
  	let s = Unix.gettimeofday () in
  	Printf.printf "Kernel3";
  	let level = Array.make n (-1) in
  	level.(startVertex) <- 0;
	let queue = Lockfree.List.create () in
	let _ = Lockfree.List.push queue startVertex in
	let pool = T.setup_pool ~num_domains:(num_domains - 1) in
	let t = Unix.gettimeofday () in
	let _ = bfs col row level queue pool 0 in
	let r = Unix.gettimeofday () in
	Printf.printf "\nBFS: %f\n" (r -. t);
	Printf.printf "\nKERNEL2 TOTAL: %f\n" (r -. s);
	T.teardown_pool pool;
	let _ = Array.iter (fun i -> Printf.printf "%d" i) level in
	level

let _ = kernel2 ()