let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 10

let startVertex = try int_of_string Sys.argv.(3) with _ -> 1

let num_domains = try int_of_string Sys.argv.(4) with _ -> 1

module T = Domainslib.Task

let rec sssp adjMatrix visited queue distance pool = 
	match Lockfree.List.elem_of queue with
	[] -> () |
	(droot, root)::_ ->
		match Lockfree.List.sdelete queue (droot,root) compare with
		false -> Printf.printf "[FAULT IN DELETION]"; exit 0 |
		true ->
		Printf.printf "[ROOT] : %d\n" root;
		match Lockfree.Hash.find adjMatrix root with
			None -> () |
			Some lst ->
				let ar = Array.of_list lst in
				Printf.printf "L : %d" (List.length lst);
				let dist = Atomic.get distance in
				T.parallel_for pool ~start:0 ~finish:(List.length lst - 1) 
				~body:(	fun i -> 
						if visited.(i) = 0 then 
							match Lockfree.List.sinsert queue (dist.(i), i) compare with
							false, _ -> Printf.printf "[FAULT IN INSERTION]";exit 0 |
							true, _ ->
								dist.(i) <- min (dist.(i)) ( dist.(root) +. (snd ar.(i)) );
					else ()
				);
				Printf.printf "[OUT] :  ";
				List.iter (fun (_,x) -> Printf.printf " %d " x) (Lockfree.List.elem_of queue);
				Printf.printf "\n";
				visited.(root) <- 1;
				sssp adjMatrix visited queue distance pool  

let kernel2 () = 
  	let ans = Kernel1_par.linkKronecker () in
  	let adjMatrix = fst ans in
  	let n = snd ans in
  	let s = Unix.gettimeofday () in
  	let visited = Array.make n (0) in
  	let distance = Array.make n (infinity) in
  	distance.(startVertex) <- 0.;
	let queue = Lockfree.List.create () in
	match Lockfree.List.sinsert queue (0.,startVertex) compare with
	false , _ -> exit 0 |
	true,_ -> 
		let pool = T.setup_pool ~num_domains:(num_domains - 1) in
		let t = Unix.gettimeofday () in
		let _ = sssp adjMatrix visited queue (Atomic.make distance) pool in
		let r = Unix.gettimeofday () in
		Printf.printf "\nSSSP: %f\n" (r -. t);
		Printf.printf "\nKERNEL3 TOTAL: %f\n" (r -. s);
		T.teardown_pool pool;
		let _ = Array.iter (fun i -> Printf.printf "%f" i) distance in
		distance

let _ = kernel2 ()