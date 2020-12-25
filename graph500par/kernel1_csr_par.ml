(*Kernel 1 is basic construction of adjacency HashMap for undirected graphs 
which is corresponding to sparse graph implementation. INPUTS : ijw and m which has been 
derived from kronecker product*)

(*(*<-------OCaml Kernel 1 inspired from https://graph500.org/?page_id=12---------->
Written by support of PRISM Lab, IIT Madras and OCaml Labs*)*)

let scale = try int_of_string Sys.argv.(1) with _ -> 12

let edgefactor = try int_of_string Sys.argv.(2) with _ -> 10

let num_domains = try int_of_string Sys.argv.(3) with _ -> 1

module T = Domainslib.Task

(*This basically sorts the list in a way that (startVertex, endVertex), 
startVertex > endVertex.
It removes the self loops from ijw*)
let sortVerticeList ar index =
  let rec sortVerticeList ar maximum index =
    if index = -1 then (int_of_float maximum)
    else if ar.(0).(index) > ar.(1).(index) then
      sortVerticeList ar
        (max maximum ar.(0).(index)) (index-1)
    else
      sortVerticeList ar
        (max maximum ar.(1).(index))
        (index - 1)
  in
  sortVerticeList ar 0. index

let rec readFile file ijw =
  try
    match Some (input_line file) with
    | None -> ijw
    | Some line -> (
        match List.rev (String.split_on_char ',' line) with
        | [] -> readFile file ijw
        | _ :: tail ->
            let list =
              Array.of_list (List.map float_of_string (List.rev tail))
            in
            Printf.printf "\n";
            let ijw = Array.append ijw [| list |] in
            readFile file ijw )
  with End_of_file ->
    let _ = close_in file in
    ijw

let computeNumber scale edgefactor =
  let n = int_of_float (2. ** float_of_int scale) in
  let m = edgefactor * n in
  (n, (2*m))

let rec adjust row index value = 
  if row.(index) = value then ()
else begin row.(index) <- value; adjust row (index-1) value end

let rec construction rowijw row index m vertice = 
  if index = m+1 then begin row.(vertice + 1) <- index-1; adjust row (Array.length row - 1) (index-1) end 
else
  if vertice != rowijw.(index) then
    if index - 1 < 0 then begin row.(vertice+1) <- row.(vertice); construction rowijw row (index) m (vertice+1) end  
    else
    if rowijw.(index-1) = vertice then begin row.(vertice+1) <- max (row.(vertice+1)) (index-1);
    construction rowijw (row) (index) m (vertice + 1) end
    else begin row.(vertice+1) <- max (row.(vertice+1)) (index-1); construction rowijw row (index) m (vertice+1) end
  else construction rowijw (row) (index+1) m (vertice)

let transpose ar newAr =
  for i = 0 to Array.length ar - 1 do
    for j = 0 to Array.length ar.(0) - 1 do
      !newAr.(j).(i) <- ar.(i).(j)
    done
  done;
  !newAr

let rec maxval index row finalRow i = 
  if index = -1 then ()
  else begin finalRow.(i) <- max finalRow.(i) row.(index).(i); maxval (index-1) row finalRow i end

let rec maximumConstr row finalRow i =
  if i = -1 then ()
else begin
  maxval (num_domains - 1) row finalRow i;
  maximumConstr row finalRow (i-1) end

let kernel1 ijw m n pool =
  (*let s = Unix.gettimeofday () in*)
  let maximumEdgeLabel = sortVerticeList ijw (m - 1) in
  let ar = transpose ijw (ref (Array.make_matrix m 3 1.)) in
  Array.sort compare ar;
  (*Array.iter (fun i-> Printf.printf "%f,%f,%f \n" i.(0) i.(1) i.(2)) ar;*) 
  let ijw = transpose (ar) (ref ijw) in
  Array.iter (fun i -> Printf.printf "%f " i) (ijw.(0));
  Printf.printf "\n";
  Printf.printf "Kernel1 Started \n";
  let temp = (Array.length (ijw.(0)) )/num_domains in
  let row = Array.make_matrix (num_domains) (n+1) 0 in
  T.parallel_for pool ~start:0 ~finish:(num_domains-1)
    ~body:( fun i -> if i=(num_domains-1) then 
     construction (Array.map (int_of_float) (ijw.(0)) ) row.(i) (i*temp) ( (Array.length ijw.(0)) - 1) (int_of_float(ijw.(0).(i*temp)))
    else construction (Array.map (int_of_float) (ijw.(0)) ) row.(i) (i*temp) (i*temp + temp-1) (int_of_float(ijw.(0).(i*temp)))
    );
  let _ = T.teardown_pool pool in
  let r = Array.make (n+1) 0 in
  maximumConstr row r (n);
  (ijw.(2), (Array.map (int_of_float) ijw.(1)) ,r, maximumEdgeLabel + 1)


let linkKronecker () =
  (*let s = Unix.gettimeofday () in *)
  let file = open_in "kronecker32.txt" in
  let ijw = readFile file [||] in
  let (n,m) = computeNumber scale edgefactor in
  (*Array.iter (fun i -> Printf.printf "%f " i) (ijw.(0));
  Printf.printf "\n";
  Array.iter (fun i -> Printf.printf "%f " i) (ijw.(1));*)
  (*let r = Unix.gettimeofday () in*)
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let (value , col, row, number) =
    kernel1 ijw m n pool
  in
  Printf.printf "Value : \n";
  Array.iter (fun i -> Printf.printf "%f " i) (value);
  Printf.printf "\n";
  Printf.printf "Col : \n";
  Array.iter (fun i -> Printf.printf "%d " i) (col);
   Printf.printf "\n";
  Printf.printf "Row : \n";
  Array.iter (fun i -> Printf.printf "%d " i) (row);
   Printf.printf "\n";
  value, col, row, number
