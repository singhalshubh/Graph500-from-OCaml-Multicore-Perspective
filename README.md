# Graph500 from OCaml-Multicore Perspective

#### This project has been done in collaboration with OCaml Labs, UK and Dr. KC Sivaramakrishnan at Indian Institute of Technology, Madras in the capacity of Research SDE. 

### Overview 
OCaml  is  a  industry  adapted  language  focused  on solving  numerical  and  scientific  computing  problems and one such use case was developing supe intensive graph  problems. We try to implement Graph  Construction, BFS, Shortest-Path  problems  using  the desired  specifications  and  rules  posed  by  graph500. This  project  aims  at  providing  a  clear  direction  of choices  of  several  data  structures  used,  algorithms developed  and  pose  a  reason  behind  every  step  of program.  
<br>
The code describes the algorithms in detail with the possibilities of future exploration. All performance metrics were tested on *Intel(R) Xeon(R)Gold 5120 CPU @ 2.20GHz 24 core* machine.

### Benchmark Graph500
Graphs are a core part of most analytics workloads.
The intent of benchmark problems **(“Search” and “Shortest-Path”)** is to develop a compact application that has multiple analysis techniques (multiple kernels) accessing a single data structure representing a **weighted, undirected graph**. In addition to a kernel to construct the graph from the input tuple list, there are two additional computational kernels to operate on the graph.
<br>
This benchmark includes a *scalable data generator* which produces edge tuples containing the start vertex and end vertex for each edge. The first kernel constructs an undirected graph in a format usable by all subsequent kernels. No subsequent modifications are permitted to benefit specific kernels. The second kernel performs a *breadth-first search* of the graph. The third kernel performs *single-source shortest path* computations on the graph.
Refer to https://graph500.org/?page_id=12 for more details.
### Installation Steps
1. Install OCaml Compiler and its packages from https://github.com/ocaml-multicore/parallel-programming-in-multicore-ocaml. Download Domainslib for running graph500 parallel version.
2. **For Sequential**
   - Look for graph500seq on master branch and clone it to a local machine.
   - Kronecker.ml produces the graph in <startVertex, endVertex, weight> tuples in columns, and stores it in the file named "kronecker.txt"
      + Run Kronecker using ocamlopt as : ocamlopt kronecker.ml ; ./a.out <scale> <edgefactor>.
   - After your graph is produced, we have to run the kernels.
   - Look for the file named "dune". For running kernel1, do two changes 
      + Include this piece of code at the bottom of kernel1
        ``` 
            ;; 
            let _ = linkKronecker();;
        ```
      + Change the file name in dune to just kernel1.
   - For running kernel2, do not do any changes to kernel1. Just change the file name in dune to just kernel2. Similarly for kernel3.
   - Now run, 
      ```
      
      dune build @buildbench
      ./<executable> <params>
      
      ```
  Make sure that you always generate kronecker.txt first and then run all the kernels. Run CSR_Sequential format in the next step as it requires format of Kronecker generation.<br>
  3. **For Parallel**
   - Look for graph500par on master branch and clone it to a local machine.
   - Kronecker.ml produces the graph in <startVertex, endVertex, weight> tuples in columns, and stores it in the file named "kronecker32.txt"
      + Run Kronecker using ocamlopt as : ocamlopt kronecker.ml ; ./a.out <scale> <edgefactor>.
   - After your graph is produced, we have to run the kernels.
   - Look for the file named "dune". For running kernel1_csr (sequential version of CSR format) or kernel1_csr_par, do two changes 
      + Include this piece of code at the bottom of kernel1
        ``` 
            ;; 
            let _ = linkKronecker();;
        ```
      + Change the file name in dune to just kernel1_csr or kernel1_csr_par (whichever is choosen).
   - For running kernel2, do not do any changes to kernel1. Just change the file name in dune to just kernel2_par. Similarly for kernel3.
   - Now run, 
      ```
      
      dune build @buildbench
      ./<executable> <params>
      
      ```
   - For running kernel1_par.ml, Copy kronecker.txt from graph500seq. and then do the two changes
      + Include this piece of code at the bottom of kernel1
        ``` 
            ;; 
            let _ = linkKronecker();;
        ```
      + Change the file name in dune to just kernel1_par.
   - If you want to run kernel 2 and kernel 3 using Hashmap implementation, please follow the codes from graph500par branch commits                https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/commit/915d96db9c4a4d9c0589aca547fe93952c5b3344. 
   
  CSR and Hashmap have different kronecker outputs and different kernel codes altogether.
  *Caution : Refer to the params very carefully especially in num_domains and start vertex in kernel2_par and kernel3_par.*
  
  ### Results
  1. For **Kernel2** : Both have been obtained for scale = 12, edgefactor = 30.<br>
  ![Speedup](https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/blob/master/graph500par/Speedup%20vs%20Number%20of%20Cores%20-%20Kernel2.png) <br>
  
  ![Execution](https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/blob/master/graph500par/Execution%20Time%20vs.%20Number%20of%20Cores.png)
  <br>
  
  2. For **Kernel3** : <br>
  ![SSSP Speedup](https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/blob/master/graph500par/Kernel3(SSSP)%20-%20Speedup%20(y-axis)%20vs%20no%20of%20cores%20(x-axis).png)
  
  For detailed analysis of results and performance metrics, and future work for kernel1 (selection of apt format) and kernel3_par (delta stepping algorithm), please refer to the documentation. Feel free to reach out to me at shubhpal@seas.upenn.edu.
  
  #### Future Work
  Construction of Kernel3_par (using delta stepping/radius stepping) and optimizations for parallel version of kernel1. <br><br>
  
  Apart from the graph500 work, I addressed the issue on https://github.com/ocaml-bench/sandmark/ **[RFC] Classifying benchmarks based on running time #179**, mentioned in folder Git Issues.<br>
  If you want to follow my production process of graph500seq and my start in OCaml, refer https://github.com/ocaml-bench/sandmark/commits?author=singhalshubh <br>
  For graph500par, refer to commits in https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/commits/graph500par <br>
  For those who are starting in OCaml, refer to the documentation and https://github.com/singhalshubh/Graph500-from-OCaml-Multicore-Perspective/tree/Documentation-StartingOCaml
  
  
