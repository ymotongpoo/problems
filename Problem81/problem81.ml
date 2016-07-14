(* -*- coding: utf-8 -*- *)
(**
   Problem 81

   In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
   by only moving to the right and down, is indicated in bold red and is equal to 2427.


   [131]  673    234    103    18
   [201]  [96]   [342]  965    150
   630    803    [746]  [422]  111
   537    699    497    [121]  956
   805    732    524    [37]   [331]

   Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'),
   a 31K text file containing a 80 by 80 matrix, from the top left to the bottom
   right by only moving right and down.
*)

module A : sig

  val filename : string
    (** filename which stores matrix data *)

  val load_matrix: string -> int array array
    (** [load_matrix filename] returns multidimensional array whose
        elsements are stored in [filename] *)

  val solver : int array array -> int
    (** [solver matrix] returns minimal cost *)

  val solve_dijkstra : (int, int) Hashtbl.t -> int

  val test : unit -> unit

end = struct

  open Euler

  let filename = "matrix.txt"

  let load_matrix filename = 
    let cin = open_in filename in
    let buf = ref [] in
    try
      while true; 
      do
        let line = input_line cin in
        buf := line :: !buf
      done;
      Array.make 0 (Array.make 0 0)
    with
    | End_of_file ->
        close_in cin;
        let lines = List.rev !buf in
        let line = List.nth lines 0 in
        let num_elems = ref 1 in
        for i = 0 to String.length line - 1 do
          if line.[i] = ','
          then num_elems := !num_elems + 1
          else ()
        done;
        let num_lines = List.length lines in
        let matrix =
          Array.make num_lines (Array.make !num_elems 0) in
        let enum_lines = Euler.range 0 num_lines in
        let enum_elems = Euler.range 0 !num_elems in
        let strip_by_comma line =
          let last = String.length line - 1 in
          let rec strip prev cur elems =
            match prev, cur, elems with
            | _, c, _ when c > last -> elems
            | p, c, es when c = last ->
                int_of_string (String.sub line (succ p) (c-p-1))::es
            | p, c, es when line.[c] = ',' ->
                strip c (succ c) (int_of_string (String.sub line (succ p) (c-p-1))::es)
            | p, c, es ->
                strip p (succ c) es
          in
          List.rev (strip 0 1 [])
        in
        List.iter (fun i -> List.iter2 (fun j x -> matrix.(i).(j) <- x)
            enum_elems (strip_by_comma (List.nth lines i))) enum_lines;
        matrix
  ;;

  (* solve_dijkstra : (int, int) Hashtbl.t -> int *)
  let solve_dijkstra init = 0;;

  let solver matrix =
    let max_row = Array.length matrix in
    let max_col = Array.length matrix.(0) in
    let init = [] in
    let nodes = 
      Array.iteri (fun r x ->
          Array.iteri (fun c y -> ((r, c), (false, -1))::init x) matrix)
    in
    solve_dijkstra table
  ;;

  
  let test () = 
    let _ = load_matrix filename in
    ()

end;;

A.test ();;
