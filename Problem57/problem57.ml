(* -*- mode:tuareg coding:utf-8 -*-; *)
(**

It is possible to show that the square root of two can be expressed as 
an infinite continued fraction.

 /2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, 
but the eighth expansion, 1393/985, is the first example 
where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, 
how many fractions contain a numerator with more digits than denominator?

*)

module A = struct

  open Big_int

  (** Big_int operation *)
  let (+~) = add_big_int;;
  let (-~) = sub_big_int;;

  (** numbers int -> int list *)
  let numbers limit = 
    let rec numbers accu = function 
      | 0 -> 0::accu
      | n -> numbers (n::accu) (pred n)
    in
    numbers [] limit
  ;;

  (** lookup: hashtbl -> (int -> big_int) -> int -> big_int  *)
  let lookup table func arg =
    if Hashtbl.mem table arg
    then Hashtbl.find table arg
    else
      let value = func arg in
      begin
        Hashtbl.add table arg value;
        value
      end
  ;;

  (**
     nums: Hashtbl.t -> Hashtbl.t -> big_int -> big_int
     dens; Hashtbl.t -> Hashtbl.t -> big_int -> big_int
  *)
  let rec nums nt dt = function
    | 0 -> big_int_of_int 3
    | n -> (lookup dt (dens nt dt) n) +~ (lookup dt (dens nt dt) (pred n))
  and dens nt dt = function
    | 0 -> big_int_of_int 2
    | n -> (lookup nt (nums nt dt) (pred n)) +~ (lookup dt (dens nt dt) (pred n))
  ;;


  (** fraction: Hashtbl.t -> Hashtbl.t -> int -> big_int * big_int *)
  let fraction nt dt n = (nums nt dt n, dens nt dt n) ;;

  (** 
      digits_of_num: big_int -> int 
      @param num target number
      @return number of digits of input variable `num'
  *)
  let digits_of_num num = String.length (string_of_big_int num);;

  (** print_list: big_int list -> unit list *)
  let print_list l = List.map (fun x -> Printf.printf "%s\n%!" (string_of_big_int x)) l;;

  (** print_tuple2: (int * int) list -> unit list *)
  let print_tuple2 l = List.map (fun (x,y) -> Printf.printf "(%d, %d)\n%!" x y) l;;

  let print_tuple2_b l = 
    List.map (fun (x,y) -> Printf.printf "(%s, %s)\n%!" (string_of_big_int x) (string_of_big_int y)) l;;

  (** rev_tl: a' list -> a' list *)
  let rev_tl = function
    | [] -> []
    | l -> List.rev (List.tl (List.rev l))
  ;;
    
  (** solver: big_int -> big_int list *)
  let solver expansion = 
    let num_table = Hashtbl.create expansion 
    and den_table = Hashtbl.create expansion in
    let fs = List.map (fraction num_table den_table) (numbers expansion) in
    let ls = List.map (fun (n, d) -> (digits_of_num n, digits_of_num d)) fs in
    List.filter (fun (n, d) -> n > d) ls
  ;;
    
  let test () =
    let candidates = solver 1000 in
    Printf.printf "# of candidates -> %d\n%!" (List.length candidates)
  ;;
  
end

let () = A.test ();;
