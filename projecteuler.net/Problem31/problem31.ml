(** -*- coding: utf-8 -*- *)
(**
Problem 31

In England the currency is made up of pound, £, and pence, p, 
and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
*)


module A : sig

  type status
    (** (rest, coin) *)

  type tree
    (** ( (rest, coin), [ ( (rest1, coin1), [children1] ), ... ] *)

  val coins : int list
    (** list of possible coins *)

  val create_tree : int list -> status -> tree
    (** [create_tree coins status] returns all possible patterns
        start with [status] and possible [coins] *)

  val count_pattern : tree -> int
    (** [count_pattern tree] returns number of all possible patterns
        in the tree *)

  val test : unit -> unit

end = struct

  type status = Status of int * int

  type tree = Empty | Tree of status * tree list

  let coins = [1; 2; 5; 10; 20; 50; 100; 200]

  let total = 200


  let rec create_tree coins = function      
    | Status(0, _) -> Empty
    | Status(r, c) as s -> 
        let min = Euler.min [r; c] in
        let candidates = List.filter (fun x -> x <= min) coins in
        let create_child x =
          let child = Status(r-x, x) in
          create_tree coins child
        in
        let children = List.map create_child candidates in
        Tree(s, children)
  ;;

  let rec count_pattern = function
    | Empty -> 1
    | Tree(_, children) -> 
        List.fold_left (fun accu t -> accu + (count_pattern t)) 0 children
  ;;

  let test () = 
    let max = Euler.max coins + 1 in
    let start = Status(total, max) in
    let tree = create_tree coins start in
    Printf.printf "total patterns : %d%!\n" (count_pattern tree)
  ;;

end

let _ = A.test ()
