(****************************************************************
 * Utility Library for Algorithm and Programming Quiz
 *
 * created by Yoshifumi YAMAGUCHI, 2010
 ***************************************************************)


(*********************************************************** 
 ***************************************** Basic operation 
 ***********************************************************)

val pow : int -> int -> int
(** [Euler.pow x n] is [n]th power of [x]. *)

val fact_aux : int -> int -> Big_int.big_int
(** [Euler.fact_aux n m] returns [m] partial factrial of [n]  *)

val fact : int -> Big_int.big_int
(** [Euler.fact n] is factorial of [n]. *)

val sqrt : int -> int
(** [Euler.sqrt n] is square root of [n]. *)

(**
 * Basic list operations 
 **)

val rec_map : ('a -> 'b) -> 'a list -> 'b list
  (** [Euler.rec_map f [a1; ...; an] ] applies function [f] to [a1, ..., an],
      and builds the list [f a1; ...; f an] with the results returned by [f].
      This is the recursive version of List.map. *)

val bottom : 'a list -> 'a
  (** [Euler.bottom l] returns bottom element of 'a list [l] *)

val range : int -> int -> int list
  (** [Euler.range s l] creates interger list start from [s] with length [l] *)

val max : 'a list -> 'a
  (** [Euler.max l] find max value in list [l] *)

val min : 'a list -> 'a
  (** [Euler.min l] find min value in list [l] *)


(***********************************************************
 ******************************** Basic algebric functions 
 ***********************************************************)

val sift : int -> int list -> int list
  (** [Euler.sift n l] returns list of eleements in int list [l] whose elements
     are not devied with int [n] *)

val sieve : int list -> int list
  (** [Euler.sieve l] implements Eratosthenes' sieve, which creates a list of
     prime numbers *)

val primes : int -> int list
  (** [Euler.primes n] is the list of prime numbers up to [n]. *)

val gcd : int -> int -> int
  (** [Euler.gcd m n] calculates gratest common devider. 
      [m] shoubld be greater than [n]. If not, swapping step will happen
      and cause extra step *)

val lcm : int -> int -> int
  (** [Euler.lcm m n] calculates least common multiple.
      lcm uses gcd's result using gcd(a,b) x lcm(a,b) = axb *)

val perm : int -> 'a list -> 'a list list
  (** [Euler.perm n l] generates nth permutation from l *)

(* val comb : int -> 'a list -> 'a list array *)
  (** [Euler.comb n l] generates nth combination from l *)

(***********************************************************
 ***************************************** Print functions 
 ***********************************************************)

val print_int_list : int list -> unit

val print_string_list : string list -> unit
