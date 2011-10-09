(**
 * Problem 65
 *)

module A : sig

  type fraction

  val eapprox : int -> fraction -> fraction -> fraction
    (** [eapprox index prev cur] returns approx fraction whose
        position is in index *)

  val econvergent : int -> fraction
    (** [econvergent limit] returns approx fraction
        on index of limit *)

  val sum_of_numerator : fraction -> int
    (** [sum_of_numerator f] returns sum of numerator of f *)

  val test : unit -> unit

end = struct

  open Big_int

  type fraction = Fraction of big_int * big_int;;

  let (/~) = div_big_int;;
  let ( *~ ) = mult_big_int
  let (+~) = add_big_int
  let (-~) = sub_big_int

  let eapprox i p c =
    match i, p, c with
    | i, Fraction (pn, pd), Fraction (cn, cd) when i mod 3 = 0 -> 
        let n = (big_int_of_int i) /~ (big_int_of_int 3) *~ (big_int_of_int 2) in 
        let num = cn *~ n +~ pn
        and den = cd *~ n +~ pd in
        Fraction (num, den)
    | _, Fraction (pn, pd), Fraction (cn, cd) ->
        let num = pn +~ cn
        and den = pd +~ cd in
        Fraction (num, den)
  ;;

  let n1 = Fraction (big_int_of_int 2, big_int_of_int 1)
  and n2 = Fraction (big_int_of_int 3, big_int_of_int 1);;

  let econvergent limit =
    let rec econvergent_aux i p c =
      match i, p, c with
      | i, _, _ when i = limit ->
          eapprox i p c 
      | _, _, _ ->
          econvergent_aux (succ i) c (eapprox i p c)
    in
    econvergent_aux 3 n1 n2
  ;;

  let print_fraction = function 
      Fraction (n, d) ->
        Printf.printf "%s/%s\n" (string_of_big_int n) (string_of_big_int d)
  ;;

  let sum_of_numerator = function
    | Fraction (n, _) ->
        let stringn = string_of_big_int n in
        let length = String.length stringn in
        let sum = ref 0 in
        for i = 0 to length - 1 do
          let n = int_of_char stringn.[i] - int_of_char '0' in
          sum := !sum + n
        done;
        !sum
  ;;

  let test () = 
    let limit = 100 in
    let p = econvergent limit in
    let _ = print_fraction p in
    Printf.printf "%d\n" (sum_of_numerator p)
  ;;

end;;

A.test ();;
