(**
 * Problem 206
 *
 * Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
 * where each “_” is a single digit.
 *)

(**
 * Speculation
 *
 * from 1 to 9, there's no digit whose square ends with zero then 
 * the answer must be end with '0'. Thus, squqre of the answer
 * end with '00' and therefore tenth digit of the answer
 * must be '3' or '7', because from 1 to 9, those are the only digit whose
 * squares end with '9'.
 *
 * 1_2_3_4_5_6_7_8_9_0 is 19 digits number and square root of 2*10^18 is
 * almost 1414213562, which must be muximum number of candidates and 
 * 10^9 is minimum.
 *)

module A = struct

  open Big_int

  let (+~) = add_big_int
  let (-~) = sub_big_int
  let ( *~ ) = mult_big_int
  let (/~) = div_big_int

  let maximum = big_int_of_int 1414213562
  and minimum = power_int_positive_int 10 9

  let threshold = power_int_positive_int 10 18

  let pattern = "1234567890"

  let rec solver n =
    let verifier s =
      if lt_big_int s threshold then false 
      else
        let strs = string_of_big_int s in
        let len = (String.length strs) / 2 in
        let ret = String.create (len+1) in
        for i = 0 to len do
          ret.[i] <- strs.[2*i]
        done;        
        if ret = pattern then true else false
    in
    if verifier (square_big_int n) then n
    else 
      if gt_big_int n maximum then zero_big_int
      else
        solver (succ_big_int n)
  ;;
    
  let test () = 
    Printf.printf "%s\n%!" (string_of_big_int minimum);
    Printf.printf "%s\n%!" (string_of_big_int maximum);
    Printf.printf "%s\n%!" (string_of_big_int threshold);
    Printf.printf "%s\n%!" (string_of_big_int (solver minimum))
  ;;

end

let _ = A.test ()
