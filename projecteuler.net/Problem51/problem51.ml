(**
 * Problem 51
 * 
 * By replacing the 1st digit of *3, it turns out that six of the nine possible values:
 * 13, 23, 43, 53, 73, and 83, are all prime.
 *
 * By replacing the 3rd and 4th digits of 56**3 with the same digit,
 * this 5-digit number is the first example having seven primes among
 * the ten generated numbers, yielding the family:
 * 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
 * Consequently 56003, being the first member of this family,
 * is the smallest prime with this property.
 * 
 * Find the smallest prime which, by replacing part of the number
 * (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
 *
 *)

module A : sig

  val test : unit -> unit

end = struct

  let primes = Euler.primes 1000000;;
                            
  let rec find_smallest = function
    | [] -> failwith "Empty"
    | [x] -> x
    | x::xs -> min x (find_smallest xs)
  ;;

  let test () = 
    let () = Printf.printf "start calc\n" in
    (* let _ = Euler.rec_map (fun x -> Printf.printf "%d%!\n" x) primes in *)
    Printf.printf "%d\n" (find_smallest primes)
  ;;
 
end

let () = A.test ()
