(**
 *  Problem 112
 *
 *  Working from left-to-right if no digit is exceeded by the digit
 *  to its left it is called an increasing number; for example, 134468.
 *
 *  Similarly if no digit is exceeded by the digit to its right
 *  it is called a decreasing number; for example, 66420.
 *
 *  We shall call a positive integer that is neither increasing
 *  nor decreasing a "bouncy" number; for example, 155349.
 *
 *  Clearly there cannot be any bouncy numbers below one-hundred,
 *  but just over half of the numbers below one-thousand (525) are bouncy.
 *  In fact, the least number for which the proportion of bouncy numbers
 *  first reaches 50% is 538.
 *
 *  Surprisingly, bouncy numbers become more and more common
 *  and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.
 *
 *  Find the least number for which the proportion of bouncy numbers is exactly 99%.
 *)


module A : sig

  val is_directional : (int -> bool) -> int -> bool
  val is_increasing : int -> bool
  val is_decreasing : int -> bool

  val test : unit -> unit

end = struct

  let threshold = 0.99

  let is_directional f n = 
    let strn = string_of_int n in
    let len = String.length strn in
    let subs = ref [] in
    let () = 
      for i = 0 to len - 2 do
        let s = (int_of_char strn.[i+1]) - (int_of_char strn.[i]) in
        subs := s::!subs
      done
    in
    List.for_all f !subs
  ;;

  let is_increasing = function 
    | n when n < 10 -> true
    | n -> is_directional (fun x -> x >= 0) n
  ;;

  let is_decreasing = function
    | n when n < 10 -> true
    | n -> is_directional (fun x -> x <= 0) n
  ;;

  let solver threshold = 
    let rec solver accu cur =
      match accu, cur with
      | accu, cur when (float_of_int accu) /. (float_of_int (cur - 1)) >= threshold ->
          cur - 1
      | _, cur when (is_increasing cur) || (is_decreasing cur) ->
          solver accu (succ cur)
      | _, cur ->
          solver (succ accu) (succ cur)
    in
    solver 0 1
  ;;

  let test () =
    begin
      Printf.printf "%b\n%!" (is_increasing 123456);
      Printf.printf "%b\n%!" (is_increasing 654321);
      Printf.printf "%b\n%!" (is_decreasing 654321);
      Printf.printf "%b\n%!" (is_decreasing 123456);
      Printf.printf "%d\n%!" (solver 0.5);
      Printf.printf "%d\n%!" (solver 0.9);
      Printf.printf "%d\n%!" (solver threshold);
    end
  
end


let _ = A.test ()
