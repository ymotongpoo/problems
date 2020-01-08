(* -*- coding:utf-8 -*- *)

(** brute force version *)
module A = struct

  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( - ) = sub_big_int;;
  let ( / ) = div_big_int;;

  let sqrt = sqrt_big_int;;
  let square = square_big_int;;

  let has_sqrt n =
    let n' = sqrt n in
    if eq_big_int n (square n')
    then n'
    else zero_big_int
  ;;

  let has_ans nlimit d =
    let rec check = function
      | n when gt_big_int n nlimit -> zero_big_int
      | n ->
          let s = has_sqrt (d*(square n)+unit_big_int) in
          if eq_big_int s zero_big_int then check (succ_big_int n) else s
    in
    check unit_big_int
  ;;

  let solver nlimit dlimit =
    let rec find max maxd = function
      | d when gt_big_int d dlimit -> maxd
      | d when eq_big_int (has_sqrt d) zero_big_int ->
          let a = has_ans nlimit d in
          if lt_big_int max a
          then find a d (succ_big_int d)
          else find max maxd (succ_big_int d)
      | d -> find max maxd (succ_big_int d)
    in
    find zero_big_int zero_big_int unit_big_int
  ;;

  let test () =
    begin
      let nlimit = big_int_of_int 10000000
      and dlimit = big_int_of_int 1000 in
      let result = solver nlimit dlimit in
      Printf.printf "%s\n%!" (string_of_big_int result);
    end
  ;;

end

(** using the definision of Pell Equation version *)

module B = struct
  (**
     http://en.wikipedia.org/wiki/Pell's_equation
     http://en.wikipedia.org/wiki/Pell_number

     http://bal4u.dip.jp/mt/program/2004/08/pellequ.html

     general answer
     x(n) = x(0)*x(n-1) + M*y(1)*y(n-1)
     y(n) = x(0)*y(n-1) + y(1)*x(n-1)

     minimam answer
     s(0) = 0, t(0) = 1
     a(0) = (int) (sqrt M)
     x(-1) = 1, y(-1) = 0
     x(0) = a(0), y(0) = 1
     
     s(k+1) = a(k) * t(k) - s(k)
     t(k+1) = (M - s(k+1) ** 2) / t(k)
     a(k+1) = (int) (a(0) + s(k+1))/t(k+1)

     x(k+1) = a(k+1)*x(k) + x(k-1)
     y(k+1) = a(k+1)*y(k) + y(k-1)
  *)

  let sqrt_int x = sqrt (float_of_int x);;

  let min_ans d =
    let rec s = function
      | 0 -> 0
      | k -> (a k)*(t k) - (s (k-1))
    and t = function
      | 0 -> 1
      | k -> (d - (s k)*(s k)) / (t (k-1))
    and a = function
      | 0 -> int_of_float (sqrt_int d)
      | k -> int_of_float ((float_of_int (a 0) +. float_of_int (s k)) /. float_of_int (t k))
    in
    let rec x = function
      | (-1) -> 1
      | 0 -> a 0
      | k -> (a k)*(x (k-1)) + (x (k-2))
    and y = function
      | (-1) -> 0
      | 0 -> 1
      | k -> (a k)*(y (k-1)) + (y (k-2))
    in
    let rec solve k =
      if t k = 1 then (x k, y k) else solve (succ k)
    in
    solve 1
  ;;

  let print_int_pair (a, b) =
    Printf.printf "(%d, %d)\n" a b
  ;;
  
  let test () = 
    begin
      print_int_pair (min_ans 2);
      print_int_pair (min_ans 3);
      print_int_pair (min_ans 5);
      print_int_pair (min_ans 7);
    end
  ;;

end


let _ = B.test ();;
