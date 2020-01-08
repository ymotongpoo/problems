module A = struct
  
  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( - ) = sub_big_int;;
  let ( / ) = div_big_int;;
  let (mod) = mod_big_int;;
  let ( = ) = eq_big_int;;
  let ( > ) = gt_big_int;;
  let ( < ) = lt_big_int;;

  let bi = big_int_of_int;;
  let ib = int_of_big_int;;
  let si = string_of_big_int;;

  let rec pow x = function 
    | n when n=zero_big_int -> unit_big_int
    | n -> x * (pow x (n-unit_big_int))
  ;;

  let num2digits num =
    let len = bi (String.length (si num)) - unit_big_int in
    let rec n2d ret rest = function
      | n when n < zero_big_int -> List.rev ret
      | n ->
          let d = rest / (pow (bi 10) n) in
          n2d (d::ret) (rest - d * (pow (bi 10) n)) (n-unit_big_int)
    in
    n2d [] num len
  ;;

  let digital_sum a b =
    let digits = num2digits (pow a b) in
    List.fold_left (+) zero_big_int digits
  ;;

  let find_max limit =
    let limit' = bi limit in
    let rec find ret a b =
      match a, b with
      | a, _ when a=limit' -> ret
      | _, b when b=limit' -> find ret (a+unit_big_int) unit_big_int
      | a, b ->
          let cur = digital_sum a b in
          if cur > ret then find cur a b
          else find ret a (b+unit_big_int)
    in
    find zero_big_int unit_big_int unit_big_int
  ;;
          
  
  let test () = 
    begin
      Printf.printf "%s\n" (si (pow (bi 10) (bi 3)));
      let l = List.map (fun x -> Printf.printf "%d " (ib x)) (num2digits (pow (bi 2) (bi 6))) in
      Printf.printf "\n";
      Printf.printf "%s\n" (si (digital_sum (bi 2) (bi 6)));
      Printf.printf "\n";
      Printf.printf "%s\n" (si (find_max 100));
      l;
    end
  ;;
  
end

let _ = A.test ();;
