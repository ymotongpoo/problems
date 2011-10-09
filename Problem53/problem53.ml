module A = struct

  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( - ) = sub_big_int;;
  let ( / ) = div_big_int;;
  let ( = ) = eq_big_int;;
  let ( > ) = gt_big_int;;
  let bi = big_int_of_int;;

  let fact n =
    let rec fact ret = function
      | n when n=zero_big_int -> ret
      | n -> fact (ret*n) (n - unit_big_int)
    in
    fact unit_big_int n
  ;;

  let comb n r = ((fact n) / (fact r)) / (fact (n-r));;

  let roller limit max =
    let rec roller ret n r =
      match n, r with
      | n, _ when n > max -> ret
      | _, r when r > n -> roller ret (n + unit_big_int) unit_big_int
      | _, _ ->
          let v = comb n r in
          if v > limit then roller (v::ret) n (r + unit_big_int)
          else roller ret n (r + unit_big_int)
    in
    roller [] unit_big_int unit_big_int
  ;;

  let test () =
    begin
      let result = roller (bi 1000000) (bi 100) in
      List.map (fun x -> Printf.printf "%s " (string_of_big_int x)) result;
      Printf.printf "%d\n" (List.length result);
    end
  ;;

end

let () = A.test ();;
