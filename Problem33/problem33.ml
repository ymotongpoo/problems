module A = struct

  let fi = float_of_int;;

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let n2d num =
    let len = String.length (string_of_int num) - 1 in
    let rec n2d ret rest = function
      | 0 -> rest::ret
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d * (pow 10 n)) (n-1)
    in
    n2d [] num len
  ;;
  
  let d2n ds =
    let len = List.length ds in
    List.fold_left2 (fun x y z -> x + y*(pow 10 (z-1))) 0 ds (nums len)
  ;;

  let find_funny base =
    let singles = nums 9 in
    let (smallers, biggers) = List.partition (fun x -> x < base) singles in
    let excepts = List.filter (fun x -> x <> base) singles in
    let rec find ret f init left right =
      match left, right with
      | _, [] -> ret
      | [], r::rs -> find ret f init init rs
      | l::ls, r::rs -> 
          if f l r then find ((l, r)::ret) f init ls (r::rs)
          else find ret f init ls (r::rs)
    in
    let big_match l r = 
      (fi r) /. (fi l) = (fi (d2n (base::[r]))) /. (fi (d2n (l::[base])))
    in
    let small_match l r =
      (fi l) /. (fi r) = (fi (d2n (l::[base]))) /. (fi (d2n (base::[r])))
    in
    (find [] big_match biggers biggers excepts) 
    @ (find [] small_match smallers smallers excepts)
  ;;

  let result = List.fold_left (fun x y -> (find_funny y) @ x) [] (nums 9);;

  let prod_result result =
    let rec prod (retd, retn) = function
      | [] -> (retd, retn)
      | (d, n)::rs -> prod (retd*d, retn*n) rs
    in
    prod (1, 1) result
  ;;
  
  let print_tuple (l, r) = Printf.printf "(%d, %d) " l r ;;
  
  let test () = 
    begin
      let _ = List.map (fun x -> print_tuple x) result in
      Printf.printf "\n";
      print_tuple (prod_result result);
      ();
    end
  ;;


end

let _ = A.test ();;
