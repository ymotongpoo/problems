module A = struct

  let nums n =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] n
  ;;

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let d2n ds = 
    let len = List.length ds in
    List.fold_left2 (fun x y z -> x + y * (pow 10 (z-1))) 0 ds (nums len)
  ;;

  let n2d n =
    let len = String.length (string_of_int n) - 1 in
    let rec n2d ret rest = function
      | 0 -> rest::ret
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d * (pow 10 n)) (n-1)
    in
    n2d [] n len
  ;;

  let is_lychrel max_itr num =
    let rev_num num = d2n (List.rev (n2d num)) in
    let rev_add num = num + rev_num num in
    let rec is_parindrome ctr num = 
      match ctr, num with
      | (-1), _ -> false
      | _, _ when ctr = max_itr -> is_parindrome (ctr-1) (rev_add num)
      | _, num when num = d2n (List.rev (n2d num)) -> true
      | _, _ -> is_parindrome (ctr-1) (rev_add num)
    in
    not (is_parindrome max_itr num)
  ;;

  let find_lychrel max_itr ns =
    List.filter (fun x -> is_lychrel max_itr x) ns
  ;;

  let test () =
    begin
      let result = find_lychrel 50 (nums 10000) in
      let _ = List.map (fun x -> Printf.printf "%d " x) result in
      Printf.printf "\n";
      let _ = Printf.printf "%d\n" (List.length result) in
      ();
    end
  ;;

end

let () = A.test ();;
