module A = struct

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let numbers limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let num2digits num =
    let len = (String.length (string_of_int num)) - 1 in
    let rec n2d ret rest = function
      | n when n < 0 -> List.rev ret
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d * (pow 10 n)) (n-1)
    in
    n2d [] num len
  ;;

  let double_sixth n =
    let times = [2;3;4;5;6] in
    List.map (fun x -> num2digits (n*x)) times
  ;;

  let rec is_same_digits l rs =
    let rec sweep l r =
      match l, r with
      | [], [] -> true
      | [], _ | _, [] -> false
      | l::ls, rs ->
          sweep ls (List.filter (fun x -> x <> l) rs)
    in
    match rs with
    | [] -> true
    | r::rs -> 
        if sweep l r then is_same_digits l rs
        else false
  ;;
  
  let rec find = function
    | [] -> 0
    | n::ns -> 
        if is_same_digits (num2digits n) (double_sixth n)
        then n
        else find ns
  ;;
  

  let test () =
    begin
      Printf.printf "%d\n" (find (numbers 1000000));
    end
  ;;

end

let () = A.test ();;
