module A = struct

  let rec pow x = function
    | 0 -> 1
    | n -> x * pow x (n-1)
  ;;


  let nums limit = 
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;


  let n2d num =
    let len = String.length (string_of_int num) - 1 in
    let rec n2d ret rest = function
      | 0 -> List.rev ret
      | n -> 
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d * (pow 10 n)) (n-1)
    in
    n2d [] num len
  ;;


  let d2n ds =
    let len = List.length ds in
    List.fold_left2 (fun x y z -> x + y*(pow 10 (z-1))) 0 ds (List.rev (nums len))
  ;;


  let rec is_pandigital num =
    let singles = nums 9 in
    List.for_all (fun x -> List.mem x num) singles
  ;;


  let concat_mult_pand num =
    let rec concat_mult ret ns =
      match ret, ns with
      | _, [] -> 0
      | ret, _ when List.length ret > 9 -> 0
      | _, x::xs ->
          let prod = n2d (num*x) in
          let cur = ret @ prod in
          if List.length cur = 9 && is_pandigital cur
          then d2n cur
          else concat_mult cur xs
    in
    concat_mult [] (nums 9)
  ;;


  let roller limit =
    let rec roller max = function
      | n when n = limit -> max
      | n ->
          if concat_mult_pand n > max
          then roller (concat_mult_pand n) (n+1)
          else roller max (n+1)
    in
    roller 0 1
  ;;


  let test () =
    Printf.printf "%d\n" (roller 1000000)
  ;;


end

let _ = A.test ();;
