let rec numbers ret = function
    | 0 -> ret
    | n -> numbers (n::ret) (n-1)
;;

let sum_of_sqr_till n = 
  List.fold_left (+) 0 (List.map (fun x -> x*x) (numbers [] n));;

let sqr_of_sum_till n =
  let sum_till_n = List.fold_left (+) 0 (numbers [] n) in
  sum_till_n * sum_till_n
;;

Printf.printf "%d\n" (sqr_of_sum_till 100 - sum_of_sqr_till 100);;
        

