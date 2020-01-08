module A = struct
  let func n = 
    let rec func c = function
      | 1 -> c
      | n when (n mod 2) = 0 -> func (c+1) (n/2)
      | n -> func (c+1) (3*n+1)
    in
    func 0 n
  ;;
  
  let max_seq_under n =
    let rec max_seq nth v = function
      | t when t > n -> nth
      | t -> 
          if v < (func t) then max_seq t (func t) (t+1)
          else max_seq nth v (t+1)
    in
    max_seq 0 0 1
  ;;
    
  let test () =
    Printf.printf "%d\n" (max_seq_under 1000000)
  ;;
end

let () = A.test ();;
