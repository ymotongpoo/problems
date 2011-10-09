module A = struct
  
  open Str

  let filename = "names.txt";;

  let elements_in filenames =
    let ic = open_in filename in
    try
      let line = input_line ic in
      split (regexp ",") line
    with e ->
      close_in_noerr ic;
      raise e
  ;;

  let quoted elem = global_replace (regexp "\"") "" elem;;

  let calc_score names =
    let name_score str =
      let max_idx = (String.length str) - 1 in
      let score_of_char c = (int_of_char c) - (int_of_char 'A') + 1 in
      let rec score ret = function
        | (-1) -> ret
        | n -> score ((score_of_char str.[n])+ret) (n-1)
      in
      score 0 max_idx
    in
    let rec calc_score ret nth = function
      | [] -> ret
      | n::ns ->
          calc_score ((nth*(name_score n))::ret) (nth+1) ns
    in
    calc_score [] 1 names
  ;;

  let test () =
    begin
      let names = List.sort compare 
        (List.map (fun x -> quoted x) (elements_in filename)) in
      let scores = calc_score names in
      List.map (fun x -> Printf.printf "%s " x) names;
      Printf.printf "%d\n" (List.fold_left (+) 0 scores);
      ();
    end
  ;;

end

let () = A.test ();;
