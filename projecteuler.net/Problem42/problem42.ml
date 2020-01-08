module A = struct
  
  open Str

  let filename = "words.txt";;

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

  let words = List.map (fun x -> quoted x) (elements_in filename);;

  let triangles limit = 
    let rec tris ret = function
      | 0 -> ret
      | n -> tris ((n*(n+1)/2)::ret) (n-1)
    in
    tris [] limit
  ;;


  let calc_score words =
    let word_score str =
      let max_idx = (String.length str) - 1 in
      let score_of_char c = (int_of_char c) - (int_of_char 'A') + 1 in
      let rec score ret = function
        | (-1) -> ret
        | n -> score ((score_of_char str.[n])+ret) (n-1)
      in
      score 0 max_idx
    in
    let rec calc_score ret = function
      | [] -> ret
      | n::ns ->
          calc_score ((word_score n)::ret) ns
    in
    calc_score [] words
  ;;


  let result = 
    let ts = triangles 1000 in
    List.filter (fun x -> List.mem x ts) (calc_score words)
  ;;


  let test () =
    begin
      Printf.printf "%d\n" (List.length result);
      ();
    end
  ;;

end

let () = A.test ();;
