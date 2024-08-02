let year = 2015
let day = 2

module Part_1 = struct
  let prism_area l w h = 2 * ((l * w) + (w * h) + (h * l))
  let min_area l w h = min (l * w) (min (w * h) (h * l))

  let run (input : string) : (string, string) result =
    let input = String.trim input in
    let input_list = String.split_on_char '\n' input in
    let total_area =
      List.fold_left
        (fun acc unsplit_string ->
          match String.split_on_char 'x' unsplit_string with
          | [ l; w; h ] ->
              let l = int_of_string l in
              let w = int_of_string w in
              let h = int_of_string h in

              prism_area l w h + min_area l w h + acc
          | _ -> failwith "Error on splitting string")
        0 input_list
    in
    Ok (string_of_int total_area)
end

module Part_2 = struct
  let min_two_of_three a b c =
    let sorted = List.sort compare [ a; b; c ] in
    match sorted with x :: y :: _ -> Some (x, y) | _ -> None

  let bow_ribbon l w h = l * w * h

  let wrap_ribbon l w h =
    match min_two_of_three l w h with
    | Some (min_one, min_two) -> min_one + min_one + min_two + min_two
    | None -> 0

  let get_dimensions str =
    match String.split_on_char 'x' str with
    | [ l; w; h ] -> (
        match
          (int_of_string_opt l, int_of_string_opt w, int_of_string_opt h)
        with
        | Some l, Some w, Some h -> Some (l, w, h)
        | _ -> None)
    | _ -> None

  let run (input : string) : (string, string) result =
    let input = String.trim input in
    let input_list = String.split_on_char '\n' input in
    let total_area =
      List.fold_left
        (fun acc unsplit_string ->
          match get_dimensions unsplit_string with
          | Some (l, w, h) -> acc + bow_ribbon l w h + wrap_ribbon l w h
          | _ -> failwith "Invalid Input format")
        0 input_list
    in
    Ok (string_of_int total_area)
end
