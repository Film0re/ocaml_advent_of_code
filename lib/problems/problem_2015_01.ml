let year = 2015
let day = 1

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let floor =
      String.fold_left
        (fun acc c -> match c with '(' -> acc + 1 | ')' -> acc - 1 | _ -> acc)
        0 input
    in
    Ok (string_of_int floor)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let rec basement position level =
      if level = -1 then position
      else
        match input.[position] with
        | '(' -> basement (position + 1) (level + 1)
        | ')' -> basement (position + 1) (level - 1)
        | _ -> basement (position + 1) level
    in
    let position = basement 0 0 in
    Ok (string_of_int position)
end
