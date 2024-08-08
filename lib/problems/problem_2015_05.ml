let year = 2015
let day = 5

module Part_1 = struct
  type naughty_or_nice = Naughty | Nice

  let count_characters str characters_to_look_for =
    String.fold_left
      (fun count c ->
        if String.contains characters_to_look_for c then count + 1 else count)
      0 str

  let count_vowls str = count_characters str "aiou"
  let contains_three_vowels str = count_vowls str >= 3

  let contains_duplicate_letter str =
    let module CharSet = Set.Make (Char) in
    let char_set = String.to_seq str |> CharSet.of_seq in
    CharSet.cardinal char_set < String.length str

  (* TODO: Finish implmementation *)
  let does_not_contains_bad_strs str = false && String.length str = 0

  let classify_string str =
    if
      contains_three_vowels str
      && contains_duplicate_letter str
      && does_not_contains_bad_strs str
    then Nice
    else Naughty

  let run (input : string) : (string, string) result =
    let input_list = String.split_on_char '\n' input in
    List.fold_left
      (fun count s ->
        match classify_string s with Nice -> count + 1 | Naughty -> count)
      0 input_list
    |> string_of_int |> Result.ok
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
