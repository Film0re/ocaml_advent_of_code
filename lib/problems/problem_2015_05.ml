let year = 2015
let day = 5

(* quite the potato implementation but for now it gets the job done :\ *)
module Part_1 = struct
  type naughty_or_nice = Naughty | Nice

  let count_characters str characters_to_look_for =
    String.fold_left
      (fun count c ->
        if String.contains characters_to_look_for c then count + 1 else count)
      0 str

  let count_vowls str = count_characters str "aeiou"
  let contains_three_vowels str = count_vowls str >= 3

  let contains_same_letters_in_a_row str =
    let rec helper list_of_chars =
      match list_of_chars with
      | l1 :: l2 :: t -> if l1 = l2 then true else helper (l2 :: t)
      | [ _ ] | [] -> false
    in
    str |> String.to_seq |> List.of_seq |> helper

  let contains_no_naughty_strs str =
    let naughty_strs = [ "ab"; "cd"; "pq"; "xy" ] in
    let rec contains_naughty_strs_helper = function
      | c1 :: c2 :: t ->
          if
            List.exists
              (fun naughty -> naughty = String.make 1 c1 ^ String.make 1 c2)
              naughty_strs
          then false
          else contains_naughty_strs_helper (c2 :: t)
      | [ _ ] | [] -> true
    in

    str |> String.to_seq |> List.of_seq |> contains_naughty_strs_helper

  let predicates =
    [
      contains_no_naughty_strs;
      contains_same_letters_in_a_row;
      contains_three_vowels;
    ]

  let all_predicates value predicates =
    List.for_all (fun pred -> pred value) predicates

  let classify_string str =
    if all_predicates str predicates then Nice else Naughty

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
