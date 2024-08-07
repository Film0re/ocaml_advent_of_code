let year = 2015
let day = 4

module Make_Advent_Coin (Args : sig
  val zeros : string
end) =
struct
  let valid_hash (str : string) =
    str |> Digest.string |> Digest.to_hex |> fun digest ->
    String.sub digest 0 (String.length Args.zeros) = Args.zeros

  let advent_coin_hash str =
    let rec advent_coin_helper str num =
      let current_advent_coin = str ^ string_of_int num in
      if valid_hash current_advent_coin then num
      else advent_coin_helper str (num + 1)
    in
    advent_coin_helper str 0

  let run (input : string) : (string, string) result =
    input |> String.trim |> advent_coin_hash |> string_of_int |> Result.ok
end

module Part_1 = Make_Advent_Coin (struct
  let zeros = "00000"
end)

module Part_2 = Make_Advent_Coin (struct
  let zeros = "000000"
end)
