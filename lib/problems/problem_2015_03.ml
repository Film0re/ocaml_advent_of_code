let year = 2015
let day = 3

module IntPairSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end)

module Part_1 = struct
  let explode s =
    let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []

  let get_new_cords x y = function
    | '>' -> Some (x + 1, y)
    | '<' -> Some (x - 1, y)
    | '^' -> Some (x, y + 1)
    | 'v' -> Some (x, y - 1)
    | _ -> Some (x, y)

  let rec get_number_of_houses x y list set =
    match list with
    | [] -> set
    | h :: t -> (
        let new_set = IntPairSet.add (x, y) set in
        match get_new_cords x y h with
        | Some (new_x, new_y) -> get_number_of_houses new_x new_y t new_set
        | None -> failwith "Something went horribly wrong")

  let run (input : string) : (string, string) result =
    let set = IntPairSet.empty in
    let list_of_string = explode input in
    get_number_of_houses 0 0 list_of_string set
    |> IntPairSet.cardinal |> string_of_int |> Result.ok
end

module Part_2 = struct
  let explode s =
    let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []

  let get_new_cords (x, y) = function
    | '>' -> Some (x + 1, y)
    | '<' -> Some (x - 1, y)
    | '^' -> Some (x, y + 1)
    | 'v' -> Some (x, y - 1)
    | _ -> Some (x, y)

  type coord = int * int
  type santa_or_robot = Santa | Robot

  let rec get_number_of_houses (santa : coord) (robot : coord) list set turn =
    match list with
    | [] -> set
    | h :: t -> (
        let new_set = set |> IntPairSet.add santa |> IntPairSet.add robot in
        match (turn : santa_or_robot) with
        | Santa -> (
            match get_new_cords santa h with
            | Some cords -> get_number_of_houses cords robot t new_set Robot
            | None -> failwith "aaaaaaaaaaah")
        | Robot -> (
            match get_new_cords robot h with
            | Some cords -> get_number_of_houses santa cords t new_set Santa
            | None -> failwith "aaaaaaaaaaah"))

  let run (input : string) : (string, string) result =
    let set = IntPairSet.empty in
    let list_of_string = explode input in
    get_number_of_houses (0, 0) (0, 0) list_of_string set Robot
    |> IntPairSet.cardinal |> string_of_int |> Result.ok
end
