open Printf

type t = int * int * int

type version_part = [
  | `Major
  | `Minor
  | `Patch
]

let compare (l1, l2, l3) (r1, r2, r3) =
  match compare l1 r1, compare l2 r2, compare l3 r3 with
  | 0, 0, res -> res
  | 0, res, _ -> res
  | res, _, _ -> res

let succ p v = match p, v with
  | `Major, (l1, _, _) -> (l1 + 1, 0, 0)
  | `Minor, (l1, l2, _) -> (l1, l2 + 1, 0)
  | `Patch, (l1, l2, l3) -> (l1, l2, l3 + 1)

let pred p v = match p, v with
  | `Major, (l1, _, _) -> (l1 - 1, 0, 0)
  | `Minor, (l1, l2, _) -> (l1, l2 - 1, 0)
  | `Patch, (l1, l2, l3) -> (l1, l2, l3 - 1)

let of_string input =
  match Scanf.sscanf input "%d.%d.%d" (fun v1 v2 v3 -> (v1, v2, v3)) with
  | v -> Some v
  | exception _ -> None

let to_string (v1, v2, v3) = sprintf "%d.%d.%d" v1 v2 v3

module Query = struct
  type t = [
    | `Patch of int * int * int
    | `Minor of int * int
    | `Major of int
  ]

  let of_string input =
    try Some (Scanf.sscanf input "%d.%d.%d" (fun v1 v2 v3 -> `Patch (v1, v2, v3)))
    with End_of_file ->
      try Some (Scanf.sscanf input "%d.%d" (fun v1 v2 -> `Minor (v1, v2)))
      with End_of_file ->
        try
          Some (Scanf.sscanf input "%d" (fun v1 -> `Major v1))
        with Scanf.Scan_failure _ -> None

  let to_string = function
    | `Patch (maj, min, patch) -> sprintf "%d.%d.%d" maj min patch
    | `Minor (maj, min) -> sprintf "%d.%d" maj min
    | `Major maj -> sprintf "%d" maj
end

(* Should this just expect a sorted list instead of sorting it itself? *)
let query query versions =
  let last ls = List.nth ls (List.length ls - 1) in
  let compareQuery q v = match q, v with
    | `Patch (maj, min, patch), v' -> (maj, min, patch) == v'
    | `Minor (maj, min), (v1, v2, _) -> maj == v1 && min == v2
    | `Major maj, (v1, _, _) -> maj == v1 in
  let res = List.sort compare (List.filter (compareQuery query) versions) in
  if res == []
  then None
  else Some (last res)

let query_str q =
  match Query.of_string q with
  | None -> invalid_arg "query_str: q is not a query"
  | Some q -> query q
