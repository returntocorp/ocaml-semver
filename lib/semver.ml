open Printf

type t = int * int * int

type query =
  | QueryPatch of int * int * int
  | QueryMinor of int * int
  | QueryMajor of int

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

let increment_version p v = match p, v with
  | `Major, (l1, _, _) -> (l1 + 1, 0, 0)
  | `Minor, (l1, l2, _) -> (l1, l2 + 1, 0)
  | `Patch, (l1, l2, l3) -> (l1, l2, l3 + 1)

let decrement_version p v = match p, v with
  | `Major, (l1, _, _) -> (l1 - 1, 0, 0)
  | `Minor, (l1, l2, _) -> (l1, l2 - 1, 0)
  | `Patch, (l1, l2, l3) -> (l1, l2, l3 - 1)

let of_string input =
  match Scanf.sscanf input "%d.%d.%d" (fun v1 v2 v3 -> (v1, v2, v3)) with
  | v -> Some v
  | exception _ -> None

let to_string (v1, v2, v3) = sprintf "%d.%d.%d" v1 v2 v3

(* Should this just expect a sorted list instead of sorting it itself? *)
let query_version query versions =
  let last ls = List.nth ls (List.length ls - 1) in
  let compareQuery q v = match q, v with
    | QueryPatch (maj, min, patch), v' -> (maj, min, patch) == v'
    | QueryMinor (maj, min), (v1, v2, _) -> maj == v1 && min == v2
    | QueryMajor maj, (v1, _, _) -> maj == v1 in
  let res = List.sort compare (List.filter (compareQuery query) versions) in
  if res == [] then None else Some (last res)

let parse_query input =
  try Scanf.sscanf input "%d.%d.%d" (fun v1 v2 v3 -> QueryPatch (v1, v2, v3))
  with End_of_file ->
    try Scanf.sscanf input "%d.%d" (fun v1 v2 -> QueryMinor (v1, v2))
    with End_of_file -> Scanf.sscanf input "%d" (fun v1 -> QueryMajor v1)

let print_query = function
  | QueryPatch (maj, min, patch) -> sprintf "%d.%d.%d" maj min patch
  | QueryMinor (maj, min) -> sprintf "%d.%d" maj min
  | QueryMajor maj -> sprintf "%d" maj

let query q vs = query_version (parse_query q) vs
