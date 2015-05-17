type t = int * int * int

type query = QueryPatch of int * int * int
           | QueryMinor of int * int
           | QueryMajor of int

type version_part = [
  | `Major
  | `Minor
  | `Patch
]

val compare : t -> t -> int

val increment_version : version_part -> t -> t
val decrement_version : version_part -> t -> t

val of_string : string -> t
val to_string : t -> string

val query_version : query -> t list -> t option

val parse_query : string -> query
val print_query : query -> string

val query : string -> t list -> t option
