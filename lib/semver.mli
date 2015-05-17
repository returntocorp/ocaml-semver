(** Simple semantic versioning module *)

(** Semantic version consisting of major/minor/patch components *)
type t = int * int * int

type query = QueryPatch of int * int * int
           | QueryMinor of int * int
           | QueryMajor of int

(** Part labels *)
type version_part = [
  | `Major (** First part of version *)
  | `Minor (** Second part of version *)
  | `Patch (** Thrid part of version *)
]

(** Compare two versions *)
val compare : t -> t -> int

(** increment [vpart] [v] increments [vpart] component in [v] *)
val increment_version : version_part -> t -> t

(** decrement [vpart] [v] decrements [vpart] component in [v] *)
val decrement_version : version_part -> t -> t

(** Parse a semantic version from a string *)
val of_string : string -> t

(** Convert a semantiv version to a string *)
val to_string : t -> string

val query_version : query -> t list -> t option

val parse_query : string -> query
val print_query : query -> string

val query : string -> t list -> t option
