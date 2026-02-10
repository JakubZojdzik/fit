type commit_id = string

type metadata = {
  id : commit_id;
  message : string;
  author : string;
  timestamp : float;
}

type t

(* diff message author -> commit *)
val create : Diff.t -> string -> string -> t

(* message author -> commit *)
val initial : string -> string -> t

(* commit -> id *)
val id : t -> commit_id

(* commit -> metadata *)
val metadata : t -> metadata

(* commit -> diff *)
val diff : t -> Diff.t

(* commit state -> new_state *)
val apply : t -> File.state -> File.state

(* commit -> str *)
val to_string : t -> string
