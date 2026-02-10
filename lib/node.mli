type node_id = Commit.commit_id

type t

(* commit parent -> node *)
val create : Commit.t -> t option -> t

(* commit primary_parent secondary_parent -> merge_node *)
val create_merge : Commit.t -> t -> t -> t

(* node -> id *)
val id : t -> node_id

(* node -> commit *)
val commit : t -> Commit.t

(* node -> parent *)
val primary_parent : t -> t option

(* node -> children *)
val children : t -> t list

(* node -> str *)
val to_string : t -> string
