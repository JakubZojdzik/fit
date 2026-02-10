type branch_name = string
type t
type 'a result = 'a Result_monad.t

(* -> empty_history *)
val empty : unit -> t

(* message author -> history *)
val init : string -> string -> t

(* history -> head_node *)
val head : t -> Node.t option

(* history -> branch_name *)
val current_branch : t -> branch_name option

(* history diff message author -> history *)
val commit : t -> Diff.t -> string -> string -> t result

(* history commit_id -> history *)
val checkout_commit : t -> Node.node_id -> t result

(* history branch_name -> history *)
val checkout_branch : t -> branch_name -> t result

(* history commit_id -> history *)
val revert_to : t -> Node.node_id -> t result

(* history branch_name -> history *)
val create_branch : t -> branch_name -> t result

(* history -> branch_names *)
val list_branches : t -> branch_name list

(* history branch_name message author -> history *)
val merge : t -> branch_name -> string -> string -> t result

(* history -> nodes *)
val log : t -> Node.t list

(* history -> file_state *)
val reconstruct_head_state : t -> File.state
