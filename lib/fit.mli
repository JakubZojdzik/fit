module Result = Result_monad
module Diff = Diff
module Commit = Commit
module Node = Node
module Cli = Cli

type repository = { path : string; history : History.t }
type 'a result = 'a Result_monad.t

(* path -> repository *)
val init : string -> repository result

(* path -> repository *)
val open_repo : string -> repository result

(* repository message author -> repository *)
val commit : repository -> string -> string -> repository result

(* repository target -> repository *)
val checkout : repository -> string -> repository result

(* repository commit_id -> repository *)
val revert : repository -> string -> repository result

(* file_path -> unit *)
val restore : repository -> string -> unit result

(* repository branch_name -> repository *)
val create_branch : repository -> string -> repository result

(* repository -> branch_names *)
val list_branches : repository -> string list

(* repository branch message author -> repository *)
val merge : repository -> string -> string -> string -> repository result

(* repository -> nodes *)
val log : repository -> Node.t list

(* repository -> status_str *)
val status : repository -> string

(* repository -> diff *)
val diff : repository -> Diff.t result

(* command -> exit_code *)
val run_command : Cli.command -> int
