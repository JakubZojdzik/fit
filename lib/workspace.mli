type workspace_path = string
type 'a result = 'a Result_monad.t

(* workspace_path -> file_state *)
val read_state : workspace_path -> File.state result

(* workspace_path file_state -> unit *)
val write_state : workspace_path -> File.state -> unit result

(* workspace_path rel_path -> text_content *)
val read_file : workspace_path -> string -> File.text_content result

(* workspace_path rel_path content -> unit *)
val write_file : workspace_path -> string -> File.text_content -> unit result

(* reverse workspace_path diff -> unit *)
val apply_diff : bool -> workspace_path -> Diff.t -> unit result
