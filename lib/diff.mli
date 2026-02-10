type file_diff = { path : string; patch : string }
type t = file_diff list

val empty : t

(* diff -> is_empty *)
val is_empty : t -> bool

(* path old_content new_content -> file_diff *)
val compute_file_diff :
  string -> File.text_content -> File.text_content -> file_diff

(* old_state new_state -> diff *)
val compute_state_diff : File.state -> File.state -> t

(* reverse file_diff content -> new_content *)
val apply_file_diff :
  bool -> file_diff -> File.text_content -> File.text_content

(* diff -> str *)
val to_string : t -> string
