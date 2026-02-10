type storage_path = string
type 'a result = 'a Result_monad.t

(* path -> unit *)
val init : storage_path -> unit result

(* path history -> unit *)
val save : storage_path -> History.t -> unit result

(* path -> history *)
val load : storage_path -> History.t result
