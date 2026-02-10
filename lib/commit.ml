type commit_id = string

type metadata = {
  id : commit_id;
  message : string;
  author : string;
  timestamp : float;
}

type t = {
  meta : metadata;
  diff : Diff.t;
}

(* MD5 *)
let hash_string s =
  Digest.to_hex (Digest.string s)

let generate_id diff message author timestamp =
  let content = Printf.sprintf "%s|%s|%f|%s" 
    message 
    author 
    timestamp 
    (Diff.to_string diff) 
  in
  hash_string content

let create diff message author =
  let timestamp = Unix.gettimeofday () in
  let id = generate_id diff message author timestamp in
  let meta = { id; message; author; timestamp } in
  { meta; diff }

let initial message author =
  create Diff.empty message author

let id commit = commit.meta.id

let metadata commit = commit.meta

let diff commit = commit.diff

(* Map should be faster than list for bigger projects *)
let apply commit state =
  List.fold_left (fun current_state file_diff ->
    let path = file_diff.Diff.path in
    let old_content = 
      match List.assoc_opt path current_state with
      | Some c -> c
      | None -> []
    in
    let new_content = Diff.apply_file_diff false file_diff old_content in
    let state_without = List.remove_assoc path current_state in
    if new_content = [] then state_without
    else (path, new_content) :: state_without
  ) state commit.diff

let to_string commit =
  Printf.sprintf "COMMIT\nID: %s\nMESSAGE: %s\nAUTHOR: %s\nTIMESTAMP: %f\nDIFF:\n%s\nEND_COMMIT"
    commit.meta.id
    commit.meta.message
    commit.meta.author
    commit.meta.timestamp
    (Diff.to_string commit.diff)
