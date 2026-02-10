module Result = Result_monad

let ( let* ) = Result.bind

type workspace_path = string
type 'a result = 'a Result.t

let read_ignore_file workspace_path =
  let path = Filename.concat workspace_path ".fitignore" in
  if Sys.file_exists path then (
    let ic = open_in path in
    let rec read acc =
      match input_line ic with
      | line ->
          let trimmed = String.trim line in
          if trimmed = "" then read acc else read (trimmed :: acc)
      | exception End_of_file -> acc
    in
    let entries = read [] in
    close_in ic;
    entries)
  else []

let should_ignore ignore_list path =
  let basename = Filename.basename path in
  basename = ".fit" || List.mem basename ignore_list

let rec collect_files ignore_list base_path rel_path =
  let full_path = Filename.concat base_path rel_path in
  if should_ignore ignore_list (Filename.basename full_path) then []
  else if Sys.is_directory full_path then
    let entries = Sys.readdir full_path |> Array.to_list in
    List.concat_map
      (fun entry ->
        let new_rel =
          if rel_path = "." then entry else Filename.concat rel_path entry
        in
        collect_files ignore_list base_path new_rel)
      entries
  else [ rel_path ]

let list_files path =
  try
    if not (Sys.file_exists path) then Result.fail "Workspace does not exist"
    else
      let ignore_list = read_ignore_file path in
      Result.return (collect_files ignore_list path ".")
  with Sys_error msg -> Result.fail msg

let read_file workspace_path rel_path =
  try
    let full_path = Filename.concat workspace_path rel_path in
    let ic = open_in full_path in
    let rec read_lines acc =
      match input_line ic with
      | line -> read_lines (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    close_in ic;
    Result.return lines
  with Sys_error msg -> Result.fail msg

let write_file workspace_path rel_path lines =
  try
    let full_path = Filename.concat workspace_path rel_path in
    let dir = Filename.dirname full_path in
    let rec mkdir_p path =
      if not (Sys.file_exists path) then begin
        mkdir_p (Filename.dirname path);
        Unix.mkdir path 0o755
      end
    in
    if not (Sys.file_exists dir) then mkdir_p dir;
    let oc = open_out full_path in
    List.iter (fun line -> output_string oc (line ^ "\n")) lines;
    close_out oc;
    Result.return ()
  with Sys_error msg -> Result.fail msg

let delete_file workspace_path rel_path =
  try
    let full_path = Filename.concat workspace_path rel_path in
    if Sys.file_exists full_path then Sys.remove full_path;
    let rec remove_empty_parents dir =
      if
        dir <> workspace_path && Sys.file_exists dir && Sys.is_directory dir
        && Array.length (Sys.readdir dir) = 0
      then begin
        Unix.rmdir dir;
        remove_empty_parents (Filename.dirname dir)
      end
    in
    remove_empty_parents (Filename.dirname full_path);
    Result.return ()
  with Sys_error msg -> Result.fail msg

let read_state workspace_path =
  let* files = list_files workspace_path in
  let results =
    List.filter_map
      (fun rel_path ->
        match read_file workspace_path rel_path with
        | Result.Ok content -> Some (rel_path, content)
        | Result.Error _ -> None)
      files
  in
  Result.return results

let write_state workspace_path state =
  let target_paths = List.map fst state in
  let* current_files = list_files workspace_path in
  let to_delete =
    List.filter (fun f -> not (List.mem f target_paths)) current_files
  in
  let* () =
    List.fold_left
      (fun acc rel_path ->
        let* () = acc in
        delete_file workspace_path rel_path)
      (Result.return ()) to_delete
  in
  List.fold_left
    (fun acc (rel_path, content) ->
      let* () = acc in
      write_file workspace_path rel_path content)
    (Result.return ()) state

let apply_diff reverse workspace_path diff =
  List.fold_left
    (fun acc (file_diff : Diff.file_diff) ->
      let* () = acc in
      let current_content =
        match read_file workspace_path file_diff.path with
        | Result.Ok c -> c
        | Result.Error _ -> []
      in
      let new_content =
        Diff.apply_file_diff reverse file_diff current_content
      in
      if new_content = [] then delete_file workspace_path file_diff.path
      else write_file workspace_path file_diff.path new_content)
    (Result.return ()) diff
