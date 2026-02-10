module StringMap = Map.Make (String)

type file_diff = { path : string; patch : string }
type t = file_diff list

let empty = []
let is_empty diff = diff = []

let write_temp prefix (content : File.text_content) =
  let filename = Filename.temp_file prefix ".tmp" in
  let oc = open_out filename in
  List.iter (fun line -> output_string oc (line ^ "\n")) content;
  close_out oc;
  filename

let read_file_lines filename : File.text_content =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let rec read acc =
      match input_line ic with
      | line -> read (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    let lines = read [] in
    close_in ic;
    lines

let compute_file_diff path (old_content : File.text_content)
    (new_content : File.text_content) =
  let old_file = write_temp "fit_old_" old_content in
  let new_file = write_temp "fit_new_" new_content in
  let cmd =
    Printf.sprintf "diff -u %s %s 2>/dev/null || true" (Filename.quote old_file)
      (Filename.quote new_file)
  in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  Sys.remove old_file;
  Sys.remove new_file;
  { path; patch = output }

let compute_state_diff (old_state : File.state) (new_state : File.state) =
  let old_map =
    List.fold_left
      (fun acc (path, content) -> StringMap.add path content acc)
      StringMap.empty old_state
  in
  let new_map =
    List.fold_left
      (fun acc (path, content) -> StringMap.add path content acc)
      StringMap.empty new_state
  in

  (* modified or removed files *)
  let from_old =
    StringMap.fold
      (fun path old_content acc ->
        let new_content =
          match StringMap.find_opt path new_map with Some c -> c | None -> []
        in
        let diff = compute_file_diff path old_content new_content in
        if diff.patch = "" then acc else diff :: acc)
      old_map []
  in

  (* new files *)
  let from_new =
    StringMap.fold
      (fun path new_content acc ->
        if StringMap.mem path old_map then acc
        else
          let diff = compute_file_diff path [] new_content in
          if diff.patch = "" then acc else diff :: acc)
      new_map []
  in

  from_old @ from_new

let apply_file_diff (reverse : bool) (file_diff : file_diff)
    (content : File.text_content) : File.text_content =
  if file_diff.patch = "" then content
  else
    let input_file = write_temp "fit_input_" content in
    let patch_file = write_temp "fit_patch_" [ file_diff.patch ] in
    let reverse_flag = if reverse then "-R " else "" in
    let cmd =
      Printf.sprintf "patch %s-u -s %s %s 2>/dev/null" reverse_flag
        (Filename.quote input_file)
        (Filename.quote patch_file)
    in
    let ret = Sys.command cmd in
    let result = if ret = 0 then read_file_lines input_file else content in
    Sys.remove input_file;
    Sys.remove patch_file;
    result

let file_diff_to_string fd =
  Printf.sprintf "FILE %s\nPATCH_START\n%sPATCH_END" fd.path fd.patch

let to_string diff =
  if diff = [] then ""
  else String.concat "\n---\n" (List.map file_diff_to_string diff)
