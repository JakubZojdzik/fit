let test_dir = ref ""
let passed = ref 0
let failed = ref 0

let setup_test_dir () =
  let dir = Filename.temp_file "fit_test_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  test_dir := dir;
  dir

let rec remove_dir path =
  if Sys.is_directory path then begin
    Sys.readdir path
    |> Array.iter (fun entry -> remove_dir (Filename.concat path entry));
    Unix.rmdir path
  end else
    Sys.remove path

let cleanup_test_dir () =
  if !test_dir <> "" && Sys.file_exists !test_dir then
    remove_dir !test_dir

let write_file path content =
  let full_path = Filename.concat !test_dir path in
  let dir = Filename.dirname full_path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Unix.mkdir d 0o755
    end
  in
  if not (Sys.file_exists dir) then mkdir_p dir;
  let oc = open_out full_path in
  output_string oc content;
  close_out oc

let read_file path =
  let full_path = Filename.concat !test_dir path in
  if not (Sys.file_exists full_path) then None
  else begin
    let ic = open_in full_path in
    let content = In_channel.input_all ic in
    close_in ic;
    Some content
  end

let file_exists path =
  Sys.file_exists (Filename.concat !test_dir path)

let run_test name f =
  (try
    f ();
    Printf.printf "  pass %s\n" name;
    incr passed
  with e ->
    Printf.printf "  FAIL %s: %s\n" name (Printexc.to_string e);
    incr failed);
  cleanup_test_dir ()

let assert_eq msg a b =
  if a <> b then failwith msg

let assert_true msg b =
  if not b then failwith msg

let assert_ok msg (r : 'a Fit.result) = match r with
  | Fit__Result_monad.Ok x -> x
  | Fit__Result_monad.Error e -> failwith (msg ^ ": " ^ e)

let assert_error msg (r : 'a Fit.result) = match r with
  | Fit__Result_monad.Error _ -> ()
  | Fit__Result_monad.Ok _ -> failwith (msg ^ ": expected error but got Ok")

let print_summary () =
  Printf.printf "\n=== %d passed, %d failed ===\n" !passed !failed;
  if !failed > 0 then exit 1
