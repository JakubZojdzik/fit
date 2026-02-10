module Result = Result_monad

type storage_path = string
type 'a result = 'a Result_monad.t

let fit_dir path = Filename.concat path ".fit"
let history_file path = Filename.concat (fit_dir path) "history"

let exists path =
  Sys.file_exists (fit_dir path) && Sys.is_directory (fit_dir path)

let init path =
  let dir = fit_dir path in
  if Sys.file_exists dir then Result.fail "Repository already exists"
  else begin
    Unix.mkdir dir 0o755;
    Result.return ()
  end

let save path history =
  try
    let file = history_file path in
    let oc = open_out_bin file in
    Marshal.to_channel oc history [];
    close_out oc;
    Result.return ()
  with Sys_error msg -> Result.fail (Printf.sprintf "Failed to save: %s" msg)

let load path =
  if not (exists path) then Result.fail "Not a fit repository"
  else
    try
      let file = history_file path in
      if not (Sys.file_exists file) then Result.return (History.empty ())
      else begin
        let ic = open_in_bin file in
        let history : History.t = Marshal.from_channel ic in
        close_in ic;
        Result.return history
      end
    with
    | Sys_error msg -> Result.fail (Printf.sprintf "Failed to load: %s" msg)
    | Failure msg ->
        Result.fail (Printf.sprintf "Failed to parse history: %s" msg)
