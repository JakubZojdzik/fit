let () =
  let args = Sys.argv in
  match Fit.Cli.parse args with
  | Fit.Result.Ok cmd -> exit (Fit.run_command cmd)
  | Fit.Result.Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
