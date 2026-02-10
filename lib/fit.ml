module Result = Result_monad
module Diff = Diff
module Commit = Commit
module Node = Node
module Cli = Cli

let (let*) = Result.bind

type repository = {
  path : string;
  history : History.t;
}

type 'a result = 'a Result.t

let default_author () =
  try Sys.getenv "USER" with Not_found -> "unknown"

let init path =
  let* () = Storage.init path in
  let history = History.init "Welcome to fit!" (default_author ()) in
  let repo = { path; history } in
  let* () = Storage.save path history in
  Result.return repo

let open_repo path =
  let* history = Storage.load path in
  Result.return { path; history }

let save repo =
  Storage.save repo.path repo.history

let commit repo message author =
  let* current_state = Workspace.read_state repo.path in
  let old_state = History.reconstruct_head_state repo.history in
  let diff = Diff.compute_state_diff old_state current_state in
  let* new_history = History.commit repo.history diff message author in
  let new_repo = { repo with history = new_history } in
  let* () = save new_repo in
  Result.return new_repo

let checkout repo target =
  let* new_history = 
    match History.checkout_branch repo.history target with
    | Ok h -> Result.return h
    | Error _ -> History.checkout_commit repo.history target
  in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  Result.return new_repo

let revert repo target =
  let* new_history = History.revert_to repo.history target in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  Result.return new_repo

let create_branch repo name =
  let* new_history = History.create_branch repo.history name in
  let new_repo = { repo with history = new_history } in
  let* () = save new_repo in
  Result.return new_repo

let list_branches repo =
  History.list_branches repo.history

let merge repo branch message author =
  let* new_history = History.merge repo.history branch message author in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  Result.return new_repo

let log repo =
  History.log repo.history

let status repo =
  let branch_str = 
    match History.current_branch repo.history with
    | Some name -> Printf.sprintf "On branch %s" name
    | None -> "HEAD detached"
  in
  let head_str =
    match History.head repo.history with
    | Some node -> Printf.sprintf "HEAD: %s" (Node.id node)
    | None -> "No commits yet"
  in
  Printf.sprintf "%s\n%s" branch_str head_str

let diff repo =
  let* current_state = Workspace.read_state repo.path in
  let old_state = History.reconstruct_head_state repo.history in
  Result.return (Diff.compute_state_diff old_state current_state)

let run_command cmd =
  let cwd = Sys.getcwd () in
  let with_repo f =
    let* repo = open_repo cwd in
    f repo
  in
  let res = match cmd with
  | Cli.Help -> 
    print_string Cli.help_text;
    Result.return ()

  | Cli.Init -> 
    let* _ = init cwd in
    print_endline "Initialized fit repository";
    Result.return ()

  | Cli.Commit { message } -> with_repo (fun repo ->
    let* _ = commit repo message (default_author ()) in
    Printf.printf "Created commit: %s\n" message;
    Result.return ())

  | Cli.Log -> with_repo (fun repo ->
    List.iter (fun node ->
      let meta = Commit.metadata (Node.commit node) in
      Printf.printf "%s - %s (%s)\n"
        meta.Commit.id
        meta.Commit.message
        meta.Commit.author
    ) (log repo);
    Result.return ())

  | Cli.Status -> with_repo (fun repo ->
    print_endline (status repo);
    Result.return ())

  | Cli.Checkout { target } -> with_repo (fun repo ->
    let* _ = checkout repo target in
    Printf.printf "Switched to %s\n" target;
    Result.return ())

  | Cli.Revert { target } -> with_repo (fun repo ->
    let* _ = revert repo target in
    Printf.printf "Reverted branch to %s\n" target;
    Result.return ())

  | Cli.Branch { name } -> with_repo (fun repo ->
    begin match name with
    | None ->
      let branches = list_branches repo in
      let current = History.current_branch repo.history in
      List.iter (fun b ->
        let marker = if Some b = current then "* " else "  " in
        Printf.printf "%s%s\n" marker b
      ) branches;
      Result.return ()
    | Some branch_name ->
      let* _ = create_branch repo branch_name in
      Printf.printf "Created branch %s\n" branch_name;
      Result.return ()
    end)

  | Cli.Merge { branch; message } -> with_repo (fun repo ->
    let* _ = merge repo branch message (default_author ()) in
    Printf.printf "Merged branch %s\n" branch;
    Result.return ())

  | Cli.Diff -> with_repo (fun repo ->
    let* d = diff repo in
    print_endline (Diff.to_string d);
    Result.return ())
  in
  match res with
  | Result.Ok () -> 0
  | Result.Error e -> Printf.eprintf "Error: %s\n" e; 1

