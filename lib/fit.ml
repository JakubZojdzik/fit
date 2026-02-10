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

type 'a result = 'a Result_monad.t

let default_author () =
  try Sys.getenv "USER" with Not_found -> "unknown"

let init path =
  let open Result in
  let* () = Storage.init path in
  let history = History.init "Initial commit" (default_author ()) in
  let repo = { path; history } in
  let* () = Storage.save path history in
  return repo

let open_repo path =
  let open Result in
  let* history = Storage.load path in
  return { path; history }

let save repo =
  Storage.save repo.path repo.history

let commit repo message author =
  let open Result in
  let* current_state = Workspace.read_state repo.path in
  let old_state = History.reconstruct_head_state repo.history in
  let diff = Diff.compute_state_diff old_state current_state in
  let* new_history = History.commit repo.history diff message author in
  let new_repo = { repo with history = new_history } in
  let* () = save new_repo in
  return new_repo

let checkout repo target =
  let open Result in
  let* new_history = 
    match History.checkout_branch repo.history target with
    | Ok h -> return h
    | Error _ -> History.checkout_commit repo.history target
  in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  return new_repo

let revert repo target =
  let open Result in
  let* new_history = History.revert_to repo.history target in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  return new_repo

let create_branch repo name =
  let open Result in
  let* new_history = History.create_branch repo.history name in
  let new_repo = { repo with history = new_history } in
  let* () = save new_repo in
  return new_repo

let list_branches repo =
  History.list_branches repo.history

let merge repo branch message author =
  let open Result in
  let* new_history = History.merge repo.history branch message author in
  let new_repo = { repo with history = new_history } in
  let new_state = History.reconstruct_head_state new_history in
  let* () = Workspace.write_state new_repo.path new_state in
  let* () = save new_repo in
  return new_repo

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
  let open Result in
  let* current_state = Workspace.read_state repo.path in
  let old_state = History.reconstruct_head_state repo.history in
  return (Diff.compute_state_diff old_state current_state)

(* Execute a CLI command *)
let run_command cmd =
  let cwd = Sys.getcwd () in
  let print_error msg = Printf.eprintf "Error: %s\n" msg in
  
  match cmd with
  | Cli.Help ->
    print_string Cli.help_text;
    0
    
  | Cli.Init ->
    (match init cwd with
     | Result.Ok _ -> 
       print_endline "Initialized fit repository";
       0
     | Result.Error e -> 
       print_error e;
       1)
       
  | Cli.Commit { message } ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match commit repo message (default_author ()) with
       | Result.Ok _ -> 
         Printf.printf "Created commit: %s\n" message;
         0
       | Result.Error e -> 
         print_error e;
         1)
         
  | Cli.Log ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       let nodes = log repo in
       List.iter (fun node ->
         let meta = Commit.metadata (Node.commit node) in
         Printf.printf "%s - %s (%s)\n" 
           meta.Commit.id 
           meta.Commit.message 
           meta.Commit.author
       ) nodes;
       0)
       
  | Cli.Status ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       print_endline (status repo);
       0)
       
  | Cli.Checkout { target } ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match checkout repo target with
       | Result.Ok _ -> Printf.printf "Switched to %s\n" target; 0
       | Result.Error e -> print_error e; 1)
       
  | Cli.Revert { target } ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match revert repo target with
       | Result.Ok _ -> Printf.printf "Reverted branch to %s\n" target; 0
       | Result.Error e -> print_error e; 1)
       
  | Cli.Branch { name } ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match name with
       | None ->
         let branches = list_branches repo in
         let current = History.current_branch repo.history in
         List.iter (fun b ->
           let marker = if Some b = current then "* " else "  " in
           Printf.printf "%s%s\n" marker b
         ) branches;
         0
       | Some branch_name ->
         match create_branch repo branch_name with
         | Result.Ok _ -> Printf.printf "Created branch %s\n" branch_name; 0
         | Result.Error e -> print_error e; 1)
         
  | Cli.Merge { branch; message } ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match merge repo branch message (default_author ()) with
       | Result.Ok _ -> Printf.printf "Merged branch %s\n" branch; 0
       | Result.Error e -> print_error e; 1)
       
  | Cli.Diff ->
    (match open_repo cwd with
     | Result.Error e -> print_error e; 1
     | Result.Ok repo ->
       match diff repo with
       | Result.Ok d -> print_endline (Diff.to_string d); 0
       | Result.Error e -> print_error e; 1)
