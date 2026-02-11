module Result = Result_monad

type command =
  | Init
  | Commit of { message : string }
  | Log
  | Status
  | Checkout of { target : string }
  | Revert of { target : string }
  | Restore of { path : string }
  | Branch of { name : string option }
  | Merge of { branch : string; message : string }
  | Diff
  | Help

let help_text =
  {|fit - functional version control system

Usage: fit <command> [options]

Commands:
  init              Initialize a new repository
  commit -m <msg>   Create a new commit with a message
  log               Show commit history
  status            Show file status
  checkout <target> Switch to a commit or branch
  revert <commit>   Revert current branch to a commit
  restore <path>    Restore last commited version of a file
  branch [name]     Create a branch or list branches
  merge <branch>    Merge a branch into the current one
  diff              Show changes since last commit
  help              Show this help
|}

let parse args =
  let args_list = Array.to_list args |> List.tl in
  (* Skip program name *)
  match args_list with
  | [] | [ "help" ] | [ "-h" ] | [ "--help" ] -> Result.return Help
  | [ "init" ] -> Result.return Init
  | [ "commit"; "-m"; message ] -> Result.return (Commit { message })
  | [ "commit" ] ->
      Result.fail "Commit requires a message: fit commit -m <message>"
  | [ "log" ] -> Result.return Log
  | [ "status" ] -> Result.return Status
  | [ "checkout"; target ] -> Result.return (Checkout { target })
  | [ "checkout" ] ->
      Result.fail "Checkout requires a target: fit checkout <commit|branch>"
  | [ "revert"; target ] -> Result.return (Revert { target })
  | [ "revert" ] -> Result.fail "Revert requires a commit: fit revert <commit>"
  | [ "restore"; path ] -> Result.return (Restore { path })
  | [ "restore" ] ->
      Result.fail "Restore requires a path to file: fit restore <path>"
  | [ "branch" ] -> Result.return (Branch { name = None })
  | [ "branch"; name ] -> Result.return (Branch { name = Some name })
  | [ "merge"; branch; "-m"; message ] ->
      Result.return (Merge { branch; message })
  | [ "merge"; branch ] ->
      Result.return
        (Merge { branch; message = Printf.sprintf "Merge branch '%s'" branch })
  | [ "merge" ] ->
      Result.fail "Merge requires a branch name: fit merge <branch>"
  | [ "diff" ] -> Result.return Diff
  | cmd :: _ ->
      Result.fail (Printf.sprintf "Unknown command: %s. Use 'fit help'" cmd)
