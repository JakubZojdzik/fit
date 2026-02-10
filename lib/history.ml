module Result = Result_monad
module BranchMap = Map.Make(String)
module StringSet = Set.Make(String)

type branch_name = string

type t = {
  root : Node.t option;
  branches : Node.t BranchMap.t;
  head : Node.t option;
  current_branch : branch_name option;
}

type 'a result = 'a Result.t

let empty () = {
  root = None;
  branches = BranchMap.empty;
  head = None;
  current_branch = None;
}

let find_node_by_id history node_id =
  let rec dfs visited node =
    let nid = Node.id node in
    if StringSet.mem nid visited then None
    else if nid = node_id then Some node
    else
      let visited = StringSet.add nid visited in
      dfs_children (Node.children node) visited
  and dfs_children children visited =
    begin match children with
    | [] -> None
    | child :: rest ->
      begin match dfs visited child with
      | Some _ as found -> found
      | None -> dfs_children rest visited
      end
    end
  in
  match history.root with
  | None -> None
  | Some root -> dfs StringSet.empty root

let init message author =
  let commit = Commit.initial message author in
  let node = Node.create commit None in
  { 
    root = Some node;
    branches = BranchMap.add "master" node BranchMap.empty;
    head = Some node;
    current_branch = Some "master";
  }

let head history = history.head

let current_branch history = history.current_branch

let commit history diff message author =
  match history.head with
  | None -> Result.fail "No HEAD"
  | Some parent_node ->
    let commit = Commit.create diff message author in
    let node = Node.create commit (Some parent_node) in
    let new_branches = 
      match history.current_branch with
      | Some branch_name -> BranchMap.add branch_name node history.branches
      | None -> history.branches
    in
    Result.return { history with branches = new_branches; head = Some node }

let checkout_commit history node_id =
  match find_node_by_id history node_id with
  | None -> Result.fail (Printf.sprintf "Node %s not found" node_id)
  | Some node ->
    Result.return { history with head = Some node; current_branch = None }

let checkout_branch history branch_name =
  match BranchMap.find_opt branch_name history.branches with
  | None -> Result.fail (Printf.sprintf "Branch %s not found" branch_name)
  | Some node ->
    Result.return { history with head = Some node; current_branch = Some branch_name }

let revert_to history node_id =
  match history.current_branch with
  | None -> Result.fail "Cannot revert in detached HEAD state"
  | Some branch_name ->
    match find_node_by_id history node_id with
    | None -> Result.fail (Printf.sprintf "Node %s not found" node_id)
    | Some node ->
      let new_branches = BranchMap.add branch_name node history.branches in
      Result.return { history with head = Some node; branches = new_branches }

let create_branch history branch_name =
  if BranchMap.mem branch_name history.branches then
    Result.fail (Printf.sprintf "Branch %s already exists" branch_name)
  else
    match history.head with
    | None -> Result.fail "No HEAD"
    | Some node ->
      let new_branches = BranchMap.add branch_name node history.branches in
      Result.return { history with branches = new_branches }

let list_branches history =
  BranchMap.bindings history.branches |> List.map fst

let reconstruct_state node =
  let rec path_to_root acc current =
    match Node.primary_parent current with
    | None -> current :: acc
    | Some parent -> path_to_root (current :: acc) parent
  in
  let path = path_to_root [] node in
  List.fold_left (fun state n ->
    let commit = Node.commit n in
    Commit.apply commit state
  ) [] path

let merge history branch_name message author =
  if history.current_branch = Some branch_name then
    Result.fail "Cannot merge branch into itself"
  else
    match BranchMap.find_opt branch_name history.branches with
    | None -> Result.fail (Printf.sprintf "Branch %s not found" branch_name)
    | Some secondary_node ->
      begin match history.head with
      | None -> Result.fail "No HEAD"
      | Some primary_node ->
        let primary_state = reconstruct_state primary_node in
        let secondary_state = reconstruct_state secondary_node in
        let merged_state = 
          let primary_paths = List.map fst primary_state in
          let secondary_only = List.filter (fun (p, _) -> 
            not (List.mem p primary_paths)
          ) secondary_state in
          primary_state @ secondary_only
        in
        let merged_diff = Diff.compute_state_diff primary_state merged_state in
        let merge_commit = Commit.create merged_diff message author in
        let merge_node = Node.create_merge merge_commit primary_node secondary_node in
        Result.return {
          history with
          branches = BranchMap.remove branch_name history.branches;
          head = Some merge_node;
        }
      end

let log history =
  let rec collect acc = function
    | None -> List.rev acc
    | Some node -> collect (node :: acc) (Node.primary_parent node)
  in
  collect [] history.head

let reconstruct_head_state history =
  match history.head with
  | None -> []
  | Some node -> reconstruct_state node
