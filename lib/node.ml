type node_id = Commit.commit_id

type t = {
  commit : Commit.t;
  primary_parent : t option;
  secondary_parent : t option;
  mutable children : t list;
}

let add_child parent child =
  match parent with Some p -> p.children <- child :: p.children | None -> ()

let create commit parent =
  let node =
    { commit; primary_parent = parent; secondary_parent = None; children = [] }
  in
  add_child parent node;
  node

let create_merge commit primary_parent secondary_parent =
  let node =
    {
      commit;
      primary_parent = Some primary_parent;
      secondary_parent = Some secondary_parent;
      children = [];
    }
  in
  add_child (Some primary_parent) node;
  add_child (Some secondary_parent) node;
  node

let id node = Commit.id node.commit
let commit node = node.commit
let primary_parent node = node.primary_parent
let children node = node.children

let to_string node =
  let primary_id =
    match node.primary_parent with
    | Some p -> Commit.id p.commit
    | None -> "null"
  in
  let secondary_id =
    match node.secondary_parent with
    | Some p -> Commit.id p.commit
    | None -> "null"
  in
  Printf.sprintf "NODE\nPRIMARY_PARENT: %s\nSECONDARY_PARENT: %s\n%s\nEND_NODE"
    primary_id secondary_id
    (Commit.to_string node.commit)
