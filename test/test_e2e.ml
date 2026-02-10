open Test_helpers

let () =
  print_endline "=== E2E ===";

  run_test "commit_and_checkout" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
    write_file "f.txt" "v3\n";
    let repo = assert_ok "c3" (Fit.commit repo "c3" "t") in
    let repo = assert_ok "checkout c1" (Fit.checkout repo c1_id) in
    assert_eq "at c1" (read_file "f.txt") (Some "v1\n");
    let _repo = assert_ok "checkout master" (Fit.checkout repo "master") in
    assert_eq "at master" (read_file "f.txt") (Some "v3\n"));

  run_test "branch_diverge" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "base\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "base" "t") in
    let repo = assert_ok "branch" (Fit.create_branch repo "feat") in
    let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    write_file "f.txt" "feature\n";
    let repo = assert_ok "fc" (Fit.commit repo "fc" "t") in
    let repo = assert_ok "co master" (Fit.checkout repo "master") in
    assert_eq "master" (read_file "f.txt") (Some "base\n");
    let _repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    assert_eq "feature" (read_file "f.txt") (Some "feature\n"));

  run_test "merge_then_commit" (fun () ->
    let dir = setup_test_dir () in
    write_file "f1.txt" "master\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "m1" "t") in
    let repo = assert_ok "branch" (Fit.create_branch repo "feat") in
    let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    write_file "f2.txt" "feat\n";
    let repo = assert_ok "fc" (Fit.commit repo "f1" "t") in
    let repo = assert_ok "co master" (Fit.checkout repo "master") in
    let repo = assert_ok "merge" (Fit.merge repo "feat" "merge" "t") in
    write_file "f3.txt" "post-merge\n";
    let repo = assert_ok "c2" (Fit.commit repo "post" "t") in
    let log = Fit.log repo in
    assert_true "log >= 3" (List.length log >= 3);
    assert_true "f1" (file_exists "f1.txt");
    assert_true "f2" (file_exists "f2.txt");
    assert_true "f3" (file_exists "f3.txt"));

  run_test "multiple_files" (fun () ->
    let dir = setup_test_dir () in
    write_file "a.txt" "file a\n";
    write_file "b.txt" "file b\n";
    write_file "sub/c.txt" "file c\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "initial" "t") in
    write_file "a.txt" "file a modified\n";
    write_file "b.txt" "file b modified\n";
    let repo = assert_ok "c2" (Fit.commit repo "modify" "t") in
    let c1_id = Fit.Node.id (List.nth (Fit.log repo) 1) in
    let _repo = assert_ok "checkout" (Fit.checkout repo c1_id) in
    assert_eq "a" (read_file "a.txt") (Some "file a\n");
    assert_eq "b" (read_file "b.txt") (Some "file b\n");
    assert_eq "c" (read_file "sub/c.txt") (Some "file c\n"));

  run_test "revert_stays_on_branch" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
    write_file "f.txt" "v3\n";
    let repo = assert_ok "c3" (Fit.commit repo "c3" "t") in
    let repo = assert_ok "revert" (Fit.revert repo c1_id) in
    assert_eq "content" (read_file "f.txt") (Some "v1\n");
    let status = Fit.status repo in
    assert_true "on master" (String.sub status 0 16 = "On branch master"));

  run_test "log_order" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "A" "t") in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "B" "t") in
    write_file "f.txt" "v3\n";
    let repo = assert_ok "c3" (Fit.commit repo "C" "t") in
    let log = Fit.log repo in
    let msgs = List.map (fun n ->
      (Fit.Commit.metadata (Fit.Node.commit n)).message) log in
    assert_eq "count" (List.length msgs) 4;
    assert_eq "first" (List.hd msgs) "C");

  run_test "persistence" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "data\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let _repo = assert_ok "commit" (Fit.commit repo "c1" "t") in
    let repo2 = assert_ok "reopen" (Fit.open_repo dir) in
    let log = Fit.log repo2 in
    assert_eq "persisted" (List.length log) 2);

  run_test "diff_empty_after_commit" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "data\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "commit" (Fit.commit repo "c1" "t") in
    let d = assert_ok "diff" (Fit.diff repo) in
    assert_true "empty" (Fit.Diff.is_empty d));

  run_test "branch_after_commit" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let repo = assert_ok "branch" (Fit.create_branch repo "dev") in
    let repo = assert_ok "co dev" (Fit.checkout repo "dev") in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
    let branches = Fit.list_branches repo in
    assert_true "dev" (List.mem "dev" branches);
    assert_true "master" (List.mem "master" branches);
    assert_eq "count" (List.length branches) 2);

  run_test "detach_reattach" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let init_id = Fit.Node.id (List.nth (Fit.log repo) 1) in
    let repo = assert_ok "detach" (Fit.checkout repo init_id) in
    let s = Fit.status repo in
    assert_true "detached" (String.sub s 0 13 = "HEAD detached");
    let repo = assert_ok "reattach" (Fit.checkout repo "master") in
    let s2 = Fit.status repo in
    assert_true "on master" (String.sub s2 0 16 = "On branch master");
    assert_eq "content" (read_file "f.txt") (Some "v1\n"));

  run_test "file_deletion" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "data\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    Sys.remove (Filename.concat dir "f.txt");
    let d = assert_ok "diff" (Fit.diff repo) in
    assert_true "deletion" (not (Fit.Diff.is_empty d)));

  run_test "merge_deletes_branch" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "base\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let repo = assert_ok "branch" (Fit.create_branch repo "feat") in
    let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    write_file "g.txt" "feat\n";
    let repo = assert_ok "fc" (Fit.commit repo "fc" "t") in
    let repo = assert_ok "co master" (Fit.checkout repo "master") in
    let repo = assert_ok "merge" (Fit.merge repo "feat" "merge" "t") in
    let branches = Fit.list_branches repo in
    assert_true "feat gone" (not (List.mem "feat" branches));
    assert_true "master" (List.mem "master" branches));

  run_test "revert_then_commit" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
    let repo = assert_ok "revert" (Fit.revert repo c1_id) in
    write_file "f.txt" "v3\n";
    let repo = assert_ok "c3" (Fit.commit repo "c3" "t") in
    assert_eq "content" (read_file "f.txt") (Some "v3\n");
    let top_msg =
      (Fit.Commit.metadata (Fit.Node.commit (List.hd (Fit.log repo)))).message
    in
    assert_eq "top msg" top_msg "c3");

  run_test "checkout_root" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "data\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let init_id = Fit.Node.id (List.hd (Fit.log repo)) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let repo = assert_ok "checkout root" (Fit.checkout repo init_id) in
    assert_true "file deleted after checkout to root" (not (file_exists "f.txt"));
    let d = assert_ok "diff" (Fit.diff repo) in
    assert_true "diff empty" (Fit.Diff.is_empty d));

  run_test "multi_branch_same_commit" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "base\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let repo = assert_ok "branch a" (Fit.create_branch repo "a") in
    let repo = assert_ok "branch b" (Fit.create_branch repo "b") in
    assert_eq "3 branches" (List.length (Fit.list_branches repo)) 3;
    let repo = assert_ok "co a" (Fit.checkout repo "a") in
    assert_eq "a" (read_file "f.txt") (Some "base\n");
    let _repo = assert_ok "co b" (Fit.checkout repo "b") in
    assert_eq "b" (read_file "f.txt") (Some "base\n"));

  run_test "feature_isolation" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "base\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "base" "t") in
    let repo = assert_ok "branch" (Fit.create_branch repo "feat") in
    let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    write_file "new.txt" "feature only\n";
    let repo = assert_ok "fc" (Fit.commit repo "fc" "t") in
    let repo = assert_ok "co master" (Fit.checkout repo "master") in
    assert_true "feature file deleted on master" (not (file_exists "new.txt"));
    let d = assert_ok "diff" (Fit.diff repo) in
    assert_true "diff empty on master" (Fit.Diff.is_empty d);
    let master_log = Fit.log repo in
    let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
    assert_true "feature file restored" (file_exists "new.txt");
    let feat_log = Fit.log repo in
    assert_true "feat more commits" (List.length feat_log > List.length master_log));

  run_test "add_remove_file" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "data\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "add" "t") in
    Sys.remove (Filename.concat dir "f.txt");
    let repo = assert_ok "c2" (Fit.commit repo "remove" "t") in
    let c1_id = Fit.Node.id (List.nth (Fit.log repo) 1) in
    let _repo = assert_ok "checkout" (Fit.checkout repo c1_id) in
    assert_true "file back" (file_exists "f.txt");
    assert_eq "content" (read_file "f.txt") (Some "data\n"));

  run_test "revert_detached_fails" (fun () ->
    let dir = setup_test_dir () in
    write_file "f.txt" "v1\n";
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
    let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
    write_file "f.txt" "v2\n";
    let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
    let repo = assert_ok "detach" (Fit.checkout repo c1_id) in
    assert_error "revert fails" (Fit.revert repo c1_id));

  run_test "commit_no_changes" (fun () ->
    let dir = setup_test_dir () in
    let repo = assert_ok "init" (Fit.init dir) in
    let repo = assert_ok "commit" (Fit.commit repo "empty" "t") in
    assert_eq "log 2" (List.length (Fit.log repo)) 2);

  print_summary ()
