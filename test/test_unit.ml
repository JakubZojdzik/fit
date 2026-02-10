open Test_helpers

let () =
  print_endline "=== Fit unit ===";

  run_test "init" (fun () ->
      let dir = setup_test_dir () in
      let _repo = assert_ok "init" (Fit.init dir) in
      assert_true ".fit exists" (Sys.file_exists (Filename.concat dir ".fit")));

  run_test "init_twice" (fun () ->
      let dir = setup_test_dir () in
      let _repo = assert_ok "init" (Fit.init dir) in
      assert_error "init twice" (Fit.init dir));

  run_test "commit" (fun () ->
      let dir = setup_test_dir () in
      write_file "test.txt" "hello\n";
      let repo = assert_ok "init" (Fit.init dir) in
      let repo2 = assert_ok "commit" (Fit.commit repo "test" "tester") in
      let log = Fit.log repo2 in
      assert_true "log >= 2" (List.length log >= 2));

  run_test "status" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let status = Fit.status repo in
      assert_true "On branch" (String.sub status 0 9 = "On branch"));

  run_test "log_initial" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let log = Fit.log repo in
      assert_eq "1 entry" (List.length log) 1);

  run_test "diff_no_changes" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let d = assert_ok "diff" (Fit.diff repo) in
      assert_true "empty diff" (Fit.Diff.is_empty d));

  run_test "diff_with_changes" (fun () ->
      let dir = setup_test_dir () in
      write_file "a.txt" "line1\n";
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "commit" (Fit.commit repo "c1" "t") in
      write_file "a.txt" "line1\nline2\n";
      let d = assert_ok "diff" (Fit.diff repo) in
      assert_true "non-empty diff" (not (Fit.Diff.is_empty d)));

  run_test "create_branch" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "branch" (Fit.create_branch repo "feature") in
      let branches = Fit.list_branches repo in
      assert_true "feature" (List.mem "feature" branches);
      assert_true "master" (List.mem "master" branches));

  run_test "create_duplicate_branch" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "branch" (Fit.create_branch repo "feature") in
      assert_error "duplicate" (Fit.create_branch repo "feature"));

  run_test "checkout_branch" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "branch" (Fit.create_branch repo "feature") in
      let repo = assert_ok "checkout" (Fit.checkout repo "feature") in
      let status = Fit.status repo in
      assert_true "on feature" (String.sub status 0 17 = "On branch feature"));

  run_test "checkout_nonexistent" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      assert_error "nonexistent" (Fit.checkout repo "nonexistent"));

  run_test "checkout_commit_id" (fun () ->
      let dir = setup_test_dir () in
      write_file "f.txt" "v1\n";
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
      let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
      write_file "f.txt" "v2\n";
      let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
      let _repo = assert_ok "checkout" (Fit.checkout repo c1_id) in
      assert_eq "restored" (read_file "f.txt") (Some "v1\n"));

  run_test "revert" (fun () ->
      let dir = setup_test_dir () in
      write_file "f.txt" "v1\n";
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "c1" (Fit.commit repo "c1" "t") in
      let c1_id = Fit.Node.id (List.hd (Fit.log repo)) in
      write_file "f.txt" "v2\n";
      let repo = assert_ok "c2" (Fit.commit repo "c2" "t") in
      let repo = assert_ok "revert" (Fit.revert repo c1_id) in
      assert_eq "reverted" (read_file "f.txt") (Some "v1\n");
      let status = Fit.status repo in
      assert_true "still on branch" (String.sub status 0 9 = "On branch"));

  run_test "revert_nonexistent" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      assert_error "nonexistent" (Fit.revert repo "nonexistent_id"));

  run_test "open_repo" (fun () ->
      let dir = setup_test_dir () in
      let _repo = assert_ok "init" (Fit.init dir) in
      let _repo2 = assert_ok "open" (Fit.open_repo dir) in
      ());

  run_test "open_repo_no_init" (fun () ->
      let dir = setup_test_dir () in
      assert_error "no init" (Fit.open_repo dir));

  run_test "merge" (fun () ->
      let dir = setup_test_dir () in
      write_file "f1.txt" "master\n";
      let repo = assert_ok "init" (Fit.init dir) in
      let repo = assert_ok "c1" (Fit.commit repo "mc" "t") in
      let repo = assert_ok "branch" (Fit.create_branch repo "feat") in
      let repo = assert_ok "co feat" (Fit.checkout repo "feat") in
      write_file "f2.txt" "feature\n";
      let repo = assert_ok "fc" (Fit.commit repo "fc" "t") in
      let repo = assert_ok "co master" (Fit.checkout repo "master") in
      let _repo = assert_ok "merge" (Fit.merge repo "feat" "merge" "t") in
      assert_true "f1 exists" (file_exists "f1.txt");
      assert_eq "f2 content" (read_file "f2.txt") (Some "feature\n"));

  run_test "merge_nonexistent" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      assert_error "nonexistent" (Fit.merge repo "ghost" "msg" "t"));

  run_test "merge_self" (fun () ->
      let dir = setup_test_dir () in
      let repo = assert_ok "init" (Fit.init dir) in
      assert_error "self" (Fit.merge repo "master" "msg" "t"));

  print_summary ()
