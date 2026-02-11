open Test_helpers

let ok x = Fit.Result.Ok x

let () =
  print_endline "=== Cli.parse ===";

  run_test "init" (fun () ->
      assert_eq "init" (Fit.Cli.parse [| "fit"; "init" |]) (ok Fit.Cli.Init));

  run_test "commit" (fun () ->
      assert_eq "commit"
        (Fit.Cli.parse [| "fit"; "commit"; "-m"; "hello" |])
        (ok (Fit.Cli.Commit { message = "hello" })));

  run_test "commit_no_msg" (fun () ->
      assert_error "commit" (Fit.Cli.parse [| "fit"; "commit" |]));

  run_test "log" (fun () ->
      assert_eq "log" (Fit.Cli.parse [| "fit"; "log" |]) (ok Fit.Cli.Log));

  run_test "status" (fun () ->
      assert_eq "status"
        (Fit.Cli.parse [| "fit"; "status" |])
        (ok Fit.Cli.Status));

  run_test "checkout" (fun () ->
      assert_eq "checkout"
        (Fit.Cli.parse [| "fit"; "checkout"; "main" |])
        (ok (Fit.Cli.Checkout { target = "main" })));

  run_test "checkout_no_target" (fun () ->
      assert_error "checkout" (Fit.Cli.parse [| "fit"; "checkout" |]));

  run_test "revert" (fun () ->
      assert_eq "revert"
        (Fit.Cli.parse [| "fit"; "revert"; "abc123" |])
        (ok (Fit.Cli.Revert { target = "abc123" })));

  run_test "revert_no_target" (fun () ->
      assert_error "revert" (Fit.Cli.parse [| "fit"; "revert" |]));

  run_test "restore" (fun () ->
      assert_eq "restore"
        (Fit.Cli.parse [| "fit"; "restore"; "test.txt" |])
        (ok (Fit.Cli.Restore { path = "test.txt" })));

  run_test "restore_no_path" (fun () ->
      assert_error "restore" (Fit.Cli.parse [| "fit"; "restore" |]));

  run_test "branch_list" (fun () ->
      assert_eq "branch list"
        (Fit.Cli.parse [| "fit"; "branch" |])
        (ok (Fit.Cli.Branch { name = None })));

  run_test "branch_create" (fun () ->
      assert_eq "branch create"
        (Fit.Cli.parse [| "fit"; "branch"; "feature" |])
        (ok (Fit.Cli.Branch { name = Some "feature" })));

  run_test "merge_with_msg" (fun () ->
      assert_eq "merge -m"
        (Fit.Cli.parse [| "fit"; "merge"; "feat"; "-m"; "merge it" |])
        (ok (Fit.Cli.Merge { branch = "feat"; message = "merge it" })));

  run_test "merge_auto_msg" (fun () ->
      assert_eq "merge auto"
        (Fit.Cli.parse [| "fit"; "merge"; "feat" |])
        (ok
           (Fit.Cli.Merge { branch = "feat"; message = "Merge branch 'feat'" })));

  run_test "merge_no_branch" (fun () ->
      assert_error "merge" (Fit.Cli.parse [| "fit"; "merge" |]));

  run_test "diff" (fun () ->
      assert_eq "diff" (Fit.Cli.parse [| "fit"; "diff" |]) (ok Fit.Cli.Diff));

  run_test "help_variants" (fun () ->
      assert_eq "empty" (Fit.Cli.parse [| "fit" |]) (ok Fit.Cli.Help);
      assert_eq "help" (Fit.Cli.parse [| "fit"; "help" |]) (ok Fit.Cli.Help);
      assert_eq "-h" (Fit.Cli.parse [| "fit"; "-h" |]) (ok Fit.Cli.Help);
      assert_eq "--help" (Fit.Cli.parse [| "fit"; "--help" |]) (ok Fit.Cli.Help));

  run_test "unknown_command" (fun () ->
      assert_error "unknown" (Fit.Cli.parse [| "fit"; "foobar" |]));

  print_summary ()
