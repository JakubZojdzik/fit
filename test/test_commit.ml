open Test_helpers

let () =
  print_endline "=== Commit ===";

  run_test "create" (fun () ->
      let c = Fit.Commit.create Fit.Diff.empty "msg" "author" in
      assert_eq "message" (Fit.Commit.metadata c).message "msg";
      assert_eq "author" (Fit.Commit.metadata c).author "author";
      assert_true "id non-empty" (String.length (Fit.Commit.id c) > 0));

  run_test "initial" (fun () ->
      let c = Fit.Commit.initial "init" "auth" in
      assert_true "empty diff" (Fit.Diff.is_empty (Fit.Commit.diff c)));

  run_test "apply" (fun () ->
      let state = [ ("a.txt", [ "line1" ]) ] in
      let new_state = [ ("a.txt", [ "line1"; "line2" ]) ] in
      let diff = Fit.Diff.compute_state_diff state new_state in
      let c = Fit.Commit.create diff "add line" "test" in
      let result = Fit.Commit.apply c state in
      let content = List.assoc "a.txt" result in
      assert_eq "applied" content [ "line1"; "line2" ]);

  run_test "to_string" (fun () ->
      let c = Fit.Commit.create Fit.Diff.empty "msg" "auth" in
      let s = Fit.Commit.to_string c in
      assert_true "non-empty" (String.length s > 0));

  run_test "unique_ids" (fun () ->
      let c1 = Fit.Commit.create Fit.Diff.empty "msg1" "auth" in
      let c2 = Fit.Commit.create Fit.Diff.empty "msg2" "auth" in
      assert_true "different ids" (Fit.Commit.id c1 <> Fit.Commit.id c2));

  run_test "metadata_timestamp" (fun () ->
      let before = Unix.gettimeofday () in
      let c = Fit.Commit.create Fit.Diff.empty "msg" "auth" in
      let after = Unix.gettimeofday () in
      let ts = (Fit.Commit.metadata c).timestamp in
      assert_true "timestamp in range" (ts >= before && ts <= after));

  print_summary ()
