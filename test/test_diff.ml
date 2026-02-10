open Test_helpers

let () =
  print_endline "=== Diff ===";

  run_test "empty" (fun () ->
    assert_true "empty is empty" (Fit.Diff.is_empty Fit.Diff.empty);
    assert_eq "empty to_string" (Fit.Diff.to_string Fit.Diff.empty) "");

  run_test "compute_identical" (fun () ->
    let s = [("a.txt", ["hello"; "world"])] in
    let diff = Fit.Diff.compute_state_diff s s in
    assert_true "identical -> empty" (Fit.Diff.is_empty diff));

  run_test "compute_new_file" (fun () ->
    let diff = Fit.Diff.compute_state_diff [] [("a.txt", ["hello"])] in
    assert_true "new file -> non-empty" (not (Fit.Diff.is_empty diff)));

  run_test "compute_removed_file" (fun () ->
    let diff = Fit.Diff.compute_state_diff [("a.txt", ["hello"])] [] in
    assert_true "removed -> non-empty" (not (Fit.Diff.is_empty diff)));

  run_test "compute_modified_file" (fun () ->
    let old_s = [("a.txt", ["hello"])] in
    let new_s = [("a.txt", ["hello"; "world"])] in
    let diff = Fit.Diff.compute_state_diff old_s new_s in
    assert_true "modified -> non-empty" (not (Fit.Diff.is_empty diff)));

  run_test "apply_roundtrip" (fun () ->
    let old_c = ["line1"; "line2"] in
    let new_c = ["line1"; "line2"; "line3"] in
    let fd = Fit.Diff.compute_file_diff "test" old_c new_c in
    let applied = Fit.Diff.apply_file_diff false fd old_c in
    assert_eq "forward" applied new_c;
    let reversed = Fit.Diff.apply_file_diff true fd new_c in
    assert_eq "reverse" reversed old_c);

  run_test "to_string_nonempty" (fun () ->
    let diff = Fit.Diff.compute_state_diff [] [("f.txt", ["x"])] in
    let s = Fit.Diff.to_string diff in
    assert_true "non-empty string" (String.length s > 0));

  run_test "multiple_files" (fun () ->
    let old_s = [("a.txt", ["a"]); ("b.txt", ["b"])] in
    let new_s = [("a.txt", ["a2"]); ("b.txt", ["b2"])] in
    let diff = Fit.Diff.compute_state_diff old_s new_s in
    assert_true "multi-file diff non-empty" (not (Fit.Diff.is_empty diff)));

  print_summary ()
