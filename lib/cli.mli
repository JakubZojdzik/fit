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

(* args -> command *)
val parse : string array -> command Result_monad.t

(* -> help_str *)
val help_text : string
