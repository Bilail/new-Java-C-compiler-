open ContextAnalysis

(**
   ____________________________________________
   /                    ------------°°°°------------                      \
   |     TYPES POUR L'ANALYSE CONTEXTUELLE
   \ ___________________________________________ /
 **)
(* let printAll programme  =
   List.iter printClass programme.classes;
   print_newline ();
*)

let printEnv e =
  print_string "ENVIRONMENT {"; print_newline ();
  print_string "  Classes : "; print_int (List.length e.decl_classes); print_newline ();
  print_string "  Local variables : "; print_int (List.length e.decl_vars); print_newline ();
  print_string "  Valid ? "; print_string (match e.is_correct_env with | true -> "Yes" | false -> "No"); print_newline ();
  print_string "}"; print_newline ();