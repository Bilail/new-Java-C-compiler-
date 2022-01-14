open Ast
open ContextAnalysis


(* Print prog_def *)
let printProg prog = 
   List.iter printClass prog.classes;
   printBloc prog.program;


(* Print de class_def *)
and let printClass c =
   print_string " CLASS"; print_string c.name_class; print_string "( "; List.iter printVariable c.attributes;
   print_string " )";
   match c.superclass with 
       | None -> ""
       | Some s -> print_string "EXTENDS"; print_string s;
   print_string "IS";
   print_string " { ";
   printBloc c.methods;
   print_string " } ";
   print_string " Le constructeur ";
   printConstructor c.constructor;
(**
  ____________________________________________
/                    ------------°°°°------------                      \
|                    TYPES POUR L'AST
\ ___________________________________________ /
**)

(* Print de block_t*)
and let printBloc b =       
   List.iter printVariable b.declarations;
   List.iter printInstr b.instructions;
   

(* Print de constructor_def*)
and let printConstructor cons = 
   print_string "DEF";
   print_string cons.name_constructor;
   print_string " ( ";
   List.iter printVariable cons.param_constructor;
   match cons.super_call with
      | None -> " "
      | Some s -> print_string " [ ";  printSuperConstructor s; print_string " ] ";
   print_string "IS";
   printBloc cons.body_constructor;

(* Print de superconstructor_call *)
and let printSuperConstructor cons = 
   print_string cons.superclass_constructor;
   List.iter printExpr cons.arguments;

(* Print de methode_def *)
and let printMethod m =
   print_string " DEF ";
   match m.is_static_method with 
     | false -> print_string " " 
     | true -> print_string " STATIC";
    match m.is_override with 
     | false -> print_string " " 
     | true -> print_string " OVERRIDE ";
    print_string m.name_method;
    print_string "("; List.iter printVariable m.param_method; print_string ")";
    match c.return_type with 
    | None -> " "
    | Some s -> print_string " [ "; 
                print_string " : "; 
                print_string s; 
                print_string " ] ";
    print_string " IS ";
    printBloc m.body_method;
    print_newline();

   (* Print de variable_def *)
   and let printVariable d =
      match d.is_var with 
         | None -> " "
         | Some s -> print_string "VAR"; 
      match d.is_static with 
         | None -> " "
         | Some s -> print_string "STATIC"; 
      print_string d.name; print_string " : "; print_string d.typ;
      print_newline ();

   (* Print de block_t*)
   and let printBloc b = 
      List.iter printVariable b.declarations;
      List.iter printInstr b.instructions;
      print_newline();
   
   (* Print d'instruction_t*) 
   and let rec printInstr ins =
      match ins with
      | Exp e -> printExpr e
      | Block b -> printBloc b 
      | Return -> print_string print_string "return ;" (* On verra jsp *)
      | Ite (si, alors, sinon) ->
      print_string " IF "; printExpr si;
      print_string " THEN "; printInstr alors;
      print_string " ELSE "; printInstr sinon;
      print_endline "]"
      | Affectation (g, d) ->
         print_string "["; print_string "["; printContainer g; print_string "]"; print_string ".";
         print_string "["; printExpr d; print_string "]"; print_string "]"; print_string ";";
      print_newline();

   (* Print container_t*) 
   and let printContainer c = 
      match c with 
      | Select a -> printAttributeCall a;
      | LocalVar n -> print_string n
      | This -> print_string "this"
      | Super -> print_string "super"
   print_newline();

   (* Print attribute_call*) 
   and let printAttributeCall a = 
      printCallBeginning a.beginning;
      List.iter printCallEnd a.selections_to_attrs;

   (* Print method_call*) 
   and let printMethodCall a = 
      printCallBeginning a.beginning;
      List.iter printCallEnd a.selections_to_meths;

   (* selection_beg_t *)
   and let printCallBeginning a =
      match a with 
      | ExpSelect e -> printExpr e; 
      | ClassSelect s -> print_string s;

   (* selection_end_t *)
   and let printCallEnd a =
      match a with 
      | AttrSelect s -> print_string s; 
      | MethSelect s le -> print_string s; print_string "."; List.iter printExpr le;

   (* print de expression_t*)
   and let rec printExpr e =
      match e with
      | IntLiteral i -> print_int i
      | StringLiteral s -> print_string s 
      | Container cont -> printContainer cont
      | Method meth -> printMethod meth
      | Binary op g d -> printBinary op printExpr g printExpr d
      | Unary op e -> printUnary op printExpr e
      | Cast s e -> print_string s printExpr e
      | NewClass s le -> print_string s List.iter printExpr le

    (* print de unary_operator_t*)
    and let printUnary u =
      match u with 
      | UMINUS -> " - " 
    (* binary_operator_t *)
    and  let printBinary b = 
      match b with 
      | IntBinOp i -> printIntBinary i
      | StringConcat -> " & "

   (* and int_binary_operator_t *)
   and let printIntBinary op  =
      match op with
        EQ -> "="
      | NEG -> "<>"
      | LT -> "<"
      | LE -> "<="
      | GT -> ">"
      | GE -> ">="
      | PLUS -> " + "
      | MINUS -> " - "
      | TIMES -> " * "
      | DIV -> " / "


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