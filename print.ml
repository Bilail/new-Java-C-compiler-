open Ast
open ContextAnalysis

(* Print de class_def *)
let printClass c =
   print_string " CLASS"; print_string c.name_class; print_string "( "; List.iter printVariable c.attributes;
   print_string " )";
   match c.superclass with 
       | None -> ""
       | Some s -> print_string "EXTENDS"; print_string s;
   print_string "IS";
   print_string " { ";
   printBloc c.methods;
   print_string " } ";

(**
  ____________________________________________
/                    ------------°°°°------------                      \
|                    TYPES POUR L'AST
\ ___________________________________________ /
**)

(*

let string_of_relop (op: Ast.opComp)  =
   match op with
     Eq -> "="
   | Neq -> "<>"
   | Lt -> "<"
   | Le -> "<="
   | Gt -> ">"
   | Ge -> ">="
 
(* Print de block_t*)
let printBloc b =       
   List.iter printVariable b.declarations;
   List.iter printInstr b.instructions;
   

(* Print de constructor_def*)
let printConstructor cons = 
   print_string "DEF";
   print_string cons.name_constructor;
   print_string " ( ";
   List.iter printVariable cons.param_constructor;
   match cons.super_call with
      | None -> " "
      | Some s -> print_string " [ "  printSuperConstructor s print_string " ] ";
   print_string "IS";
   printBloc cons.body_constructor;

(* Print de superconstructor_call *)
let printSuperConstructor cons = 
   print_string cons.superclass_constructor;
   List.iter printExpr cons.arguments;

(* Print de methode_def *)
let printMethod m =
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
   (* je sais pas comment gerer : 
   def [ static ] [ override ] nom (param, ...) : nomClasse := expression
   def [ static ] [ override ] nom (param, ...) [ : nomClasse ] is bloc

   Les deux différentes possibilité avec : NomCLASSE *)

   (* Print de variable_def *)
   let printVariable d =
      match d.is_var with 
         | None -> " "
         | Some s -> print_string "VAR"; 
      match d.is_static with 
         | None -> " "
         | Some s -> print_string "STATIC"; 
      print_string d.name; print_string " : "; print_string d.typ;
      print_newline ();

   (* Print de block_t*)
   let printBloc b = 
      List.iter printVariable b.declarations;
      List.iter printInstr b.instructions;
   
   (* Print d'instruction_t*) 
   let rec printInstr ins =
      match ins with
      | Exp e -> printExpr e
      | Block b -> printBloc b 
      | Return -> print_string ";" (* On verra jsp *)
      | Ite (si, alors, sinon) ->
      print_string " IF "; printExpr si;
      print_string " THEN "; printExpr alors;
      print_string " ELSE "; printExpr sinon;
      print_endline "]"
      | Affectation (g, d) ->
         print_string "["; print_string "["; printExpr g; print_string "]"; print_string ".";
         print_string "["; printExpr d; print_string "]"; print_string "]"; print_string ";";

   (* Print container_t*) 
(* let printContainer c = 
      match c with 
      | Select a -> printAtt
      | LocalVar n -> 
      | This -> 
      | Super -> 

*)
let rec printExpr e =
   match e with
       Id s -> print_string s
     | Cste i -> print_int i
     | Plus(g, d) ->
        print_string "["; printExpr g; print_string " + ";
        printExpr d; print_string "]"
     | Minus (g, d) ->
        print_string "["; printExpr g; print_string " - ";
        printExpr d; print_string "]"
     | Times (g, d) ->
        print_string "["; printExpr g; print_string " * ";
        printExpr d; print_string "]"
     | Div (g, d) ->
        print_string "["; printExpr g; print_string " / ";
        printExpr d; print_string "]"
     | UMinus e -> print_string "[ - ";  printExpr e; print_string "]"
     | Comp(op, g, d) ->
        print_string "["; printExpr g;
        print_string (string_of_relop op); printExpr d; print_string "]";

let string_of_relop (op: Ast.opComp)  =
   match op with
     Eq -> "="
   | Neq -> "<>"
   | Lt -> "<"
   | Le -> "<="
   | Gt -> ">"
   | Ge -> ">=";

*)

let printAll cl bl (* cons1 cons2 m d ins e *) =
   printClass cl;
   printBloc bl;
 (*  printConstructor cons;
   printSuperConstructor cons2;
   printMethod m;
   printVariable d;
   printInstr ins;
   printExpr e; *)
   print_newline ()


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     TYPES POUR L'ANALYSE CONTEXTUELLE
\ ___________________________________________ /
**)


let printEnv e =
   print_string "ENVIRONMENT {"; print_newline ();
   print_string "  Classes : "; print_int (List.length e.decl_classes); print_newline ();
   print_string "  Local variables : "; print_int (List.length e.decl_vars); print_newline ();
   print_string "  Valid ? "; print_string (match e.is_correct_env with | true -> "Yes" | false -> "No"); print_newline ();
   print_string "}"; print_newline ();
   
