open Ast

(**
		
**)
(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     				ACTUELLEMENT TESTE
\ ___________________________________________ /
**)
(**
1. Plusieurs classes ne peuvent pas avoir le même nom

**)


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     		FONCTIONS UTILES PLUS BAS
\ ___________________________________________ /
**)

let forbidClassName classes name =
	let check = not (List.exists (fun c -> c.name_class = name) classes)
	in match check with
	| true -> true
	| false ->
		print_string "[Error] Two classes cannot have the same name : "; print_string name; print_newline ();
		false






(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     FONCTIONS D'ANALYSE CONTEXTUELLE
\ ___________________________________________ /
**)


type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; is_correct_env=true}



let analyseClass c env =
	{
		decl_classes = c::env.decl_classes;
		decl_vars = env.decl_vars;
		is_correct_env =
			env.is_correct_env &&
			forbidClassName env.decl_classes c.name_class
	}


let analyseProgram prog =
	List.fold_left (fun env c -> analyseClass c env) emptyEnv prog.classes



