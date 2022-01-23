open Ast

(**
		
**)
(**
  ____________________________________________
/         ------------°°°°------------        \
|             ACTUELLEMENT TESTE               
\ ___________________________________________ /
**)
(**

1. Classes
    1. Plusieurs classes ne peuvent pas avoir le même nom
    2. Les paramètres de la classe sont les mêmes que les params du constructeur
    3. Le constructeur et sa classe extend-ent la même superclasse
    4. On doit extend une classe qui existe (pas forcément déclarée plus haut)
    5. On ne peut pas s'extend soi-même
    6. On ne peut pas faire d'héritage circulaire
    7. L'appel à la superclasse dans le constructeur a des arguments qui correspondent à la superclasse // A tester



A faire
    Les params du constructeur correspondent aux args de l'instanciation
    Les params des méthodes correspondent aux arguments des appels
    On ne peut pas déclarer de variables locales avec le même nom dans la même portée
    Les membres gauche et droit d'une affectation doivent avoir même type
    Une expression renvoie un type existant

A faire (expressions return)
  IntLiteral
  StringLiteral
  Container : Selec
  Container : LocalVar
  Container : This
  Container : Super
  Method 
  Binary
  Unary
  Cast
  NewClass


**)

(**
  ____________________________________________
/         ------------°°°°------------        \
|        QUELQUES FONCTIONS AUXILIAIRES        
\ ___________________________________________ /
**)


(*-----------------------------------------------------------------------------------------------
                                      Types auxiliaires
-----------------------------------------------------------------------------------------------*)

(*  Contient : classes existantes ; variables locales ; validité jusqu'à présent *)
type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
  current_class : class_def option;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; current_class=None; is_correct_env=true}



(* Est renvoyé par la vérification d'une expression *)
(* Contient type de retour de l'expression ; validité jusqu'à présent *)
type exprUpward = {
  expr_return_type : string;
  is_correct_expr : bool
}
let emptyExprUpw = {expr_return_type="Integer"; is_correct_expr=true}



type exprListUpward = {
  expr_return_types : string list;
  is_correct_exprs : bool
}



exception Error_inherit_cycle of string


(*-----------------------------------------------------------------------------------------------
                                           divers
-----------------------------------------------------------------------------------------------*)

(* Compte les occurrences d'un élément dans une liste *)
let rec countOccurrences elem lis =
    List.fold_left (fun count el -> if elem = el then count+1 else count) 0 lis



and print_bool b = match b with
| true -> print_string "YES"
| false -> print_string "NO"



(*-----------------------------------------------------------------------------------------------
                    pour les appels de méthodes/constructeurs
-----------------------------------------------------------------------------------------------*)

and getExprTypes (exprs: expression_t list) env =
  let upwards = (List.map (fun e -> expr_verif e env) exprs)
  in
  {
    expr_return_types = List.map (fun u -> u.expr_return_type) upwards;
    is_correct_exprs = List.fold_left (fun acc u -> u.is_correct_expr && acc) true upwards
  }


and getVarDeclTypes (variables: variable_def list) =
  {
    expr_return_types = List.map (fun var -> var.typ) variables;
    is_correct_exprs = true
  }



(*-----------------------------------------------------------------------------------------------
                                       pour les classes
-----------------------------------------------------------------------------------------------*)

(* Indique si un nom de classe existe dans une liste *)
and countClassnameAmong classes name =
    (countOccurrences name (List.map (fun c -> c.name_class) classes))


      
(* Retoune la classe associée au nom de classe donnée en param *)
and find_class s l_c =
  List.find_opt(fun c -> c.name_class = s) l_c


(* Vérifie qu'un type est sous type d'un autre type *)
and is_subclass m f l_cl =
  let rec issc m f l_cl =
    if m = f then
      true
    else
      let fi = find_class f l_cl
      in match fi with
      | None -> print_string f; print_string " inexistante\n"; false
      | Some fille -> ( 
        match fille.superclass with 
          | None -> print_string "Pas de superclasse pour "; print_string fille.name_class; print_newline (); false
          | Some s -> is_subclass m s l_cl
      )
  in issc m f l_cl
  




and is_subclass_cyclesafe parentName childName classes = 
    let rec findInInheritance parentName childName classes (alreadyVisited: string list) =
        if parentName = childName then
            true
        else if List.mem childName alreadyVisited then
            raise (Error_inherit_cycle childName)
        else
            let child = find_class childName classes
            in
            match child with
            | None -> false
            | Some c -> (
                match c.superclass with
                | None -> false
                | Some supername -> findInInheritance parentName supername classes (childName::alreadyVisited)
            )
    in
    findInInheritance parentName childName classes []





(*-----------------------------------------------------------------------------------------------
                                    pour les environnements
-----------------------------------------------------------------------------------------------*)

(* Verifie qu'une variable est dans l'envrionnement *)
and var_in_env env e = 
  List.exists (fun c -> c.name == e) env.decl_vars


(**
(* Ajout d'une variable à un environnement *)
let add_env_var env v = 
  let new = {
    decl_classes = env.decl_classes;
	  decl_vars = v::env.decl_vars;
    is_correct_env =
			env.is_correct_env && e.is_correct_env
  }
**)

(**
(* Ajout d'une classe à un environnement *)
  let add_env_classe env c = 
    let new = {
      decl_classes = c::env.decl_classes;
		  decl_vars = env.decl_vars;
	  	is_correct_env =
			  env.is_correct_env &&
			  forbidClassName env.decl_classes c.name_class &&
            chckParamsInClaAndConstr c
	}
**) 





(**
  ____________________________________________
/         ------------°°°°------------        \
|             ANALYSE : 1- CLASSES             
\ ___________________________________________ /
**)




(* Affiche une erreur si "name" est un nom porté par une classe dans "classes" *)
and forbidClassName (classes:class_def list) name =
	let check = ((countClassnameAmong classes name) = 1)
	in match check with
	| true -> true
	| false ->
		print_string "[Error] Two classes cannot have the same name : "; print_string name; print_newline ();
		false


(* Affiche une erreur si les paramètres d'une classe et de son constructeur sont différents *)
and chckParamsInClaAndConstr c =
    let check = c.params_class = c.constructor.param_constructor
    in match check with
    | true -> true
    | false -> 
        print_string "[Error] A class and its constructor must have the same parameters in "; print_string c.name_class;
        false
 
(* Affiche une erreur si la superclasse n'existe pas *)
and chckSuperclassExistence call c classes =
    let check = (countClassnameAmong classes call.superclass_constructor) = 1
    in match check with
    | true -> true
    | false ->
        (print_string "[Error] "; print_string c.name_class; print_string " class extends a class that doesn't exist : "; print_string call.superclass_constructor; print_newline (); (print_string "Superclasse existe OK : "; print_string call.superclass_constructor; print_newline ());
        false)


(* Affiche une erreur si une classe hérite d'elle-même *)
and forbidInheritanceCycle c classes =
    match c.superclass with
    | None -> true
    | Some supername -> (
        match (
                try not (is_subclass_cyclesafe c.name_class supername classes) with
                | Error_inherit_cycle classname -> print_string "[Error] "; print_string c.name_class; print_string " is part of an inheritance cycle involving "; print_string classname; print_newline (); false
        ) with
        | true -> true
        | false -> print_string "[Error] "; print_string c.name_class; print_string " extends a class that's part of an inheritance cycle"; print_newline (); false
    )




(* Affiche une erreur si les arguments de l'appel à la superclasse et les paramètres de ladite classe ne correspondent pas en terme de types *)
and chckSuperclassCallParams call env =
  let check = (
    let superclass = find_class call.superclass_constructor env.decl_classes
    in match superclass with
    | None ->
      print_string "[Error] Inexistent superclass "; print_string call.superclass_constructor;
      false
    | Some c -> (
        (getVarDeclTypes c.params_class).expr_return_types = (getExprTypes call.arguments env).expr_return_types
    )
  )
  in match check with
  | true -> true
  | false ->
    print_string "[Error] Call to superclass "; print_string call.superclass_constructor; print_string " is impossible due to wrong arguments"; print_newline ();
    false
 



(* Affiche une erreur s'il y a un problème dans le graphe d'héritage *)
and chckSuperclass call c env =
    chckSuperclassExistence call c env.decl_classes &&
    forbidInheritanceCycle c env.decl_classes &&
    chckSuperclassCallParams call env



(* Affiche une erreur si la classe et son constructeur n'appellent pas la même superclasse *)
and chckSuperclassInClaAndConstr c env = 
    match c.superclass with
    | Some s ->
      (match c.constructor.super_call with
      | Some call ->
        (if call.superclass_constructor = s then
          chckSuperclass call c env
        else
          (print_string "[Error] Mismatch between the superclass of "; print_string c.name_class; print_string " and the superclass called by its constructor : "; print_string s; print_string " and "; print_string call.superclass_constructor; print_newline ();
          false)
        )
      | None ->
        (print_string "[Error] The constructor of class "; print_string c.name_class; print_string " doesn't call its superclass : "; print_string s; print_newline ();
        false)
      )
    | None ->
      (match c.constructor.super_call with
      | Some call ->
        (print_string "Class "; print_string c.name_class; print_string " doesn't extend any superclass but its constructor calls superclass "; print_string call.superclass_constructor; print_newline ();
        false)
      | None ->
        true
      )


    


(*-----------------------------------------------------------------------------------------------
                                           Appel
-----------------------------------------------------------------------------------------------*)

and analyseClass env c =
	let newEnv = {
		decl_classes = c::env.decl_classes;
		decl_vars = env.decl_vars;
    current_class = Some c;
		is_correct_env =
			env.is_correct_env &&
			forbidClassName env.decl_classes c.name_class &&
      chckParamsInClaAndConstr c &&
      chckSuperclassInClaAndConstr c env
	}
    in
    (match newEnv.is_correct_env with
    | true -> print_string c.name_class; print_string " OK"; print_newline ()
    | false -> print_string c.name_class; print_string " KO"; print_newline ());
    newEnv.is_correct_env





(**
  ____________________________________________
/         ------------°°°°------------        \
|       ANALYSE : 2- Instructions et appels           
\ ___________________________________________ /
**)

(* Vérifie que les arguments d'une instantiation "new" correspondent aux paramètres de la classe correspondante *)
and chckParamsInClaAndNew cName arguments env =
  let check = (
    let newClass = find_class cName env.decl_classes
    in match newClass with
    | None ->
      print_string "[Error] Inexistent class in new : "; print_string cName;
      false
    | Some c -> (
        (getVarDeclTypes c.params_class).expr_return_types = (getExprTypes arguments env).expr_return_types
    )
  )
  in match check with
  | true -> true
  | false ->
    print_string "[Error] Instanciation of class  "; print_string cName; print_string " is impossible due to wrong arguments"; print_newline ();
    false




(**
  ____________________________________________
/         ------------°°°°------------        \
|           ANALYSE : 3- Expressions           
\ ___________________________________________ /
**)

and chckCastTarget target source classes =
  let check =
    is_subclass_cyclesafe target source classes
  in match check with
  | true -> true
  | false ->
    print_string "[Error] Cannot cast "; print_string source; print_string " into "; print_string " because the former doesn't inherit the latter";
    false


and chckExpectedType expected actualType =
  if expected = actualType then
    true
  else
    (print_string "[Error] Expected type "; print_string expected; print_string " but found type "; print_string actualType; print_newline ();
    false)




(*-----------------------------------------------------------------------------------------------
                                           Appels
-----------------------------------------------------------------------------------------------*)


and expr_verif expr env = (
  match expr with 
  | IntLiteral i -> { expr_return_type="Integer"; is_correct_expr=true }
  | StringLiteral s -> { expr_return_type="String"; is_correct_expr=true }
  | Container c  -> container_verif c env
  | Method m -> methode_verif m env 
  | Binary (op,e1,e2) -> binary_verif op e1 e2 env 
  | Unary (op,e) -> unary_verif op e env
  | Cast(target,e) -> cast_verif e target env
  | NewClass (s,e_list) -> new_verif s e_list env
)


and container_verif c env = 
  (match c with
  | Select s -> emptyExprUpw
  | LocalVar s -> emptyExprUpw
  | This -> emptyExprUpw
  | Super -> emptyExprUpw
)



(* Vérifie la validité d'un appel de méthode *)
and methode_verif m env = 
  emptyExprUpw



and unary_verif op e env =
  emptyExprUpw



and binary_verif op e1 e2 env =
  let verifiedExpr1 = expr_verif e1 env
  in
  let verifiedExpr2 = expr_verif e2 env
  in
  match op with 
  | IntBinOp int_op ->
      {
      expr_return_type = "Integer";
      is_correct_expr =
        chckExpectedType "Integer" verifiedExpr1.expr_return_type &&
        chckExpectedType "Integer" verifiedExpr2.expr_return_type &&
        verifiedExpr1.is_correct_expr &&
        verifiedExpr2.is_correct_expr
      }
  | StringConcat ->
    {
      expr_return_type = "String";
      is_correct_expr =
        chckExpectedType "String" verifiedExpr1.expr_return_type &&
        chckExpectedType "String" verifiedExpr2.expr_return_type &&
        verifiedExpr1.is_correct_expr &&
        verifiedExpr2.is_correct_expr
    }


(* Vérifie la validité d'un cast *)
and cast_verif expr target env =
  let verifiedExpr = expr_verif expr env
  in
    if chckCastTarget target verifiedExpr.expr_return_type env.decl_classes then
      {
        expr_return_type = target;
        is_correct_expr = verifiedExpr.is_correct_expr
      }
    else
      {
        expr_return_type = target;
        is_correct_expr = false
      }


(* Vérifie qu'une instanciation est valide *)
and new_verif cName arguments env =
  {
    expr_return_type = cName;
    is_correct_expr =
      chckParamsInClaAndNew cName arguments env &&
      List.fold_left (fun acc e -> (expr_verif e env).is_correct_expr && acc ) true arguments
  }


(*
let instr_verif instr env = 
  match instr with 
  | Exp(e) -> expr_verif e env
  | Block(b) -> block_verif b env
  | Ite(si, alors, sinon) ->  expr_verif si env; 
                              instr_verif alors env; 
                              instr_verif alors env
  | Return ->
  | Affectation(c,e) -> (* Vérification des types module heritage *)
    (* On doit vérifié que le type de e est un type ou sous type de c *)
*)

(**
let block_verif b env =
  **)



(*-----------------------------------------------------------------------------------------------
                              Fonction Utile pour les verifs 
-----------------------------------------------------------------------------------------------*)





(**
  ____________________________________________
/         ------------°°°°------------        \
|             ANALYSE : Programme              
\ ___________________________________________ /
**)


let analyseProgram prog =
    let env = {
        decl_classes = prog.classes;
        decl_vars = [];
        current_class = None;
        is_correct_env = true
    }
    in
	List.fold_left (fun is_correct c -> (analyseClass env c) && is_correct) true prog.classes

