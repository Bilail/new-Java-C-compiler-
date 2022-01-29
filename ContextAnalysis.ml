open Ast
open ContextAnalysisTools

(**
		
**)
(**
  ____________________________________________
/         ------------°°°°------------        \
|             ACTUELLEMENT TESTE               
\ ___________________________________________ /
**)
(**

0. Rendu obligatoire par la grammaire
  1. Chaque classe possède un constructeur
  2. Une affectation ne peut se faire qu'avec un contenant et une expression évaluant vers une valeur


00. Effectif de par la façon de construire l'arbre
  1. Les paramètres d'un constructeur de classe sont aussi des attributs et leur valeur est assignée implicitement



1. Classes
    1. Plusieurs classes ne peuvent pas avoir le même nom
    2. Les paramètres de la classe sont les mêmes que les params du constructeur
    3. Le constructeur et sa classe extend-ent la même superclasse
    4. On doit extend une classe qui existe (pas forcément déclarée plus haut)
    5. On ne peut pas s'extend soi-même
    6. On ne peut pas faire d'héritage circulaire
    7. L'appel à la superclasse dans le constructeur a des arguments qui correspondent à la superclasse


2. Instructions et appels
    1. Les params des méthodes correspondent aux arguments des appels modulo héritage
    2. Les params du constructeur correspondent aux args de l'instanciation
    3. On ne peut pas instancier une classe inexistante
    4. Un attribut appelé existe dans l'arbre d'héritage
    5. Une clause IF ne peut contenir qu'une expression Integer
    6. Une méthode appelée existe dans l'arbre d'héritage
    7. Les variables locales peuvent être écrasées à l'intérieur d'une portée plus spécifique


3. Containers
  1. Le container d'une affectation est d'un type égal ou parent de celui de l'expression
  2. Un appel d'attribut en sélections en chaîne doit renvoyer une valeur à chaque sélection, y compris la dernière
  3. Un appel de méthode en sélections en chaîne doit renvoyer une valeur à chaque sélection, sauf à la dernière
  4. This et super non-appellable en dehors d'une classe
  5. 
  7. Une variable locale doit être déclarée pour être utilisée
  8. 
  9. 


4. Expressions
  1. On ne peut cast que vers une classe parente
  2. Les opérateurs + - * / UMINUS et les opérateurs de comparaison ne fonctionnent qu'avec des expressions renvoyant des Integer ou dérivés
  3. L'opérateur de concaténation & n'accepte que des expressions String ou dérivés




A faire
    On ne peut pas déclarer de variables locales avec le même nom dans la même portée
    This seul est interdit en membre gauche d'une assignation
    This non-appelable dans une méthode statique
    Super appelable dans les méthodes statiques et non-statiques 
    
    On ne peut override que une méthode existante dans la chaîne d'héritage
    On ne peut pas override une méthode inexistante dans la chaîne d'héritage
    Une valeur doit avoir été initialisée pour être appelée OU on suppose que toutes les valeurs sont initialisées par défaut



**)



let rec getExprTypes (exprs: expression_t list) env =
  let upwards = (List.map (fun e -> expr_verif e env) exprs)
  in
  {
    expr_return_types = List.map (fun u -> u.expr_return_type) upwards;
    is_correct_exprs = List.fold_left (fun acc u -> u.is_correct_expr && acc) true upwards
  }


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


(* Affiche une erreur si la classe n'existe pas, la classe sinon *)
and chckClassExistence name classes = (
  let check = List.find_opt (fun c -> c.name_class = name) classes
  in match check with
  | Some c -> Some c
  | None -> (
    print_string "[Error] Class "; print_string name; print_string " not found"; print_newline ();
    None
  )
)

 
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
and chckSuperclassCallParams call c env =
  let check = (
    let superclass = find_class call.superclass_constructor env.decl_classes
    in match superclass with
    | None ->
      print_string "[Error] Inexistent superclass "; print_string call.superclass_constructor;
      false
    | Some superclass -> (
      let newEnv = (* Updating environment *)
        add_env_varList c.params_class env
      in
        (getVarDeclTypes superclass.params_class).expr_return_types = (getExprTypes call.arguments newEnv).expr_return_types
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
    chckSuperclassCallParams call c env



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
  let env = {
    decl_classes = env.decl_classes;
    decl_vars = env.decl_vars;
    current_class = Some c;
    is_correct_env = env.is_correct_env
  }
  in
	let newEnv = {
		decl_classes = c::env.decl_classes;
		decl_vars = env.decl_vars;
    current_class = env.current_class;
		is_correct_env =
			env.is_correct_env &&
			forbidClassName env.decl_classes c.name_class &&
      chckParamsInClaAndConstr c &&
      chckSuperclassInClaAndConstr c env &&
      List.fold_left (fun prevOK meth -> analyseMethod meth c env && prevOK) true c.methods
	}
    in
    (match newEnv.is_correct_env with
    | true -> print_string c.name_class; print_string " OK"; print_newline ()
    | false -> print_string c.name_class; print_string " KO"; print_newline ());
    newEnv.is_correct_env



and analyseMethod meth c env =
  let newEnv = add_env_varList meth.param_method env
  in
  block_verif meth.body_method newEnv




(**
  ____________________________________________
/         ------------°°°°------------        \
|              ANALYSE : 2- Instructions           
\ ___________________________________________ /
**)


(*-----------------------------------------------------------------------------------------------
                                           Appel
-----------------------------------------------------------------------------------------------*)

and block_verif block env =
  let newEnv = add_env_varList block.declarations env
  in
  List.fold_left (fun prevOK instr -> prevOK && instr_verif instr newEnv) true block.instructions



and instr_verif instr env = 
  match instr with 
  | Exp(e) -> (expr_verif e env).is_correct_expr
  | Block(b) -> block_verif b env
  | Return -> true
  
  | Affectation(c,e) ->
    let verifiedContainer = analyseContainer c env
    in
    let verifiedExpr = expr_verif e env
    in
    let inheritanceOK = (
      match is_subclass_cyclesafe verifiedContainer.expr_return_type verifiedExpr.expr_return_type env.decl_classes with
      | true -> true
      | false -> print_string "[Error] "; print_string verifiedExpr.expr_return_type; print_string " type cannot be assigned to "; print_string verifiedContainer.expr_return_type; print_string " because it's not a subclass of the latter"; print_newline();
      false
    )
    in
      verifiedContainer.is_correct_expr &&
      verifiedExpr.is_correct_expr &&
      inheritanceOK
  
  (* TODO Vérification des types module heritage *)
    (* On doit vérifié que le type de e est un type ou sous type de c *)
  
  | Ite(ifExpr, thenExpr, elseExpr) -> 
    let verifiedIfExpr = expr_verif ifExpr env
    in
      chckExpectedType "Integer" verifiedIfExpr.expr_return_type env &&
      verifiedIfExpr.is_correct_expr &&
      instr_verif thenExpr env &&
      instr_verif elseExpr env


(**
  ____________________________________________
/         ------------°°°°------------        \
|           ANALYSE : 3- Containers           
\ ___________________________________________ /
**)



(* Vérifie que les arguments d'une instantiation "new" correspondent aux paramètres de la classe correspondante *)
and chckParamsInClaAndNew cName arguments env =
    let newClass = find_class cName env.decl_classes
    in match newClass with
    | None ->
      print_string "[Error] Inexistent class in instanciation (new keyword) : "; print_string cName; print_newline ();
      false
    | Some c -> (
        let expectedTypes = getVarDeclTypes c.params_class
        in
        let verifiedExprs = getExprTypes arguments env
        in
          let check =
            is_subclasses_cyclesafe expectedTypes.expr_return_types verifiedExprs.expr_return_types env.decl_classes
          in match check with
          | true -> verifiedExprs.is_correct_exprs
          | false ->
            print_string "[Error] Instanciation of class "; print_string cName; print_string " due to wrong arguments. Expected : "; print_string_list expectedTypes.expr_return_types; print_string " Provided : "; print_string_list verifiedExprs.expr_return_types; print_newline ();
            false
    )



(* Renvoie le nom d'une variable locale et affiche un message d'erreur si la variable n'existe pas *)
and chckLocalVarExistence name env =
  let localVar = 
    List.find_opt (fun var -> var.name = name) env.decl_vars
  in
  match localVar with
  | Some var -> Some var.typ
  | None -> (
    print_string "[Error] Local variable "; print_string name; print_string " doesn't exist in this scope"; print_newline ();
    None
  )


(* Affiche une erreur si un "this" est hors d'une classe, renvoie la classe sinon *)
and chckThiscallInsideClass env =
  match env.current_class with
  | Some c -> env.current_class
  | None ->
    print_string "[Error] Keyword \"this\" can only be used inside a class instance"; print_newline ();
    None


(* Affiche une erreur si le "super" est hors d'une classe ou que ladite classe n'a pas de superclasse, sinon renvoie la superclasse *)
and chckSuperKeywordCall env =
  match env.current_class with
  
  | None ->
    print_string "[Error] Keyword \"super\" can only be used inside a class instance"; print_newline ();
    None
  
  | Some c -> (
    match c.superclass with
    | Some name -> find_class name env.decl_classes
    | None ->
      print_string "[Error] Keyword \"super\" can only be used in classes inheriting at least one other class, unlike "; print_string c.name_class; print_newline();
      None
  )


(* Affiche une erreur si un attribut statique (ou non) n'appartient pas à une classe, et renvoie la définition de la variable si trouvée *)
and chckAttributeExists_staticFilter name (c:class_def) env is_static =
  let (check:variable_def option) = (
    let (check:variable_def option) = find_attribute_in name (filter_attribs_static c.attributes is_static)
    in (
    match check with
    | Some var -> Some var
    | None ->
      (
        match c.superclass with
        | Some classname -> (
          let c = find_class classname env.decl_classes
          in match c with
          | Some c -> chckAttributeExists_staticFilter name c env is_static
          | None -> None
        )
        | None -> None
      )
    )
  )
  in match check with
  | Some var -> Some var
  | None ->
    print_string "[Error] Attribute "; print_string name; print_string " cannot be found in "; print_string c.name_class; print_string " (static : "; print_bool is_static; print_string ")"; print_newline ();
    None


(* Affiche une erreur si une méthode n'appartient pas à une classe, et renvoie la méthode si elle est trouvée *)
and chckMethodExists_staticFilter name (c:class_def) is_static env =
  let (check:methode_def option) = find_method_in name (filter_meths_static c.methods is_static)
  in (
  match check with
  | Some meth -> Some meth
  | None -> (
    let superclass = (
      match c.superclass with
      | None -> None
      | Some classname -> find_class classname env.decl_classes
    )
    in match superclass with
    | Some superclass -> chckMethodExists_staticFilter name superclass is_static env
    | None ->
      print_string "[Error] Method "; print_string name; print_string " cannot be found in "; print_string c.name_class; print_string " (static: "; print_bool is_static; print_string ")"; print_newline ();
      None
    )
  )



and chckMethodExistsAndArgsMatch_staticFilter name (c:class_def) (arguments:expression_t list) (env:environment) is_static =
  let meth = chckMethodExists_staticFilter name c is_static env
  in match meth with
  | None -> (None, false)
  | Some meth -> (
    let verifiedExprs =
      chckMethodArgsVsParams meth arguments env
    in match verifiedExprs with
    | Some verifiedExprs -> (Some meth, verifiedExprs.is_correct_exprs)
    | None -> (None, false)
  )

(* Affiche une erreur si les paramètres et arguments ne correspondent pas, sinon renvoie un exprListUpward *)
and chckMethodArgsVsParams (meth:methode_def) (arguments:expression_t list) env =
  let verifiedExprs = getExprTypes arguments env
  in
  let expectedTypes = (getVarDeclTypes meth.param_method).expr_return_types
  in
  let check =
    is_subclasses_cyclesafe expectedTypes verifiedExprs.expr_return_types env.decl_classes
  in
  match check with
  | true -> Some verifiedExprs
  | false ->
    print_string "[Error] Method "; print_string meth.name_method; print_string " was not given the right arguments. Expected : "; print_string_list expectedTypes; print_string " Provided : "; print_string_list verifiedExprs.expr_return_types; print_newline ();
    None


and chckSelectionClassExistence classname env =
  let c = find_class classname env.decl_classes
  in match c with
  | Some classe -> c
  | None ->
    print_string "[Error] Cannot use selection on a method or attribute returning inexistent type "; print_string classname; print_newline ();
    None






(*-----------------------------------------------------------------------------------------------
                                           Appel
-----------------------------------------------------------------------------------------------*)

and methAttribCallEnd_verif (verifiedExpr:exprUpward) selections env = (
  if verifiedExpr.is_correct_expr then (
    match selections with
    | [] -> verifiedExpr
    | s::lis -> (
      let c = chckSelectionClassExistence verifiedExpr.expr_return_type env
      in (
        match c with
        | None -> emptyExprUpw
        | Some c -> (
          match s with
          | AttrSelect name -> (
            let attrib = chckAttributeExists_staticFilter name c env false
            in (
              match attrib with
              | None -> emptyExprUpw
              | Some attrib -> (
                let verifiedExpr = (
                {
                  expr_return_type = attrib.typ;
                  is_correct_expr = true;
                }
                )
                in
                methAttribCallEnd_verif verifiedExpr lis env
              )
            )
          )
          | MethSelect(name, arguments) -> 
            let (meth, is_correct) = chckMethodExistsAndArgsMatch_staticFilter name c arguments env false
            in (
              match meth with
              | None -> emptyExprUpw
              | Some meth ->
                let typename = (
                  match meth.return_type with
                  | None -> "void"
                  | Some typ -> typ
                )
                in let verifiedExpr =
                {
                  expr_return_type = typename;
                  is_correct_expr = is_correct
                }
                in
                methAttribCallEnd_verif verifiedExpr lis env
            )
        )
      )
    )
  )
  else (
    emptyExprUpw
  )
)


(* Vérifie qu'un attribute_call est valide, puis renvoie le exprUpward correspondant *)
and methAttribCallBeginning_verif (attribOrMeth:attribute_call) (env:environment) =
    match attribOrMeth.beginning with
    | ExpSelect e ->
      let verifiedExpr = expr_verif e env
      in
      methAttribCallEnd_verif verifiedExpr attribOrMeth.selections_to_attrs env

    | ClassSelect classname -> (
      let c = chckClassExistence classname env.decl_classes
      in match c with
      | None -> emptyExprUpw
      | Some c ->
        match attribOrMeth.selections_to_attrs with
        | [] -> raise (Error_no_selection_after_class "Impossible : Missing selection after the class in a selection")
        | sel::lis ->
          match sel with
          | AttrSelect name -> (
            let attrib = chckAttributeExists_staticFilter name c env true
            in match attrib with
            | None -> emptyExprUpw
            | Some attrib ->
              let beginning =
              {
                expr_return_type = attrib.typ;
                is_correct_expr = true
              }
              in
              methAttribCallEnd_verif beginning lis env
          )
          | MethSelect(name, arguments) ->
            let (meth, is_correct) = chckMethodExistsAndArgsMatch_staticFilter name c arguments env true
            in match meth with
            | None -> emptyExprUpw
            | Some meth -> (
              let typename = (
              match meth.return_type with
                | None -> "void"
                | Some typ -> typ
              )
              in
                let beginning =
                  {
                    expr_return_type = typename;
                    is_correct_expr = is_correct
                  }
                in
                methAttribCallEnd_verif beginning lis env
            )
    )



(* Vérifie la validité de l'appel à un container *)
and analyseContainer container env = (
  match container with


  (* Vérifie pour une sélection *)
  (* Ex :    Integer.print(x)       (Point myPoint3D).moveUp(4).y   *)
  | Select ac -> methAttribCallBeginning_verif ac env

  
  (* Vérifie pour une variable locale *)
  (* Ex :      x     myVar     *)
  | LocalVar s -> (
    let typ = chckLocalVarExistence s env
    in (
      match typ with
      | Some typename -> {
          expr_return_type = typename;
          is_correct_expr = true
        }
      | None -> emptyExprUpw
    )
  )

  (* Vérifie pour un This appelé tout seul *)
  | This -> (
    let cla = chckThiscallInsideClass env 
    in match cla with
    | Some c -> { expr_return_type = c.name_class; is_correct_expr = true }
    | None -> emptyExprUpw
  )

  | Super -> (
    let cla = chckSuperKeywordCall env
    in match cla with
    | Some superclass -> {
        expr_return_type = superclass.name_class;
        is_correct_expr = true
    }
    | None -> emptyExprUpw
  )
)


(**
  ____________________________________________
/         ------------°°°°------------        \
|           ANALYSE : 4- Expressions           
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


and chckExpectedType expected actualType env =
  if is_subclass_cyclesafe expected actualType env.decl_classes then
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
  | Container c  -> analyseContainer c env
  | Method m -> methode_verif m env 
  | Binary (op,e1,e2) -> binary_verif op e1 e2 env 
  | Unary (op,e) -> unary_verif op e env
  | Cast(target,e) -> cast_verif e target env
  | NewClass (s,e_list) -> new_verif s e_list env
)



(* Vérifie la validité d'un appel de méthode *)
and methode_verif (mc:method_call) env = 
  let (fakeAttributeCall:attribute_call) =
    {
      beginning = mc.beginning_call;
      selections_to_attrs = mc.selections_to_meths
    }
  in
  methAttribCallBeginning_verif fakeAttributeCall env



and unary_verif (op:unary_operator_t) e env =
  let verifiedExpr = expr_verif e env
  in match op with
  | UMINUS ->
    {
      expr_return_type = "Integer";
      is_correct_expr =
        chckExpectedType "Integer" verifiedExpr.expr_return_type env &&
        verifiedExpr.is_correct_expr
    }



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
        chckExpectedType "Integer" verifiedExpr1.expr_return_type env &&
        chckExpectedType "Integer" verifiedExpr2.expr_return_type env &&
        verifiedExpr1.is_correct_expr &&
        verifiedExpr2.is_correct_expr
      }
  | StringConcat ->
    {
      expr_return_type = "String";
      is_correct_expr =
        chckExpectedType "String" verifiedExpr1.expr_return_type env &&
        chckExpectedType "String" verifiedExpr2.expr_return_type env &&
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
and new_verif classname arguments env =
  {
    expr_return_type = classname;
    is_correct_expr =
      chckParamsInClaAndNew classname arguments env &&
      List.fold_left (fun acc e -> (expr_verif e env).is_correct_expr && acc ) true arguments
  }




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
      List.fold_left (fun is_correct c -> (analyseClass env c) && is_correct) true prog.classes &&
      block_verif prog.program env

