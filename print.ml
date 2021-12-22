open Ast

(* imprime une expression sous forme entierement parenthésée, de façon à
 * ce qu'on puisse facilement verifier si les précédences et associativités
 * demandées sont bien respectées.
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
       print_string (Misc.string_of_relop op); printExpr d; print_string "]"
    | Selection () ->
    | Instanciation () -> 
    | Envoi () -> 
    | Cast () ->

(* PRINT CLASS *) (* A VERIFIER *)
let printClass c =
  print_string " CLASS"; print_string c.name; print_string "( "; List.iter printDecl c.attribut;
  print_string " )";
  match c.superClasse with 
      | None -> ""
      | Some s -> print_string "EXTENDS"; print_string s;
  print_string "IS";
  print_string " { ";
  printbloc c.method;
  print_string " } ";

(* PRINT METHOD *) (* A VERIFIER *)
let printmethod m =
  print_string "DEF";
  match m.static with 
    | false -> print_string " " 
    | true -> print_string "STATIC";
   match m.override with 
    | false -> print_string " " 
    | true -> print_string "OVERRIDE";
   print_string m.nom;
   List.iter printDecl m.param;
   match c._____ with 
    |true -> print_string ":" ; print_string  m.typretour ; print_string ":="; printInstr m.instruction (* A VOIR*)
    |false -> print_string "IS" ; printBloc ______ (* JE SUIS PAS SUR *) 

(* PRINT Instruction *) (* A VERIFIER *)
let printInstr ins =
  match ins with 
  |Exp() ->
  |Ite(,,) ->
  |Return ->
  |Affectation(,) ->

(* PRINT Bloc *) (* A VERIFIER *)
let printBloc b = 
   List.iter printDecl b.declarations;
   List.iter printInstr b.instructions

(* PRINT Déclaration *) (* A VERIFIER *)
let printDecl d =
  print_string "VAR"; print_string "STATIC"; print_string d.nom; print_string " : "; printExpr d.typ;
  print_newline ()

let printAll ld e =
  List.iter printDecl ld;
  printExpr e;
  print_newline ()

