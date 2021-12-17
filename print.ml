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
  (*
     print_string " CLASS "; printExpr sinon;
     print_string " THIS "; printExpr sinon;
     print_string " SUPER "; printExpr sinon;
     print_string " ELSE "; printExpr sinon;
     print_string " ELSE "; printExpr sinon;
     print_endline "]"
*)

let printClass c =
  print_string c.name;
  print_string "( ";
  List.iter printDecl d.attribut;
  print_string " )";
  match c.superClasse with 
      None -> ""
      | Some s -> print_string s;
  



let printDecl d =
  print_string d.nom; print_string " : "; printExpr d.typ;
  print_newline ()

let printAll ld e =
  List.iter printDecl ld;
  printExpr e;
  print_newline ()

type classType = {
  name : string; 
  superClasse : string option; 
  attribut : decl list; (* param + attribut *)
  meth : methode list;
  constructeur : methode;
  }