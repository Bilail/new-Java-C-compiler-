{
open Ast
open Parse
open Lexing
exception Eof

(* gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

(* cree une table de hachage qu'on remplit avec le token associe
 * a chaque mot-clef
 *)
let keyword_table = Hashtbl.create 16
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "if", IF;
      "then", THEN;
      "else", ELSE;
      "class" , CLASSE;
      "super" , SUPER;
      "this" , THIS;
      (*"result", RESULT;*)
      "new", NEW;
      "var", VAR;
      "def", DEF;
      "is", IS;
      "static", STATIC;
      "override", OVERRIDE;
      "extends", EXTENDS
    ]
}

let lettre = ['A'-'Z' 'a'-'z']
let l_MAJ = ['A'-'Z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )

rule
 comment = parse
             "*/" { (* fin de commentaire trouvee. Le commentaire ne doit pas
                     * etre transmis. On renvoie donc ce que nous renverra un
                     * nouvel appel a l'analyseur lexical
                     *)
                    token lexbuf
                  }
  | '\n'           { (* incremente le compteur de ligne et poursuit la
                      * reconnaissance du commentaire en cours
                      *)
                     new_line lexbuf; comment lexbuf
                   }
  | eof            { (* detecte les commentaires non fermes pour pouvoir
                      * faire un message d'erreur clair.
                      * On pourrait stocker la position du dernier commentaire
                      * encore ouvert pour ameliorer le dioagnostic
                      *)
                     raise (MISC_Error "unclosed comment")
                   }
  | _              { (* rien a faire de special pour ce caractere, donc on
                      * poursuit la reconnaissance du commentaire en cours
                      *)
                     comment lexbuf
                   }
 and
 read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  (*| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }*)


and
 token = parse
    l_MAJ LC* as classe_name { CLASSNAME classe_name }
    | lettre LC * as id
      { (* id contient le texte reconnu. On verifie s'il s'agit d'un mot-clef
         * auquel cas on renvoie le token associe. Sinon on renvoie Id avec le
         * texte reconnu en valeur 
         *)
        try
          Hashtbl.find keyword_table id
        with Not_found -> ID id
      }
    

  | [' ''\t''\r']+  { (* consommer les delimiteurs, ne pas les transmettre
                       * et renvoyer ce que renverra un nouvel appel a
                       *  l'analyseur lexical
                       *)
                       token lexbuf
                    }
  | '\n'     { next_line lexbuf; token lexbuf}
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | chiffre+ as lxm { CSTE(int_of_string lxm) }
  | "/*"           { comment lexbuf }
  | '+'            { PLUS }
  | '&'            {CONCAT}
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACKET }
  | '}'            { RBRACKET }
  | ';'            { SEMICOLON }
  | ':'            { COLON }
  | ','            { COMMA }
  | '.'            { SELECTION }
  | ":="           { ASSIGN }
  | "<"		         { RELOP (Ast.LT) }
  | "<="           { RELOP (Ast.LE) }
  | ">"            { RELOP (Ast.GT) }
  | ">="           { RELOP (Ast.GE) }
  | "="            { RELOP (Ast.EQ) }
  | "<>"           { RELOP (Ast.NEQ) }
  | eof            { EOF } 
  | _ as lxm       { (* action par défaut: filtre un unique caractere, different
                      * de ceux qui precedent. Il s'agit d'un caratere errone:
                      * on le signale et on poursuit quand meme l'analyse.
                      * On aurait pu décider de lever une exception et
                      * arreter l'analyse.
                      *)
                     print_endline
                       ("undefined character: " ^ (String.make 1 lxm));
                     token lexbuf

  }


