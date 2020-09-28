(* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html *)

{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t']     { token lexbuf } (* skip blanks *)
  | ['\n' ]        { EOL }          (* end of line *)
  | ['0'-'9']+
    as str         { INT(int_of_string str) } (* integer *)
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }    (* end of file (CTRL-D) *)
