(* https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html *)

open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let rec read = fun () -> (
      let result = Parser.main Lexer.token lexbuf in
      print_int result; print_newline (); flush stdout;
      read ()
    ) in
    read ()
  with Lexer.Eof ->
    exit 0
