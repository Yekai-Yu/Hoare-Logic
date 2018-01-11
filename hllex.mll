{

open Hlparse
open Hlcommon

exception Eof

}

let numeric = ['0' - '9']
let lower_case = ['a' - 'z']
let alpha = ['a' - 'z' 'A' - 'Z' ]
let id_char = numeric | alpha | "'" | "_"

let whitespace = [' ' '\t' '\n']

rule token = parse
  | [' ' '\t']      { token lexbuf }  (* skip over whitespace *)
  | ['\n']          { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF } 
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "{"     { LBRACE }
  | "}"     { RBRACE }

(* New *)
  | "-->"   { ARROW }   (* for implication *)  

(* B *) 
  | "True"  { TRUE }
  | "False" { FALSE }  
  | "and"   { AND }
  | "not"   { NOT }
  | "<"     { LT }
  | ">"     { GT }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | "="     { EQUALS }

(* E *)
  | "~"     { NEG }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "/"		  { DIV }
  | "mod"   { MOD }
  | "rem"	  { REM }
  | "quot"  { QUOT }

(* C *)
  | "skip"  { SKIP }
  | ";"     { SEMI }    (* for sequence *)
  | ":="    { ASSIGN }  (* for middle program to assign *)
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fi"    { FI }
  | "while" { WHILE }
  | "do"    { DO }
  | "od"    { OD }

(* N *)
  | numeric+ as s                    { INT (int_of_string s) }

(* I *)
  | (lower_case (id_char*)) as s     { IDENT s }

(* do not modify this function: *)
{ let lextest s = token (Lexing.from_string s)

  let get_all_tokens s =
      let b = Lexing.from_string (s^"\n") in
      let rec g () =
      match token b with EOF -> []
      | t -> t :: g () in
      g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }
