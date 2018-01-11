open Hlcommon
open Hllex
open Hlparse


let process kind processor input =
    try
      if input = "" then ParseEmpty else
      ParseOk (processor token (Lexing.from_string input))
    with Parsing.Parse_error ->
      (* prerr_endline ("Syntax error in " ^ kind ^ ": " ^ input); *)
      SyntaxError input

(* Try to parse each part of the node. If there are errors, sets
   the `errors` flag to `true` and prints a syntax error. Since
   we expect the program will be killed after all the syntax errors
   are found, we can replace the parse output with some garbage to
   make it typecheck. *)
let process_node (node : unprocessed_node) =
  let label         = label_of_string node.str_label in
  let left          = process "pre_cond"       condition_exp   node.str_left in
  let middle        = process "command"        com_expression  node.str_middle in
  let right         = process "post_cond"      condition_exp   node.str_right in
  let sideCondition = process "side_condition" condition_exp   node.str_sideCondition in
  {label;left;middle;right;sideCondition}

let processed_tree tree =
  List.map (fun (key, node) -> (key, process_node node)) tree
