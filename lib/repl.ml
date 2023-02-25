open Interpreter
open Environment

let read_input () =
  let rec next_line acc =
    match read_line () with
    | "" -> acc
    | line ->
      print_string ">> ";
      next_line (acc @ [ String.trim line ])
  in
  print_string ">  ";
  String.concat " ; " (next_line [])
;;

let execute input env = Executor.execute_chunk (Parser.parse input) env
let env = { mode = Release; vars = IdentifierMap.empty; funcs = IdentifierMap.empty }

let rec iterate env =
  try
    let input = read_input () in
    let env_updated = execute input env in
    let () = if env_updated.mode = Debug then show_env env_updated in
    iterate env_updated
  with
  | End_of_file -> exit 0
  | Failure m ->
    print_endline m;
    iterate env
;;
