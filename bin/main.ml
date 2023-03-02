open Lua

let interpret_file filepath =
  let program = Util.read_file filepath in
  let _ = Repl.execute program Repl.env in
  ()
;;

let () =
  if Array.length Sys.argv = 2
  then (
    let file_path = Sys.argv.(1) in
    if Sys.file_exists file_path
    then interpret_file file_path
    else failwith "file doesn't exist")
  else (
    print_endline "Hello from Lua!";
    Repl.iterate Repl.env)
;;
