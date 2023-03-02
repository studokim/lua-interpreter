open Lua

let interpret_file filepath =
  if Sys.file_exists filepath
  then (
    let program = Util.read_file filepath in
    let _ = Repl.execute program Repl.env in
    ())
  else failwith "file doesn't exist"
;;

let () =
  if Array.length Sys.argv = 2
  then (
    let filepath = Sys.argv.(1) in
    interpret_file filepath)
  else (
    print_endline "Hello from Lua!";
    Repl.iterate Repl.env)
;;
