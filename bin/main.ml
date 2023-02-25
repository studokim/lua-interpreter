open Lua

let () = print_endline "Hello from main!"
let program_example = "x = (1 + 2) ; print(x)"

let vars =
  Interpreter.execute_ast
    (Parser.unpack (Parser.parse_all Parser.Chunk.chunk program_example))
    Interpreter.vars
;;

let () = Interpreter.show_vars vars
