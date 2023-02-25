open Lua

let () = print_endline "Hello from Lua!"
let () = Repl.iterate Repl.env
