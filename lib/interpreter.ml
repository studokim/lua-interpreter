open Ast

(*** Environment ***)

module Ident = struct
  type t = identifier

  let compare id1 id2 =
    match id1 with
    | Name nm1 ->
      (match id2 with
       | Name nm2 -> compare nm1 nm2)
  ;;
end

module IdentMap = Map.Make (Ident)

let show_var id lit =
  match id with
  | Name name ->
    (match lit with
     | Numeric num -> print_string (name ^ " = " ^ string_of_int num ^ "\n"))
;;

let show_vars env = IdentMap.iter show_var env

(*** Interpreter ***)

let rec execute_expr expression env =
  match expression with
  | Literal lit -> lit
  | Identifier id ->
    (try IdentMap.find id env with
     | Not_found ->
       (match id with
        | Name name ->
          prerr_string ("Identifier `" ^ name ^ "` not declared.\n");
          Numeric (-1)))
  | Binop (left, op, right) ->
    let litl = execute_expr left env in
    let litr = execute_expr right env in
    (match litl with
     | Numeric numl ->
       (match litr with
        | Numeric numr ->
          (match op with
           | AddOp -> Numeric (numl + numr)
           | SubOp -> Numeric (numl - numr)
           | MulOp -> Numeric (numl * numr)
           | DivOp -> Numeric (numl / numr))))
;;

let rec print args env =
  match args with
  | [] -> print_newline ()
  | head :: tail ->
    (match execute_expr head env with
     | Numeric num ->
       print_int num;
       if tail != [] then print_char ' ';
       print tail env)
;;

let execute_stm statement env =
  match statement with
  | Call (Name func, args) ->
    (match func with
     | "print" ->
       print args env;
       env
     | _ ->
       prerr_string ("Function `" ^ func ^ "` not implemented.\n");
       env)
  | Assignment (id, expr) -> IdentMap.add id (execute_expr expr env) env
;;

let rec execute_ast chunk env =
  match chunk with
  | Chunk [] -> env
  | Chunk (head :: tail) -> execute_stm head env |> execute_ast (Chunk tail)
;;

(*** Tests ***)

let vars = IdentMap.empty
let var = Name "var"
let zero = Numeric 0

let ast =
  Chunk
    [ Assignment (var, Literal zero)
    ; Assignment (var, Literal (Numeric 10))
    ; Assignment (var, Binop (Literal (Numeric 3), SubOp, Literal (Numeric 2)))
    ; Assignment (Name "arg1", Literal zero)
    ; Assignment (Name "arg2", Literal (Numeric 1))
    ; Assignment (Name "arg2", Binop (Literal (Numeric 2), AddOp, Literal (Numeric 3)))
    ; Call (Name "print", [ Identifier (Name "arg2"); Literal (Numeric 19); Literal zero ])
    ; Call
        ( Name "print"
        , [ Identifier var
          ; Binop
              ( Binop (Identifier (Name "var"), MulOp, Literal (Numeric 5))
              , MulOp
              , Literal (Numeric 3) )
          ] )
    ; Call (Name "print", [])
    ]
;;
