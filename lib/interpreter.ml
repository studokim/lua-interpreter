open Ast

(*** Environment ***)

module Environment = struct
  module Identifier = struct
    type t = identifier

    let compare id1 id2 =
      match id1 with
      | Name nm1 ->
        (match id2 with
         | Name nm2 -> compare nm1 nm2)
    ;;
  end

  module IdentifierMap = Map.Make (Identifier)

  type mode =
    | Debug
    | Release

  type env =
    { mode : mode
    ; vars : literal IdentifierMap.t
    ; funcs : func IdentifierMap.t
    }

  let string_of_identifier = function
    | Name name -> name
  ;;

  let show_var id = function
    | Numeric num ->
      print_string (string_of_identifier id ^ " = " ^ string_of_float num ^ "\n")
    | String str -> print_string (string_of_identifier id ^ " = \"" ^ str ^ "\"\n")
  ;;

  let show_vars vars = IdentifierMap.iter show_var vars

  let show_func id = function
    | args, _ ->
      print_string
        (string_of_identifier id
        ^ " ("
        ^ String.concat ", " (List.map string_of_identifier args)
        ^ ")\n")
  ;;

  let show_funcs funcs = IdentifierMap.iter show_func funcs

  let show_env env =
    show_vars env.vars;
    show_funcs env.funcs
  ;;
end

(*** Executor ***)

module Executor = struct
  open Environment

  let rec execute_expression expression env =
    match expression with
    | Literal lit -> lit
    | Identifier id ->
      (try IdentifierMap.find id env.vars with
       | Not_found ->
         failwith ("identifier `" ^ string_of_identifier id ^ "` not declared"))
    | Binop (left, op, right) ->
      let litl = execute_expression left env in
      let litr = execute_expression right env in
      (match litl with
       | Numeric numl ->
         (match litr with
          | Numeric numr ->
            (match op with
             | AddOp -> Numeric (numl +. numr)
             | SubOp -> Numeric (numl -. numr)
             | MulOp -> Numeric (numl *. numr)
             | DivOp -> Numeric (numl /. numr))
          | String _ -> failwith "cannot operate with strings")
       | String _ -> failwith "cannot operate with strings")
    | CallExpr (id, _) ->
      (try
         let _ = IdentifierMap.find id env.funcs in
         failwith "call not implemented"
       with
       | Not_found -> failwith ("function `" ^ string_of_identifier id ^ "` not declared"))
  ;;

  let execute_statement statement env =
    match statement with
    | Assignment (id, expr) ->
      { mode = env.mode
      ; vars = IdentifierMap.add id (execute_expression expr env) env.vars
      ; funcs = env.funcs
      }
    | CallStmt expr ->
      (match expr with
       | CallExpr (id, _) ->
         (match string_of_identifier id with
          | "__show_vars" ->
            show_vars env.vars;
            env
          | "__show_funcs" ->
            show_funcs env.funcs;
            env
          | "__show_env" ->
            show_env env;
            env
          | "__release" -> { mode = Release; vars = env.vars; funcs = env.funcs }
          | "__debug" -> { mode = Debug; vars = env.vars; funcs = env.funcs }
          | _ ->
            (try
               let _ = IdentifierMap.find id env.funcs in
               failwith ("call of `" ^ string_of_identifier id ^ "` not implemented")
             with
             | Not_found ->
               failwith ("function `" ^ string_of_identifier id ^ "` not declared")))
       | _ -> failwith "function call expected")
    | Definition (id, func) ->
      { mode = env.mode; vars = env.vars; funcs = IdentifierMap.add id func env.funcs }
  ;;

  let rec execute_chunk chunk env =
    match chunk with
    | Chunk [] -> env
    | Chunk (head :: tail) -> execute_statement head env |> execute_chunk (Chunk tail)
  ;;
end

(*** Tests ***)

let var = Name "var"
let zero = Numeric 0.

let ast =
  Chunk
    [ Assignment (var, Literal zero)
    ; Assignment (var, Literal (Numeric 10.))
    ; Assignment (var, Binop (Literal (Numeric 3.), SubOp, Literal (Numeric 2.)))
    ; Assignment (Name "arg1", Literal zero)
    ; Assignment (Name "arg2", Literal (Numeric 1.))
    ; Assignment (Name "arg2", Binop (Literal (Numeric 2.), AddOp, Literal (Numeric 3.)))
    ; CallStmt
        (CallExpr
           ( Name "print"
           , [ Identifier (Name "arg2"); Literal (Numeric 19.); Literal zero ] ))
    ; CallStmt
        (CallExpr
           ( Name "print"
           , [ Identifier var
             ; Binop
                 ( Binop (Identifier (Name "var"), MulOp, Literal (Numeric 5.))
                 , MulOp
                 , Literal (Numeric 3.) )
             ] ))
    ; CallStmt (CallExpr (Name "print", []))
    ]
;;
