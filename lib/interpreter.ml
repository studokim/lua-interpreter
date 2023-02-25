open Ast

(*** Environment ***)

module Environment = struct
  module Variable = struct
    type t = identifier

    let compare id1 id2 =
      match id1 with
      | Name nm1 ->
        (match id2 with
         | Name nm2 -> compare nm1 nm2)
    ;;
  end

  module VariableMap = Map.Make (Variable)

  let show_var id lit =
    match id with
    | Name name ->
      (match lit with
       | Numeric num -> print_string (name ^ " = " ^ string_of_float num ^ "\n")
       | String str -> print_string (name ^ " = \"" ^ str ^ "\"\n"))
  ;;

  let show_vars env = VariableMap.iter show_var env
end

(*** Executor ***)

module Executor = struct
  open Environment

  let rec execute_expr expression env =
    match expression with
    | Literal lit -> lit
    | Identifier id ->
      (try VariableMap.find id env with
       | Not_found ->
         (match id with
          | Name name -> failwith ("identifier `" ^ name ^ "` not declared")))
    | Binop (left, op, right) ->
      let litl = execute_expr left env in
      let litr = execute_expr right env in
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
  ;;

  let execute_stmt statement env =
    match statement with
    | Call (Name func, _) ->
      (match func with
       | "__show_vars" ->
         show_vars env;
         env
       | _ -> failwith ("function `" ^ func ^ "` not implemented"))
    | Assignment (id, expr) -> VariableMap.add id (execute_expr expr env) env
  ;;

  let rec execute_chunk chunk env =
    match chunk with
    | Chunk [] -> env
    | Chunk (head :: tail) -> execute_stmt head env |> execute_chunk (Chunk tail)
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
    ; Call
        (Name "print", [ Identifier (Name "arg2"); Literal (Numeric 19.); Literal zero ])
    ; Call
        ( Name "print"
        , [ Identifier var
          ; Binop
              ( Binop (Identifier (Name "var"), MulOp, Literal (Numeric 5.))
              , MulOp
              , Literal (Numeric 3.) )
          ] )
    ; Call (Name "print", [])
    ]
;;
