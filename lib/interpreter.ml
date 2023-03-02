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

  type env =
    { vars : literal IdentifierMap.t
    ; funcs : (args * body) IdentifierMap.t
    }

  let string_of_identifier = function
    | Name name -> name
  ;;

  type id_type =
    | Variable
    | Function
    | Not_declared

  let id_type id env =
    let is_var = IdentifierMap.mem id env.vars in
    let is_func = IdentifierMap.mem id env.funcs in
    if is_var && is_func
    then failwith ("ambigous identifier `" ^ string_of_identifier id ^ "`\n")
    else if is_var
    then Variable
    else if is_func
    then Function
    else Not_declared
  ;;

  let find_var id env =
    try IdentifierMap.find id env.vars with
    | Not_found -> failwith ("variable `" ^ string_of_identifier id ^ "` not declared\n")
  ;;

  let find_func id env =
    try IdentifierMap.find id env.funcs with
    | Not_found -> failwith ("function `" ^ string_of_identifier id ^ "` not declared\n")
  ;;

  let show_var id value =
    let name = string_of_identifier id in
    match value with
    | Numeric num -> print_string (name ^ " = " ^ string_of_float num ^ "\n")
    | String str -> print_string (name ^ " = \"" ^ str ^ "\"\n")
    | Bool b -> print_string (name ^ " = " ^ string_of_bool b ^ "\n")
    | Nil -> print_string (name ^ " = nil\n")
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

  let introduce_params (args : args) (params : params) : statement list =
    let introduce_one (arg : identifier) (param : expression) : statement =
      Assignment (arg, param)
    in
    try List.map2 introduce_one args params with
    | Invalid_argument _ ->
      failwith
        ("expected "
        ^ string_of_int (List.length args)
        ^ " parameters, "
        ^ string_of_int (List.length params)
        ^ " given")
  ;;

  let is_builtin_func id =
    match string_of_identifier id with
    | "__show_vars" -> true
    | "__show_funcs" -> true
    | "__show_env" -> true
    | _ -> false
  ;;

  let call_builtin_func id env =
    match string_of_identifier id with
    | "__show_vars" ->
      show_vars env.vars;
      Literal Nil
    | "__show_funcs" ->
      show_funcs env.funcs;
      Literal Nil
    | "__show_env" ->
      show_env env;
      Literal Nil
    | _ -> failwith "not a builtin function"
  ;;

  let rec execute_expression expression env =
    match expression with
    | Literal lit -> Literal lit
    | Identifier id ->
      (match id_type id env with
       | Variable -> Literal (find_var id env)
       | Function -> Identifier id
       | Not_declared ->
         failwith ("identifier `" ^ string_of_identifier id ^ "` not declared\n"))
    | Binop (left, op, right) ->
      let left = execute_expression left env in
      let right = execute_expression right env in
      (match left with
       | Literal (Numeric left) ->
         (match right with
          | Literal (Numeric right) ->
            (match op with
             | AddOp -> Literal (Numeric (left +. right))
             | SubOp -> Literal (Numeric (left -. right))
             | MulOp -> Literal (Numeric (left *. right))
             | DivOp -> Literal (Numeric (left /. right)))
          | _ -> failwith "only operations on numbers are allowed")
       | _ -> failwith "only operations on numbers are allowed")
    | Call (id, params) ->
      if is_builtin_func id
      then call_builtin_func id env
      else (
        let args, body = find_func id env in
        let chunk = Chunk (introduce_params args params @ body) in
        (* TODO: function call can modify env on Return *)
        let expr, _ = execute_chunk chunk env in
        expr)

  and execute_statement statement env =
    match statement with
    | Comment -> env
    | Expression expr ->
      (* i.e. call the function that has side-effects *)
      let _ = execute_expression expr env in
      env
    | Assignment (id, expr) ->
      let expr = execute_expression expr env in
      (* var       = func -> remove; add
         func|none = func -> none;   add
         var|none  = var  -> add;    none
         func      = var  -> add;    remove
         *)
      (match expr with
       (* execute_expression guarantees that if expr is Identifier, it is a Function *)
       | Identifier func ->
         let definition = find_func func env in
         (match id_type id env with
          | Variable ->
            { vars = IdentifierMap.remove id env.vars
            ; funcs = IdentifierMap.add id definition env.funcs
            }
          | Function | Not_declared ->
            { vars = env.vars; funcs = IdentifierMap.add id definition env.funcs })
       | Literal lit ->
         (match id_type id env with
          | Variable | Not_declared ->
            { vars = IdentifierMap.add id lit env.vars; funcs = env.funcs }
          | Function ->
            { vars = IdentifierMap.add id lit env.vars
            ; funcs = IdentifierMap.remove id env.funcs
            })
       | _ ->
         failwith "the right-hand expression should've folded to Identifier or Literal")
    | Definition (id, args, body) ->
      if is_builtin_func id
      then failwith ("`" ^ string_of_identifier id ^ "` is a builtin function")
      else { vars = env.vars; funcs = IdentifierMap.add id (args, body) env.funcs }
    | Return _ -> failwith "should've returned in `execute_chunk`"

  and execute_chunk chunk env =
    match chunk with
    | Chunk [] -> Literal Nil, env
    | Chunk (head :: tail) ->
      (match head with
       | Return expr -> expr, env
       | _ -> execute_statement head env |> execute_chunk (Chunk tail))
  ;;
end

(*** Tests ***)

let ast =
  let var = Name "var" in
  let zero = Numeric 0. in
  Chunk
    [ Assignment (var, Literal zero)
    ; Assignment (var, Literal (Numeric 10.))
    ; Assignment (var, Binop (Literal (Numeric 3.), SubOp, Literal (Numeric 2.)))
    ; Assignment (Name "arg1", Literal zero)
    ; Assignment (Name "arg2", Literal (Numeric 1.))
    ; Assignment (Name "arg2", Binop (Literal (Numeric 2.), AddOp, Literal (Numeric 3.)))
    ; Expression
        (Call
           ( Name "print"
           , [ Identifier (Name "arg2"); Literal (Numeric 19.); Literal zero ] ))
    ; Expression
        (Call
           ( Name "print"
           , [ Identifier var
             ; Binop
                 ( Binop (Identifier (Name "var"), MulOp, Literal (Numeric 5.))
                 , MulOp
                 , Literal (Numeric 3.) )
             ] ))
    ; Expression (Call (Name "print", []))
    ]
;;
