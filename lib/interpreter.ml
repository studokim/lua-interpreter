open Ast

exception Error of string
exception Implementation_error of string

let fail msg = raise (Error msg)
let crash msg = raise (Implementation_error msg)

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
    ; funcs : (args * chunk) IdentifierMap.t
    }

  let string_of_identifier = function
    | Name name -> name
  ;;

  let string_of_func id args =
    string_of_identifier id
    ^ " ("
    ^ String.concat ", " (List.map string_of_identifier args)
    ^ ")"
  ;;

  type id_type =
    | Variable
    | Function

  let id_type id env =
    let is_var = IdentifierMap.mem id env.vars in
    let is_func = IdentifierMap.mem id env.funcs in
    if is_var && is_func
    then fail (": ambigous identifier `" ^ string_of_identifier id ^ "`")
    else if is_func
    then Function
    else Variable
  ;;

  let returned = Name "#returned"

  let return_var lit env =
    { vars = IdentifierMap.add returned lit env.vars
    ; funcs = IdentifierMap.remove returned env.funcs
    }
  ;;

  let return_func (args, body) env =
    { vars = IdentifierMap.remove returned env.vars
    ; funcs = IdentifierMap.add returned (args, body) env.funcs
    }
  ;;

  let find_var id env =
    try IdentifierMap.find id env.vars with
    | Not_found -> Nil
  ;;

  let find_func id env =
    try IdentifierMap.find id env.funcs with
    | Not_found -> fail (": function `" ^ string_of_identifier id ^ "` not declared")
  ;;
end

(*** Executor ***)

module rec Builtins : sig
  val is_builtin : Ast.identifier -> bool

  val call_builtin
    :  Ast.identifier
    -> Ast.expression list
    -> Environment.env
    -> Ast.expression * Environment.env
end = struct
  open Environment

  let show_var id value =
    let name = string_of_identifier id in
    match value with
    | Numeric num -> print_string (name ^ " = " ^ string_of_float num ^ "\n")
    | String str -> print_string (name ^ " = \"" ^ str ^ "\"\n")
    | Bool b -> print_string (name ^ " = " ^ string_of_bool b ^ "\n")
    | Nil -> print_string (name ^ " = nil\n")
  ;;

  let show_func id (args, chunk) =
    let args, _ = args, chunk in
    print_endline (string_of_func id args)
  ;;

  let show_env env =
    print_string "-----\n";
    IdentifierMap.iter show_var env.vars;
    IdentifierMap.iter show_func env.funcs;
    print_string "-----\n";
    Literal Nil, env
  ;;

  let import_file params env =
    match params with
    | Literal (String filepath) :: [] ->
      if Sys.file_exists filepath
      then (
        let program = Util.read_file filepath in
        Literal Nil, Chunk.execute (Parser.parse program) env)
      else fail (": file `" ^ filepath ^ "` doesn't exist")
    | _ -> fail ": __import_file(filepath) takes exactly one string argument"
  ;;

  let rec print args env =
    match args with
    | [] ->
      print_newline ();
      Literal Nil, env
    | head :: tail ->
      let _ =
        match Expression.execute head env with
        | Literal Nil, _ -> print_string "nil"
        | Literal (Bool lit), _ -> print_string (string_of_bool lit)
        | Literal (Numeric lit), _ -> print_float lit
        | Literal (String lit), _ -> print_string lit
        | Identifier func, _ ->
          let args, _ = find_func func env in
          print_string (string_of_func func args)
        | _ -> crash ": the expr should've folded to Literal or function Identifier"
      in
      if tail != [] then print_char ' ';
      print tail env
  ;;

  let is_builtin id =
    match string_of_identifier id with
    | "__show_env" -> true
    | "__import_file" -> true
    | "print" -> true
    | _ -> false
  ;;

  let call_builtin id params env =
    match string_of_identifier id with
    | "__show_env" -> show_env env
    | "__import_file" -> import_file params env
    | "print" -> print params env
    | _ -> crash (": `" ^ string_of_identifier id ^ "` is not a builtin function")
  ;;
end

and Function : sig
  val call : identifier -> params -> Environment.env -> expression * Environment.env
end = struct
  open Environment

  let introduce_params args params =
    let introduce_one arg param = Assignment (arg, param) in
    try List.map2 introduce_one args params with
    | Invalid_argument _ ->
      fail
        (": expected "
        ^ string_of_int (List.length args)
        ^ " parameters, "
        ^ string_of_int (List.length params)
        ^ " given")
  ;;

  let call id params env =
    if Builtins.is_builtin id
    then Builtins.call_builtin id params env
    else (
      let args, Chunk body = find_func id env in
      let chunk = Chunk (introduce_params args params @ body) in
      (* Function call can modify env if it declares new variables or functions.
         Since we don't implement Lua's `local` keyword, all these are global *)
      let env = Chunk.execute chunk env in
      (* TODO: fix hidden dependence by data *)
      match id_type returned env with
      | Variable -> Literal (find_var returned env), env
      | Function -> Identifier returned, env)
  ;;
end

and Expression : sig
  val test : expression -> Environment.env -> bool
  val execute : expression -> Environment.env -> expression * Environment.env
end = struct
  open Environment

  let test condition env =
    match condition with
    | Identifier id -> IdentifierMap.mem id env.funcs
    | Literal (Bool lit) -> lit
    | Literal Nil -> false
    | Literal _ -> true
    | _ -> crash ": the condition should've folded to Literal or function Identifier"
  ;;

  let binop left op right env =
    match op with
    | AddOp | SubOp | MulOp | DivOp ->
      (match left, right with
       | Literal (Numeric left), Literal (Numeric right) ->
         (match op with
          | AddOp -> Literal (Numeric (left +. right))
          | SubOp -> Literal (Numeric (left -. right))
          | MulOp -> Literal (Numeric (left *. right))
          | DivOp -> Literal (Numeric (left /. right))
          | _ -> crash ": `op` should've been arithmetic here")
       | _ -> fail ": only operations on numbers are allowed")
    | EqualsOp -> Literal (Bool (left = right))
    | AndOp | OrOp ->
      let left = test left env in
      let right = test right env in
      (match op with
       | AndOp -> Literal (Bool (left && right))
       | OrOp -> Literal (Bool (left || right))
       | _ -> crash ": `op` should've been `and` or `or` here")
  ;;

  let rec execute expression env =
    match expression with
    | Literal lit -> Literal lit, env
    | Identifier id ->
      (match id_type id env with
       | Variable -> Literal (find_var id env), env
       | Function -> Identifier id, env)
    | Binop (left, op, right) ->
      let left, env = execute left env in
      let right, env = execute right env in
      binop left op right env, env
    | Call (id, params) -> Function.call id params env
  ;;
end

and Statement : sig
  val execute : Ast.statement -> Environment.env -> Environment.env
end = struct
  open Environment

  let assign id expr env =
    (* var  = func -> remove; add
       func = func -> none;   add
       var  = var  -> add;    none
       func = var  -> add;    remove
       *)
    match Expression.execute expr env with
    | Literal lit, env ->
      (match id_type id env with
       | Variable -> { vars = IdentifierMap.add id lit env.vars; funcs = env.funcs }
       | Function ->
         { vars = IdentifierMap.add id lit env.vars
         ; funcs = IdentifierMap.remove id env.funcs
         })
    (* TODO: fix hidden dependence
       Expression.execute guarantees that if expr is Identifier, it is a Function *)
    | Identifier func, env ->
      let definition = find_func func env in
      (match id_type id env with
       | Variable ->
         { vars = IdentifierMap.remove id env.vars
         ; funcs = IdentifierMap.add id definition env.funcs
         }
       | Function ->
         { vars = env.vars; funcs = IdentifierMap.add id definition env.funcs })
    | _ ->
      crash
        ": the right-hand expression should've folded to Literal or function Identifier"
  ;;

  let execute statement env =
    match statement with
    | Comment -> env
    | Expression expr ->
      (* i.e. call the function that has side-effects *)
      let _, env = Expression.execute expr env in
      env
    | Assignment (id, expr) -> assign id expr env
    | Branch (condition, thenpart, elsepart) ->
      let condition, env = Expression.execute condition env in
      let condition = Expression.test condition env in
      if condition then Chunk.execute thenpart env else Chunk.execute elsepart env
    | Definition (id, args, body) ->
      if Builtins.is_builtin id
      then fail (": `" ^ string_of_identifier id ^ "` is a builtin function")
      else
        { vars = IdentifierMap.remove id env.vars
        ; funcs = IdentifierMap.add id (args, body) env.funcs
        }
    | Return expr ->
      (match Expression.execute expr env with
       | Literal lit, env -> return_var lit env
       | Identifier func, env -> return_func (find_func func env) env
       | _ ->
         crash
           ": the return expression should've folded to Literal or function Identifier")
  ;;
end

and Chunk : sig
  val execute : Ast.chunk -> Environment.env -> Environment.env
end = struct
  let rec execute chunk env =
    match chunk with
    | Chunk [] -> env
    | Chunk (head :: tail) ->
      (match head with
       | Return _ -> Statement.execute head env
       | _ -> Statement.execute head env |> execute (Chunk tail))
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
