open Ast

(* Environment *)

module Ident =
struct
    type t = identifier

    let compare id1 id2 =
        match id1 with
        | Name nm1 -> match id2 with
        | Name nm2 -> compare nm1 nm2

end
module IdentMap = Map.Make(Ident)

let show_var id lit =
    match id with Name name ->
        match lit with Numeric num ->
            print_string(name ^ " = " ^ string_of_int num ^ "\n")

let show_vars env = IdentMap.iter show_var env

(* Interpreter *)

let rec print args env =
    match args with
    | [] -> print_newline ()
    | head :: tail -> match head with
        | Identifier id -> (
            try (match IdentMap.find id env with
                | Numeric num -> print_int num; if tail != [] then print_char ' ' ; print tail env
            ) with
            | Not_found -> match id with
                | Name name -> prerr_string ("Identifier `" ^ name ^ "` not declared.\n")
        )
        | Literal lit ->
            match lit with
            | Numeric num -> print_int num; if tail != [] then print_char ' ' ; print tail env

let execute statement env =
    match statement with
    | Call (Name func, args) -> (
        match func with
            | "print" -> print args env; env
            | _ -> prerr_string ("Function `" ^ func ^ "` not implemented.\n") ; env
    )
    | Assignment (id, expr) ->
        match expr with
        | Literal lit -> IdentMap.add id lit env
        | _ -> prerr_endline "Wrong assignment."; env

let rec execute_ast chunk env =
    match chunk with
    | Chunk [] -> env
    | Chunk (hd :: tl) -> execute hd env |> execute_ast (Chunk tl)

(* Tests *)

let vars = IdentMap.empty
let var = Name "var"
let twelwe = Numeric 12

let ast = Chunk (
    Assignment (var, Literal(Numeric 1)) ::
    Assignment (Name "arg1", Literal(Numeric 2)) ::
    Assignment (Name "arg2", Literal(Numeric 3)) ::
    Call (
        Name "print",
        Identifier (Name "arg2") :: Literal(Numeric 19) :: []) ::
    Call (
        Name "print",
        Literal (twelwe) :: Identifier (var) :: []) ::
    Call (
        Name "print",
        []) ::
    [])
