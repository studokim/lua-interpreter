(* open Ast *)
open Lexer
(* open Util *)

(*** Parsing ***)

(* let build_ast (stm : stream) : chunk = Chunk [] *)

(* TODO: use angstrom here to "preserve parsing result and continue" *)
let parse_binop tokens : Ast.expression =
  match tokens.next with
  | None -> raise End_of_file
  | Some t -> (
      match t with
      | Identifier i -> (
          let ts_next = read_token tokens in
          match ts_next.next with
          | None -> raise End_of_file
          | Some t -> (
            match t with
            | Assign -> (
              let ts_next = read_token ts_next in
              )
            | _ -> raise End_of_file

(*** Tests ***)

let test_assignment = "x = 1"

(*
   x = 1
   >>>
   [Identifier "x"; Assign; Literal 1]
   >>>
   Chunk (Assignment (Name "x", Literal (Numeric 1)) :: [])
*)
let test_binop = "x = 1 + 2"
(*
  Chunk (Assignment (Name "x", Binary (Literal (Numeric 1), "+", Literal (Numeric 2))) :: [])
*)
