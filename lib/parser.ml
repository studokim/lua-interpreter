open Ast
open Lexer

(*** Individual Parsing ***)

let match_token tokens parser =
  match tokens.next with Some token -> parser token | None -> false

let match_token_at n tokens parser = match_token (read_token_at n tokens) parser

(* unpack Result.Ok, fail *)
let u = function Result.Ok v -> v | Result.Error e -> failwith e
let is_assign = function TAssign -> true | _ -> false
let is_identifier = function TIdentifier _ -> true | _ -> false

let identifier = function
  | Some (TIdentifier id) -> Result.Ok (Name id)
  | _ -> Result.Error ": identifier expected"

let is_operator = function
  | TPlus | TMinus | TAsterisk | TSlash -> true
  | _ -> false

let operator = function
  | Some TPlus -> Result.Ok AddOp
  | Some TMinus -> Result.Ok SubOp
  | Some TAsterisk -> Result.Ok MulOp
  | Some TSlash -> Result.Ok DivOp
  | _ -> Result.Error ": operator expected"

let is_expression = function
  | TIdentifier _ -> true
  | TLiteral _ -> true
  | _ -> false

let expression = function
  | Some (TIdentifier id) -> Result.Ok (Identifier (Name id))
  | Some (TLiteral lit) -> Result.Ok (Literal (Numeric lit))
  | _ -> Result.Error ": identifier or literal expected"

(*** Combined Parsing ***)
(* fail with exceptions instead of Result.Error *)

(* TODO: use angstrom here to "preserve parsing result and continue" *)
let parse_assign tokens =
  if match_token_at 1 tokens is_assign then
    let id = identifier tokens.next in
    let lit = expression (read_token_at 2 tokens).next in
    Assignment (u id, u lit)
  else failwith ": assignment expected"

let parse_binop tokens =
  let left = expression tokens.next in
  let op = operator (read_token_at 1 tokens).next in
  let right = expression (read_token_at 2 tokens).next in
  Binop (u left, u op, u right)

(*** Tests ***)

let test_assign = scan_string "x = 1"
(*
   Chunk (Assignment (Name "x", Literal (Numeric 1)) :: [])
*)

let test_binop = scan_string "1 + 2"
(*
  Chunk (Binary (Literal (Numeric 1), "+", Literal (Numeric 2)) :: [])
*)

let test_assign_binop = scan_string "x = 1 + 2"
(*
  Chunk (Assignment (Name "x", Binary (Literal (Numeric 1), "+", Literal (Numeric 2))) :: [])
*)
