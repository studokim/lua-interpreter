open Ast
open Lexer

type node =
  | NIdentifier of identifier
  | NExpression of expression
  | NStatement of statement

type parser = { program : tokens; result : (node, string) result }

(*** Individual Parsing ***)

let match_token tokens is_parser =
  match tokens.next with Some token -> is_parser token | None -> false

let match_token_at n tokens is_parser =
  match_token (read_token_at n tokens) is_parser

(* unpack Result.Ok, fail otherwise *)
let u = function
  | Result.Ok value -> value
  | Result.Error error -> failwith error

let is_assign = function TAssign -> true | _ -> false
let is_identifier = function TIdentifier _ -> true | _ -> false

let identifier tokens =
  match tokens.next with
  | Some (TIdentifier id) -> (read_token tokens, Result.Ok (Name id))
  | _ -> (read_token tokens, Result.Error ": identifier expected")

let is_operator = function
  | TPlus | TMinus | TAsterisk | TSlash -> true
  | _ -> false

let operator tokens =
  match tokens.next with
  | Some TPlus -> (read_token tokens, Result.Ok AddOp)
  | Some TMinus -> (read_token tokens, Result.Ok SubOp)
  | Some TAsterisk -> (read_token tokens, Result.Ok MulOp)
  | Some TSlash -> (read_token tokens, Result.Ok DivOp)
  | _ -> (read_token tokens, Result.Error ": operator expected")

let is_expression = function
  | TIdentifier _ -> true
  | TLiteral _ -> true
  | _ -> false

let expression tokens =
  match tokens.next with
  | Some (TIdentifier id) ->
      (read_token tokens, Result.Ok (Identifier (Name id)))
  | Some (TLiteral lit) -> (read_token tokens, Result.Ok (Literal (Numeric lit)))
  | _ -> (read_token tokens, Result.Error ": identifier or literal expected")

(*** Combined Parsing ***)
(* fail with exceptions instead of Result.Error *)

(* TODO: use angstrom here to "preserve parsing result and continue" *)
(* TODO: "x=1+2" is parsed as "x=1", after=[Plus, Literal 2] *)
let assign tokens =
  let tokens_next = read_token_at 3 tokens in
  if match_token_at 1 tokens is_assign then
    let _, id = identifier tokens in
    let _, exp = expression (read_token_at 2 tokens) in
    try (tokens_next, Result.Ok (Assignment (u id, u exp)))
    with _ -> (tokens_next, Result.Error ": assignment expected")
  else (tokens_next, Result.Error ": assignment expected")

let binop tokens =
  let tokens_next = read_token_at 3 tokens in
  let _, left = expression tokens in
  let _, op = operator (read_token_at 1 tokens) in
  let _, right = expression (read_token_at 2 tokens) in
  try (tokens_next, Result.Ok (Binop (u left, u op, u right)))
  with _ -> (tokens_next, Result.Error ": binop expected")

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
