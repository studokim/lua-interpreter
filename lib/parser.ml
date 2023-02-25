open Angstrom
open Ast
open Lexer

(*** Individual Parsing ***)

let whitespace =
  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  take_while is_whitespace
;;

let parens p = char '(' *> whitespace *> p <* whitespace <* char ')'

module Identifier = struct
  let name =
    let is_name c = is_alpha c || is_digit c || c = '_' in
    let first =
      peek_char
      >>= function
      | Some c when is_alpha c || c = '_' -> advance 1 *> return c
      | _ -> fail "alpha or underscore expected"
    in
    first
    >>= fun first ->
    take_while is_name >>= fun rest -> return (Name (Char.escaped first ^ rest))
  ;;
end

module Literal = struct
  (* there could be exponents https://www.lua.org/pil/2.3.html *)
  let numeric =
    let sign =
      peek_char
      >>= function
      | Some '-' -> advance 1 *> return "-"
      | Some '+' -> advance 1 *> return "+"
      | Some c when is_digit c -> return "+"
      | _ -> fail "sign or digit expected"
    in
    sign
    >>= fun sign ->
    take_while1 is_digit
    >>= fun integer ->
    peek_char
    >>= function
    | Some '.' ->
      advance 1 *> take_while1 is_digit
      >>= fun fraction ->
      return (Numeric (float_of_string (sign ^ integer ^ "." ^ fraction)))
    | _ -> return (Numeric (float_of_string (sign ^ integer)))
  ;;
end

(*** Combined Parsing ***)

module Expression = struct
  let identifier = Identifier.name >>= fun result -> return (Identifier result)
  let literal = Literal.numeric >>= fun result -> return (Literal result)

  let binop =
    let operator =
      peek_char
      >>= function
      | Some '+' -> advance 1 *> return AddOp
      | Some '-' -> advance 1 *> return SubOp
      | Some '*' -> advance 1 *> return MulOp
      | Some '/' -> advance 1 *> return DivOp
      | _ -> fail "arithmetic operator expected"
    in
    fix (fun expression ->
      let binop_constructor left op right = Binop (left, op, right) in
      let binop =
        lift3
          binop_constructor
          (whitespace *> expression <* whitespace)
          operator
          (whitespace *> expression <* whitespace)
      in
      choice [ identifier; parens identifier; literal; parens literal; parens binop ])
  ;;

  let expression = binop
end

module Statement = struct
  let assignment =
    let operator =
      peek_char
      >>= function
      | Some '=' -> advance 1
      | _ -> fail "assignment operator expected"
    in
    let assignment_constructor id expr = Assignment (id, expr) in
    lift2
      assignment_constructor
      (Identifier.name <* whitespace <* operator)
      (whitespace *> Expression.expression)
  ;;

  let call =
    let separator = char ',' in
    let call_constructor id exprs = Call (id, exprs) in
    lift2
      call_constructor
      (Identifier.name <* whitespace)
      (parens (sep_by separator (whitespace *> Expression.expression <* whitespace)))
  ;;

  let statement = choice [ assignment; call ]
end

module Chunk = struct
  let chunk =
    let separator = string ";" <|> whitespace in
    let chunk_constructor stmts = Chunk stmts in
    lift
      chunk_constructor
      (sep_by separator (whitespace *> Statement.statement <* whitespace))
  ;;
end

(*** Tests ***)

let parse_all parser string = parse_string ~consume:All parser string
let parse_prefix parser string = parse_string ~consume:Prefix parser string

let unpack = function
  | Result.Ok r -> r
  | _ -> failwith "cannot unpack"
;;

let test_assign = "x = 1"
(*
   Chunk (Assignment (Name "x", Literal (Numeric 1)) :: [])
*)

let test_binop = "1 + 2"
(*
  Chunk (Binary (Literal (Numeric 1), "+", Literal (Numeric 2)) :: [])
*)

let test_assign_binop = "x = 1 + 2"
(*
  Chunk (Assignment (Name "x", Binary (Literal (Numeric 1), "+", Literal (Numeric 2))) :: [])
*)
