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
      return (Literal (Numeric (float_of_string (sign ^ integer ^ "." ^ fraction))))
    | _ -> return (Literal (Numeric (float_of_string (sign ^ integer))))
  ;;
end

(*** Combined Parsing ***)

module Expression = struct
  let identifier =
    let is_name c = is_alpha c || is_digit c || c = '_' in
    let first =
      peek_char
      >>= function
      | Some c when is_alpha c || c = '_' -> advance 1 *> return c
      | _ -> fail "alpha or underscore expected"
    in
    first
    >>= fun first ->
    take_while is_name
    >>= fun rest -> return (Identifier (Name (Char.escaped first ^ rest)))
  ;;

  let operator =
    peek_char
    >>= function
    | Some '+' -> advance 1 *> return AddOp
    | Some '-' -> advance 1 *> return SubOp
    | Some '*' -> advance 1 *> return MulOp
    | Some '/' -> advance 1 *> return DivOp
    | _ -> fail "arithmetic operator expected"
  ;;

  let parens p = char '(' *> p <* char ')'

  let binop =
    fix (fun expression ->
      let binop_constructor left op right = Binop (left, op, right) in
      let binop =
        lift3
          binop_constructor
          (whitespace *> expression)
          (whitespace *> operator <* whitespace)
          (expression <* whitespace)
      in
      choice [ identifier; Literal.numeric; parens binop ])
  ;;
end

(*** Tests ***)

let parse_all parser string = parse_string ~consume:All parser string
let parse_prefix parser string = parse_string ~consume:Prefix parser string

let unpack = function
  | Result.Ok r -> r
  | _ -> failwith "cannot unpack"
;;

let wrap_stmt = function
  | s -> Chunk [ s ]
;;

let wrap_expr = function
  | e -> Chunk [ Expression e ]
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
