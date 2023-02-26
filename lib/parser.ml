open Angstrom
open Ast

(*** Individual Parsing ***)
let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'
;;

let is_alpha c =
  let code = Char.code c in
  (code >= Char.code 'A' && code <= Char.code 'Z')
  || (code >= Char.code 'a' && code <= Char.code 'z')
;;

let whitespace =
  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  take_while is_whitespace
;;

let parens p = char '(' *> whitespace *> p <* whitespace <* char ')'

module Identifier = struct
  let keywords = [ "function"; "end"; "return" ]

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
    take_while is_name
    >>= fun rest ->
    let name = Char.escaped first ^ rest in
    if List.mem name keywords then fail (name ^ "is a keyword") else return (Name name)
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

  (* there could be escaped quotes *)
  let string =
    let is_quote c = c = '"' in
    char '"' *> take_till is_quote <* char '"' >>= fun s -> return (String s)
  ;;

  let bool =
    Angstrom.string "true" *> return (Bool true)
    <|> Angstrom.string "false" *> return (Bool false)
  ;;

  let nil = Angstrom.string "nil" *> return Nil
  let literal = choice [ numeric; string; bool; nil ]
end

(*** Combined Parsing ***)

module Expression = struct
  let identifier = Identifier.name >>= fun result -> return (Identifier result)
  let literal = Literal.literal >>= fun result -> return (Literal result)

  let operator =
    peek_char
    >>= function
    | Some '+' -> advance 1 *> return AddOp
    | Some '-' -> advance 1 *> return SubOp
    | Some '*' -> advance 1 *> return MulOp
    | Some '/' -> advance 1 *> return DivOp
    | _ -> fail "arithmetic operator expected"
  ;;

  let expression =
    fix (fun expression ->
      let binop =
        let binop_constructor left op right = Binop (left, op, right) in
        lift3
          binop_constructor
          (whitespace *> expression <* whitespace)
          operator
          (whitespace *> expression <* whitespace)
      in
      let call =
        let call_constructor id params = Call (id, params) in
        lift2
          call_constructor
          (Identifier.name <* whitespace)
          (parens (sep_by (char ',') (whitespace *> expression <* whitespace)))
      in
      choice [ call; identifier; literal; parens binop ])
  ;;
end

module Statement = struct
  let expression = Expression.expression >>= fun result -> return (Expression result)

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

  let statement =
    fix (fun statement ->
      let body =
        let separator = string ";" <|> whitespace in
        sep_by separator (whitespace *> statement <* whitespace)
      in
      let definition =
        let definition_constructor id args body = Definition (id, args, body) in
        lift3
          definition_constructor
          (string "function" *> whitespace *> Identifier.name <* whitespace)
          (parens (sep_by (char ',') (whitespace *> Identifier.name <* whitespace)))
          (whitespace *> body <* whitespace <* string "end")
      in
      let return =
        string "return" *> whitespace *> (Expression.expression <|> return (Literal Nil))
        >>= fun result -> return (Return result)
      in
      choice [ definition; return; assignment; expression ])
  ;;
end

module Chunk = struct
  let chunk =
    let separator = string ";" <|> whitespace in
    sep_by separator (whitespace *> Statement.statement <* whitespace)
    >>= fun result -> return (Chunk result)
  ;;
end

(*** Helpers ***)

let unpack = function
  | Result.Ok ast_node -> ast_node
  | Result.Error e -> failwith ("parsing error" ^ e)
;;

let parse_all parser string = parse_string ~consume:All parser string
let parse_prefix parser string = parse_string ~consume:Prefix parser string
let parse string = unpack (parse_all Chunk.chunk string)
