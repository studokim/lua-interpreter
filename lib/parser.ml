open Angstrom
open Lexer

(*** Individual Parsing ***)

module Identifier = struct
  let is_identifier c = is_alpha c || is_digit c || c = '_'

  let identifier =
    let first =
      peek_char
      >>= function
      | Some c when is_alpha c || c = '_' -> return c
      | _ -> fail "alpha or underscore expected"
    in
    first
    >>= fun first ->
    take_while is_identifier >>= fun rest -> return (Char.escaped first ^ rest)
  ;;

  let parse_identifier code = parse_string ~consume:All identifier code
end

module Numeric = struct
  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 *> return "-"
    | Some '+' -> advance 1 *> return "+"
    | Some c when is_digit c -> return "+"
    | _ -> fail "sign or digit expected"
  ;;

  let int =
    sign
    >>= fun sign ->
    take_while1 is_digit >>= fun integer -> return (int_of_string (sign ^ integer))
  ;;

  let float =
    sign
    >>= fun sign ->
    take_while1 is_digit
    <* char '.'
    >>= fun integer ->
    take_while1 is_digit
    >>= fun fraction -> return (float_of_string (sign ^ integer ^ "." ^ fraction))
  ;;

  let parse_float code = parse_string ~consume:All float code
  let parse_int code = parse_string ~consume:All int code
end

(*** Combined Parsing ***)

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
