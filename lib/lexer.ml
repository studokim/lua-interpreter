(*** Streams ***)

type stream = {
  index : int;
  (* before is backwards ordered *)
  before : char list;
  next : char;
  after : char list;
}

let read_char stm =
  match stm.after with
  | [] ->
      { index = stm.index; before = stm.before; next = Util.eof; after = [] }
  | head :: tail ->
      {
        index = stm.index + 1;
        before = stm.next :: stm.before;
        next = head;
        after = tail;
      }

let unread_char stm =
  match stm.before with
  | [] -> raise End_of_file
  | head :: tail ->
      {
        index = stm.index - 1;
        before = tail;
        next = head;
        after = stm.next :: stm.after;
      }

let stream_of_string str =
  match Util.explode str with
  | [] -> raise End_of_file
  | head :: tail -> { index = 0; before = []; next = head; after = tail }

(*** by Character ***)

exception Syntax_error of string

let syntax_error stm msg =
  raise (Syntax_error (msg ^ " on character " ^ string_of_int stm.index))

let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'

let is_alpha c =
  let code = Char.code c in
  (code >= Char.code 'A' && code <= Char.code 'Z')
  || (code >= Char.code 'a' && code <= Char.code 'z')

(* stops on the first non-blank *)
let rec skip_blank_chars stm =
  let c = stm.next in
  if c = ' ' || c = '\t' || c = '\r' || c = '\n' then
    skip_blank_chars (read_char stm)
  else stm

(*** by Token ***)

(* all lang keywords/symbols *)
type token =
  | Identifier of string
  | Print
  | Literal of int
  | Assign
  | Equals
  | LeftParen
  | RightParen
  | AddOp
  | SubOp
  | MulOp
  | DivOp

type scanner = { stream : stream; token : token option }

(* scans a stream and returns next token *)
let scan scanner =
  let stm = skip_blank_chars scanner.stream in
  let c = stm.next in
  let rec scan_identifier stm acc =
    let stm_next = read_char stm in
    let c_next = stm_next.next in
    if is_alpha c_next || is_digit c_next || c_next = '_' then
      scan_identifier stm_next (acc ^ Char.escaped c_next)
    else if acc = "print" then { stream = read_char stm; token = Some Print }
    else { stream = read_char stm; token = Some (Identifier acc) }
  in
  let rec scan_literal stm acc =
    let stm_next = read_char stm in
    let c_next = stm_next.next in
    if is_digit c_next then scan_literal stm_next (acc ^ Char.escaped c_next)
    else { stream = read_char stm; token = Some (Literal (int_of_string acc)) }
  in
  if is_alpha c then scan_identifier stm (Char.escaped c)
  else if is_digit c then scan_literal stm (Char.escaped c)
  else
    match c with
    | '=' ->
        let stm_next = read_char stm in
        let c_next = stm.next in
        if c_next = '=' then
          { stream = read_char stm_next; token = Some Equals }
        else { stream = read_char stm; token = Some Assign }
    | '(' -> { stream = read_char stm; token = Some LeftParen }
    | ')' -> { stream = read_char stm; token = Some RightParen }
    | '+' -> { stream = read_char stm; token = Some AddOp }
    | '-' -> { stream = read_char stm; token = Some SubOp }
    | '*' -> { stream = read_char stm; token = Some MulOp }
    | '/' -> { stream = read_char stm; token = Some DivOp }
    (* TODO: find out how to compile with Util.eof instead of '\000' *)
    | '\000' -> raise End_of_file
    | _ -> syntax_error stm "couldn't identify the token"

(*** Tests ***)

let blank = stream_of_string "   non_blank   "
let blank_skipped = skip_blank_chars blank
let program = stream_of_string "x + 1"
let s = { stream = program; token = None }
let _ = s
