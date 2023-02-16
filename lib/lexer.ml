let eof = '\000'

(*** Streams ***)

type stream = {
  (* numbering starts from 0, last index of the N-character-long stream is N,
     there are eofs infinitely to the left and to the right of the string *)
  index : int;
  (* before is backwards ordered *)
  before : char list;
  next : char;
  after : char list;
}

let read_char stm =
  match stm.after with
  | [] -> { index = stm.index + 1; before = stm.before; next = eof; after = [] }
  | head :: tail ->
      {
        index = stm.index + 1;
        before = stm.next :: stm.before;
        next = head;
        after = tail;
      }

let unread_char stm =
  match stm.before with
  | [] -> { index = stm.index - 1; before = []; next = eof; after = stm.after }
  | head :: tail ->
      {
        index = stm.index - 1;
        before = tail;
        next = head;
        after = stm.next :: stm.after;
      }

let stream_of_string str =
  match Util.explode str with
  | [] -> { index = 0; before = []; next = eof; after = [] }
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
type keyword = Print

type token =
  | TKeyword of keyword
  | TIdentifier of string
  | TLiteral of int
  | TAssign
  | TEquals
  | TLeftParen
  | TRightParen
  | TPlus
  | TMinus
  | TAsterisk
  | TSlash

type scanner = { stream : stream; token : token option }
type tokens = { before : token list; next : token option; after : token list }

(* scans a stream and returns next token *)
let scan_token scanner =
  let stm = skip_blank_chars scanner.stream in
  let c = stm.next in
  let rec scan_identifier stm acc =
    let stm_next = read_char stm in
    let c_next = stm_next.next in
    if is_alpha c_next || is_digit c_next || c_next = '_' then
      scan_identifier stm_next (acc ^ Char.escaped c_next)
    else if acc = "print" then
      { stream = read_char stm; token = Some (TKeyword Print) }
    else { stream = read_char stm; token = Some (TIdentifier acc) }
  in
  let rec scan_literal stm acc =
    let stm_next = read_char stm in
    let c_next = stm_next.next in
    if is_digit c_next then scan_literal stm_next (acc ^ Char.escaped c_next)
    else { stream = read_char stm; token = Some (TLiteral (int_of_string acc)) }
  in
  if is_alpha c then scan_identifier stm (Char.escaped c)
  else if is_digit c then scan_literal stm (Char.escaped c)
  else
    match c with
    | '=' ->
        let stm_next = read_char stm in
        let c_next = stm_next.next in
        if c_next = '=' then
          { stream = read_char stm_next; token = Some TEquals }
        else { stream = read_char stm; token = Some TAssign }
    | '(' -> { stream = read_char stm; token = Some TLeftParen }
    | ')' -> { stream = read_char stm; token = Some TRightParen }
    | '+' -> { stream = read_char stm; token = Some TPlus }
    | '-' -> { stream = read_char stm; token = Some TMinus }
    | '*' -> { stream = read_char stm; token = Some TAsterisk }
    | '/' -> { stream = read_char stm; token = Some TSlash }
    (* TODO: find out how to compile with eof instead of '\000'
       couldn't find any way to create a const or named literals *)
    | '\000' -> { stream = stm; token = None }
    | _ -> syntax_error stm "couldn't identify the token"

let scan_all scanner =
  let rec insert_next scanner acc =
    let scanner_next = scan_token scanner in
    match scanner_next.token with
    | None -> acc
    | Some t -> t :: insert_next scanner_next acc
  in
  match insert_next scanner [] with
  | [] -> { before = []; next = None; after = [] }
  | head :: tail -> { before = []; next = Some head; after = tail }

let scan_string str = scan_all { stream = stream_of_string str; token = None }

(*** by Token list ***)

let read_token tokens =
  match tokens.next with
  | None -> tokens
  | Some t -> (
      match tokens.after with
      | [] -> { before = t :: tokens.before; next = None; after = [] }
      | head :: tail ->
          { before = t :: tokens.before; next = Some head; after = tail })

let unread_token tokens =
  match tokens.next with
  | None -> tokens
  | Some t -> (
      match tokens.before with
      | [] -> { before = []; next = None; after = t :: tokens.after }
      | head :: tail ->
          { before = tail; next = Some head; after = t :: tokens.after })

(* read token at n positions ahead
   i.e. if tokens are
   {before = []; next = Some (Identifier "x"); after = [Plus; Literal 1]}
   then match_at 0 matches Identifier "x",
        match_at 2 matches Literal 1.
*)
let rec read_token_at n tokens =
  if n > 0 then
    let tokens_next = read_token tokens in
    read_token_at (n - 1) tokens_next
  else if n < 0 then
    let tokens_prev = unread_token tokens in
    read_token_at (n + 1) tokens_prev
  else tokens

(*** Tests ***)

let blank = stream_of_string "   non_blank   "
let blank_skipped = skip_blank_chars blank
let program = stream_of_string "x + 1"
let s = { stream = program; token = None }
