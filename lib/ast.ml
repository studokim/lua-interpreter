(* Functions are first-order *)
type identifier = Name of string

type literal =
  | Numeric of float
  | String of string
  | Bool of bool
  | Nil

type operator =
  | AddOp
  | SubOp
  | MulOp
  | DivOp

type expression =
  | Identifier of identifier
  | Literal of literal
  | Binop of expression * operator * expression
  | Call of identifier * params

and params = expression list

type statement =
  | Comment
  | Expression of expression
  | Assignment of identifier * expression
  | Definition of identifier * args * body
  | Return of expression

and args = identifier list
and body = statement list

type chunk = Chunk of statement list
