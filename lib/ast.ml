(* Functions are first-order *)
type identifier = Name of string
type literal = Numeric of float

type operator =
  | AddOp
  | SubOp
  | MulOp
  | DivOp

type expression =
  | Identifier of identifier
  | Literal of literal
  | Binop of expression * operator * expression

type statement =
  | Expression of expression
  | Assignment of identifier * expression
  | Call of identifier * expression list

type chunk = Chunk of statement list
