(* Functions are first-order *)
type identifier = Name of string

type literal =
  | Numeric of float
  | String of string

type operator =
  | AddOp
  | SubOp
  | MulOp
  | DivOp

type expression =
  | Identifier of identifier
  | Literal of literal
  | Binop of expression * operator * expression
  | CallExpr of identifier * expression list

type statement =
  | Assignment of identifier * expression
  | CallStmt of expression
  | Definition of identifier * func

and func = identifier list * chunk
and chunk = Chunk of statement list
