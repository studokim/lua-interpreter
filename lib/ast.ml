type identifier =
| Name of string

type literal =
| Numeric of int

type operator = string

type expression =
| Identifier of identifier
| Literal of literal
| Binary of expression * operator * expression

type statement =
| Assignment of identifier * expression
| Call of identifier * expression list

type chunk =
| Chunk of statement list

let hello =
        print_endline "Hello world"
