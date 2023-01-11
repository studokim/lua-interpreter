type identifier =
| Name of string

type literal =
| Numeric of int

type expression =
| Identifier of identifier
| Literal of literal

type statement =
| Assignment of identifier * expression
| Call of identifier * expression list

type chunk =
| Chunk of statement list

let hello =
        print_endline "Hello world"
