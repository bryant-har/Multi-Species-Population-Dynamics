(** The AST component of parsing equations from strings. *)

(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Pow
  | Div
  | LogBase

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Float of float
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | Deriv of expr
  | Sin of expr
  | Cos of expr
  | Fact of expr
  | Tan of expr
