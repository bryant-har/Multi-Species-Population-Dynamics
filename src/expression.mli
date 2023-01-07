(** Turns strings into AST expressions. *)

val to_expression : string -> Ast.expr
(** [to_expression s] is the abstract syntax tree (AST) representing string [s]. *)