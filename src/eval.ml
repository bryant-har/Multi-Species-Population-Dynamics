include Ast

(***)
(***)
(* ACTIVE CONCERNS - _ frequently used as a catch all. Rigorously test that all
   functions are accounted for Add any cases that are not accounted for

   -Consider converting to unary ops like with binary ops (Repeated code) *)
(***)
(***)

(***)

let convert (e : Ast.expr) : expr = e
let convert_back (e : expr) : Ast.expr = e

let rec fact acc i =
  if i < 0 then failwith "Negative Argument"
  else if i = 0 then acc
  else fact (i * acc) (i - 1)

let sub e =
  let rec subIn assoc e =
    match e with
    | Let (var, e1, e2) -> subIn ((var, subIn assoc e1) :: assoc) e2
    | Float e -> Float e
    | Var id ->
        if List.exists (fun (v, _) -> v = id) assoc then
          assoc |> List.find (fun (v, _) -> v = id) |> snd
        else Var id
    | Binop (bop, l, r) -> Binop (bop, subIn assoc l, subIn assoc r)
    (*handle unary ops*)
    | Deriv e -> Deriv (subIn assoc e)
    | Sin e -> Sin (subIn assoc e)
    | Cos e -> Cos (subIn assoc e)
    | Fact e -> Fact (subIn assoc e)
    | Tan e -> Tan (subIn assoc e)
  in
  subIn [] e

let forceSub (var, value) e = sub (Let (var, value, e))

(**[eval e] evaluates a single operation. Precondition: e is a unary or binary
   operator with float operands returns Float result expr -> expr Throws excp if
   invalid op or floats not passed in *)

let eval e =
  let op, e1, e2 =
    match e with
    | Binop (op, Float e1, Float e2) -> (op, e1, e2)
    | Sin (Float e1) -> (Add, 0., sin e1)
    | Cos (Float e1) -> (Add, 0., cos e1)
    | Tan (Float e1) -> (Add, 0., tan e1)
    | Float e1 -> (Add, 0., e1)
    | Fact (Float e1) -> (Add, 0., float (fact 1 (int_of_float e1)))
    | _ -> failwith "unknown operator or operand(s) not float"
  in
  Float
    (try
       match op with
       | Add -> e1 +. e2
       | Mult -> e1 *. e2
       | Div -> e1 /. e2
       | Pow -> e1 ** e2
       | LogBase -> log e2 /. log e1
     with _ -> failwith "Incompatible operands (to implement)")

let rec fullEval e =
  let e = sub e in
  try eval e
  with _ -> (
    match e with
    | Float e1 -> Float e1
    | Binop (op, e1, e2) -> fullEval (Binop (op, fullEval e1, fullEval e2))
    | Sin e1 -> fullEval (Sin (fullEval e1))
    | Cos e1 -> fullEval (Cos (fullEval e1))
    | Tan e1 -> fullEval (Tan (fullEval e1))
    | Fact e1 -> fullEval (Fact (fullEval e1))
    | _ -> failwith "incompatible argument")

let regenerateEuler (x, y) step alist e =
  let rec regenerateEuler2 (x, y) step alist e =
    match alist with
    | [] -> fullEval e
    | h :: t -> regenerateEuler2 (x, y) step t (forceSub h e)
  in
  match regenerateEuler2 (x, y) step alist e with
  | Float slope -> (x +. step, y +. (slope *. step))
  | _ -> failwith "??? i have no clue how this fails to match"
