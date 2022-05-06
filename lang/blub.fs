module Blub

open BlubAST
open BlubParser
open System

(* Represents a variable environment *)
type Env = Map<string, Expr>

(* prettyprint
 *   Prints out Blub expressions all pretty-like.
 *   The moral equivalent of toString in Java.
 *)
let rec prettyprint (e: Expr) : string =
    match e with
    | Num n -> string n
    | EString s -> s
    | Variable v -> "Variable(" + v + ")"
    | Assignment (e1, e2) ->
        "Assignment("
        + (prettyprint e1)
        + ", "
        + (prettyprint e2)
        + ")"
    | Plus (e1, e2) ->
        "Plus("
        + (prettyprint e1)
        + ", "
        + (prettyprint e2)
        + ")"
    | Print e -> "Print(" + (prettyprint e) + ")"
    | Sequence es ->
        let pe = es |> List.map prettyprint
        "Sequence(" + String.Join(",", pe) + ")"

(* eval
 *   The Blub interpreter!
 *   Always takes in an expression and an environment, and
 *   always returns an expression and an updated environment.
 *
 *   Blub does not statically check data types; it does
 *   perform dynamic checks in some places.
 *   However, Blub is "closed" under all Blub operations,
 *   meaning that a Blub expression will never return
 *   something that isn't a Blub expression.
 *)
let rec eval (e: Expr) (env: Env) : Expr * Env =
    match e with
    (* Number literals return themselves *)
    | Num n -> Num n, env
    (* String literals return themselves *)
    | EString s -> EString s, env
    (* Evaluating a variable returns its value in
     * the environment. Fails if the variable is not
     * in the environment.
     *)
    | Variable v ->
        if env.ContainsKey v then
            let value = env.Item v
            value, env
        else
            failwith ("Undefined variable '" + v + "'")
    (* Assignment evaluates the right hand side, then
     * stores that evaluated result in the variable
     * indicated by the left hand side.  Fails if the
     * left hand side is not a variable.
     *)
    | Assignment (e1, e2) ->
        match e1 with
        | Variable v ->
            let e2', env' = eval e2 env
            let env'' = env'.Add(v, e2')
            e2', env''
        | _ -> failwith "Left side of assignment must be a variable."
    (* Returns the sum of two numbers.
     * Fails if the left hand and right hand sides do
     * not evaluate to numbers.
     *)
    | Plus (e1, e2) ->
        let e1', env' = eval e1 env
        let e2', env'' = eval e2 env'

        match e1', e2' with
        | Num n1, Num n2 -> Num(n1 + n2), env''
        | _ -> failwith "Addition operands must evaluate to numeric type."
    (* Evaluates a value, prints it to the console, and
     * then returns it.  Printing uses Blub's built-in
     * pretty printer because people are shallow and
     * apparently looks are all that matters.
     *)
    | Print e ->
        let e', env' = eval e env
        printfn "%s" (prettyprint e')
        e', env'
    (* Evaluates a sequence of values, returning
     * the last one in the sequence.
     *)
    | Sequence es ->
        match es with
        | [] -> failwith "Empty sequence not allowed."
        | [ e ] -> eval e env
        | e :: es' ->
            let e', env' = eval e env
            let s = Sequence es'
            eval s env'
