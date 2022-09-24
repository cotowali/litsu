(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.Typing

open FSharp.Compatibility.OCaml
open Litsu.Type
open Litsu.TypeEnv
open Litsu.SyntaxTree

exception UnifyException of Type * Type
exception UndefinedVariableException of string

let rec occur r1 =
    function
    | Type.Var (r2) when r1 == r2 -> true
    | Type.Var ({ contents = None }) -> false
    | Type.Var ({ contents = Some (t2) }) -> occur r1 t2
    | _ -> false

let rec unify (t1: Type) (t2: Type) =
    match t1, t2 with
    | Type.Int, Type.Int
    | Type.Bool, Type.Bool
    | Type.String, Type.String -> ()
    | Type.Unit, Type.Unit -> ()
    | Type.Fun (args1, rt1), Type.Fun (args2, rt2) ->
        (try
            List.iter2 unify args1 args2
         with Invalid_argument (_) ->
             raise (UnifyException(t1, t2)))

        unify rt1 rt2
    | Type.Var (r1), Type.Var (r2) when r1 == r2 -> ()
    | Type.Var ({ contents = Some (t1') }), _ -> unify t1' t2
    | _, Type.Var ({ contents = Some (t2') }) -> unify t1 t2'
    | Type.Var ({ contents = None } as r1), _ ->
        if occur r1 t2 then
            raise (UnifyException(t1, t2))

        r1 := Some(t2)
    | _, Type.Var ({ contents = None } as r2) ->
        if occur r2 t1 then
            raise (UnifyException(t1, t2))

        r2 := Some(t1)
    | _, _ -> raise (UnifyException(t1, t2))


let rec derefType (t: Type) : Type =
    let dt = derefType in

    match t with
    | Type.Var ({ contents = None } as r) ->
        r := Some(Type.Unknown)
        Type.Unknown
    | Type.Var ({ contents = Some (t) } as r) ->
        let t = dt t in
        r := Some(t)
        t
    | Type.Fun (targs, t) -> Type.Fun(List.map dt targs, derefType t)
    | t -> t

let rec derefExpr (expr: Expr) : Expr =
    let de = derefExpr in
    let dt = derefType in

    match expr with
    | Expr.Int (_)
    | Expr.String (_)
    | Expr.Unit as e -> e
    | Expr.Infix (op, lhs, rhs, t) -> Expr.Infix(op, de lhs, de rhs, dt t)
    | Expr.If (cond, e1, e2, t) -> Expr.If(de cond, de e1, de e2, dt t)
    | Expr.Let (name, typ, args, e1, e2) ->
        Let(name, dt typ, (List.map (fun (s, t) -> s, dt t) args), de e1, de e2)
    | Expr.App (f, args, t) -> App(de f, List.map de args, dt t)
    | Expr.Var (name, typ) -> Expr.Var(name, dt typ)

and derefNode: Node -> Node =
    function
    | Expr (expr) -> Node.Expr(derefExpr expr)

let deref (p: Program) : Program = { Nodes = List.map derefNode p.Nodes }

let rec infer (env: TypeEnv) (e: Expr) : Type =
    try
        match e with
        | Expr.Int (_) -> Type.Int
        | Expr.String (_) -> Type.String
        | Expr.Unit -> Type.Unit
        | Expr.Infix (op, lhs, rhs, t) ->
            let t1 = (infer env lhs)
            let t2 = (infer env rhs)
            unify t1 t2

            match op with
            | "=" -> unify t Type.Bool
            | "+"
            | "-" -> unify t t1
            | _ -> failwith (sprintf "Unknown operator `%s`" op)

            t
        | Expr.If (cond, e1, e2, t) ->
            unify Type.Bool (infer env cond)
            let t1 = (infer env e1) in
            let t2 = (infer env e2) in
            unify t1 t2
            unify t t1
            t
        | Expr.Let (name, t, args, e1, e2) ->
            unify
                t
                (if List.length args > 0 then
                     Type.Fun(List.map snd args, infer (TypeEnv.addList args env) e1)
                 else
                     infer env e1)

            infer (TypeEnv.add name t env) e2
        | Expr.App (f, args, t) ->
            let ft = infer env f in
            let nargs = List.length args in

            match ft with
            | (Fun (fargs, rt)
            | Type.Var ({ contents = Some (Fun (fargs, rt)) })) when List.length fargs > nargs ->
                // partial apply
                List.iter2 unify fargs[0 .. (nargs - 1)] (List.map (infer env) args)
                unify t (Fun(fargs[(nargs - 1) .. (List.length fargs)], rt))
                t
            | _ ->
                unify ft (Type.Fun(List.map (infer env) args, t))
                // t is still newType here.
                // it will be replaced by calling unify at caller of this func
                t
        | Expr.Var (name, t) ->
            if TypeEnv.exists name env then
                let t' = TypeEnv.find name env in
                unify t t'
                t'
            else
                raise (UndefinedVariableException name)
    with
    | UnifyException (t1, t2) ->
        raise (ExprException(e, Some(sprintf "mismatched type: %A and %A" t1 t2)))
    | e -> raise e

let check (p: Program) : Program =
    let env = TypeEnv []

    List.map
        (function
        | Expr (e) -> infer env e)
        p.Nodes
    |> ignore

    deref p
