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
    | Expr.Infix (e) ->
        Expr.Infix(
            { Op = e.Op
              Left = de e.Left
              Right = de e.Right
              Type = dt e.Type }
        )
    | Expr.If (e) ->
        Expr.If(
            { Cond = de e.Cond
              Expr1 = de e.Expr1
              Expr2 = de e.Expr2
              Type = dt e.Type }
        )
    | Expr.Let (e) ->
        Let(
            { Name = e.Name
              Type = dt e.Type
              Args = (List.map (fun v -> { Name = v.Name; Type = dt v.Type }) e.Args)
              Expr1 = de e.Expr1
              Expr2 = de e.Expr2 }
        )
    | Expr.App (e) ->
        Expr.App(
            { Fun = de e.Fun
              Args = List.map de e.Args
              Type = dt e.Type }
        )
    | Expr.Var (e) -> Expr.Var({ Name = e.Name; Type = dt e.Type })

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
        | Expr.Infix (e) ->
            let t1 = (infer env e.Left)
            let t2 = (infer env e.Right)
            unify t1 t2

            match e.Op with
            | "="
            | "<>"
            | "<"
            | "<="
            | ">"
            | ">=" -> unify e.Type Type.Bool
            | "+"
            | "-" -> unify e.Type t1
            | _ -> failwith (sprintf "Unknown operator `%s`" e.Op)

            e.Type
        | Expr.If (e) ->
            unify Type.Bool (infer env e.Cond)
            let t1 = (infer env e.Expr1) in
            let t2 = (infer env e.Expr2) in
            unify t1 t2
            unify e.Type t1
            e.Type
        | Expr.Let (e) ->
            unify
                e.Type
                (if List.length e.Args > 0 then
                     Type.Fun(
                         List.map (fun (a: Var) -> a.Type) e.Args,
                         infer
                             (TypeEnv.addList (List.map (fun a -> (a.Name, a.Type)) e.Args) env)
                             e.Expr1
                     )
                 else
                     infer env e.Expr1)

            infer (TypeEnv.add e.Name e.Type env) e.Expr2
        | Expr.App (e) ->
            let ft = infer env e.Fun in
            let nargs = List.length e.Args in

            match ft with
            | (Fun (fargs, rt)
            | Type.Var ({ contents = Some (Fun (fargs, rt)) })) when List.length fargs > nargs ->
                // partial apply
                List.iter2 unify fargs[0 .. (nargs - 1)] (List.map (infer env) e.Args)
                unify e.Type (Fun(fargs[(nargs) .. (List.length fargs)], rt))
                e.Type
            | _ ->
                unify ft (Type.Fun(List.map (infer env) e.Args, e.Type))
                e.Type
        | Expr.Var (e) ->
            if TypeEnv.exists e.Name env then
                unify e.Type (TypeEnv.find e.Name env)
                e.Type
            else
                raise (UndefinedVariableException e.Name)
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
