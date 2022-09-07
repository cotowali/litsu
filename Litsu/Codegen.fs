(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2022 zakuro <z@kuro.red>. All rights reserved.        *)
(*                                                                     *)
(* This Source Code Form is subject to the terms of the Mozilla Public *)
(* License, v. 2.0. If a copy of the MPL was not distributed with this *)
(* file, You can obtain one at https://mozilla.org/MPL/2.0/.           *)
(*                                                                     *)
(***********************************************************************)

module Litsu.Codegen

open Litsu.SyntaxTree
open Litsu.Type

let unreachable = failwith "Unreachable"

let private genExpr (write: string -> 'a) (expr: Expr) : unit =
    let rec f: Expr -> string =
        function
        | Expr.Int (n) -> sprintf "%d" n
        | Expr.Add (lhs, rhs) ->
            (match typInfix lhs rhs with
             | Type.Int -> sprintf "$(( %s + %s ))" (f lhs) (f rhs)
             | _ -> unreachable)
        | Expr.Sub (lhs, rhs) ->
            (match typInfix lhs rhs with
             | Type.Int -> sprintf "$(( %s - %s ))" (f lhs) (f rhs)
             | _ -> unreachable)
        | Expr.Eq (lhs, rhs) ->
            let op =
                match typ lhs with
                | Type.Int -> "-eq"
                | Type.Bool -> "="
                | _ -> unreachable in

            let cond = sprintf "[ \"%s\" %s \"%s\" ]" (f lhs) op (f rhs) in
            sprintf "$(%s && printf 'true' || printf 'false')" cond in

    write (sprintf "printf '%%s\\n' \"%s\"\n" (f expr)) |> ignore



let private genNode (write: string -> 'a) (node: Node) : unit =
    match node with
    | Expr (expr) -> genExpr write expr

let codegen (write: string -> 'a) (prog: Program) : unit =
    List.map (genNode write) prog.Nodes |> ignore
