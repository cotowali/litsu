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

open FSharp.Compatibility.OCaml.Pervasives
open Litsu.SyntaxTree
open Litsu.Type

let unreachable () = failwith "Unreachable"

type Context = { IndentN: int; Counter: int ref }

let private newContext () : Context = { IndentN = 0; Counter = ref 0 }

let private newInnerContext (ctx: Context) : Context = { ctx with IndentN = ctx.IndentN + 1 }

let private varname (name: string) : string = sprintf "LITSU_%s" name

let private sTrue, sFalse = ("true", "false")

let private indentStr (n: int) : string = String.replicate n "  "
let private ctxIndentStr (ctx: Context) : string = indentStr ctx.IndentN

type writeF = string -> unit

let rec private genExpr (ctx: Context) (write: writeF) (expr: Expr) : unit =
    let filterArgs = List.filter (fun e -> typ e <> Type.Unit)

    let rec funcCall (wo: writeF) (fe: Expr) (args: Expr list) =
        sprintf
            "%s %s"
            (f wo fe)
            (String.concat " " (List.map (fun e -> sprintf "\"%s\"" (f wo e)) (filterArgs args)))

    and f (writeOuter: string -> unit) (expr: Expr) : string =
        let wo = writeOuter

        match expr with
        | Expr.Int (n) -> sprintf "%d" n
        | Expr.String (s) ->
            String.collect
                (function
                | '"' -> "\\\""
                | '\\' -> "\\\\"
                | '$' -> "\\$"
                | c -> sprintf "%c" c)
                s
        | Expr.Unit -> ""
        | Expr.Infix (e) ->
            (match e.Op with
             | "+"
             | "-" ->
                 (match e.Type with
                  | Type.Int -> sprintf "$(( ( %s ) %s ( %s ) ))" (f wo e.Left) e.Op (f wo e.Right)
                  | _ -> unreachable ())
             | "="
             | "<>"
             | "<"
             | ">"
             | "<="
             | ">=" ->
                 let op =
                     (match e.Op with
                      | "=" ->
                          (match typ e.Left with
                           | Type.Int -> "-eq"
                           | _ -> "=")
                      | "<>" ->
                          (match typ e.Left with
                           | Type.Int -> "-ne"
                           | _ -> "!=")
                      | "<" -> "-lt"
                      | "<=" -> "-le"
                      | ">" -> "-gt"
                      | ">=" -> "-ge"
                      | _ -> unreachable ())

                 let cond = sprintf "[ \"%s\" %s \"%s\" ]" (f wo e.Left) op (f wo e.Right) in
                 sprintf "$(%s && printf '%s' || printf '%s')" cond sTrue sFalse
             | _ -> unreachable ())
        | Expr.If (e) ->
            let mutable out = sprintf "$(if [ \"%s\" = '%s' ]\n" (f wo e.Cond) sTrue
            let writeInner s = out <- out + s
            let writeOuter s = out <- s + out
            writeInner (ctxIndentStr ctx)
            writeInner "then\n"
            genExpr { ctx with IndentN = ctx.IndentN + 1 } writeInner e.Expr1
            writeInner (ctxIndentStr ctx)
            writeInner "else\n"
            genExpr { ctx with IndentN = ctx.IndentN + 1 } writeInner e.Expr2
            writeInner (ctxIndentStr ctx)
            writeInner "fi)"
            out
        | Expr.Let (e) ->
            let mutable out = "$("
            let writeInner s = out <- out + s
            let writeOuter s = out <- s + out
            let origCtx = ctx
            let ctx = newInnerContext origCtx

            if List.length e.Args > 0 then
                let fname = varname e.Name
                writeInner (sprintf "%s() {\n" fname)

                (fun () ->
                    let ctx = newInnerContext ctx in

                    List.iteri
                        (fun i v ->
                            writeInner (
                                sprintf "%s%s=$%d\n" (ctxIndentStr ctx) (varname v.Name) (i + 1)
                            ))
                        (List.filter (fun a -> a.Type <> Type.Unit) e.Args)

                    genExpr ctx writeInner e.Expr1)
                    ()

                writeInner (sprintf "%s}\n" (ctxIndentStr ctx))
                // This allows calling function with "${name} args"
                writeInner (sprintf "%s%s=%s\n" (ctxIndentStr ctx) fname fname)
            else
                writeInner (sprintf "%s=\"%s\"\n" (varname e.Name) (f wo e.Expr1))

            genExpr ctx writeInner e.Expr2

            writeInner (ctxIndentStr origCtx)
            writeInner ")"
            out
        | Expr.App ({ Fun = fe
                      Args = args
                      Type = Type.Fun (targs, _) }) ->
            let fname =
                incr ctx.Counter
                sprintf "__anon_func_%d" ctx.Counter.contents

            match typ fe with
            | Type.Fun (ftargs, rt) ->
                writeOuter (
                    (sprintf "%s%s() %s \"$@\"\n" (ctxIndentStr ctx) fname (funcCall wo fe args))
                    + (sprintf "%s%s=%s;\n" (ctxIndentStr ctx) fname fname)
                )

                sprintf "%s" fname
            | _ -> unreachable ()
        // partial app
        //sprintf "%s() { %s; %s }" fname (String.concat ";"
        | Expr.App (e) -> sprintf "$(%s)" (funcCall wo e.Fun e.Args)
        | Expr.Var (e) -> sprintf "${%s}" (varname e.Name)

    let mutable out = ""
    let writeInner s = out <- out + s
    let writeOuter s = out <- s + out
    writeInner (ctxIndentStr ctx)

    writeInner (
        sprintf
            "printf '%%s%s' \"%s\"\n"
            (match typ expr with
             | Type.Unit as t
             | Fun (_, t) when t = Type.Unit -> ""
             | _ -> "\\n")
            (f writeOuter expr)
    )

    write out

let private genNode (ctx: Context) (write: string -> unit) (node: Node) : unit =
    match node with
    | Expr (expr) -> genExpr ctx write expr

let codegen (write: string -> unit) (prog: Program) : unit =
    List.map (genNode (newContext ()) write) prog.Nodes |> ignore
