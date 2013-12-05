open System         
open Owin
open Microsoft.Owin
open Microsoft.Owin.Hosting
open Microsoft.FSharp.Quotations
open FSharpx

open Skynerd.Server

module Owin =
    open FSharpx.State
    type OwinBuilder() =
        member this.Return(a) : State<'T,'State> = fun s -> (a,s)
        member this.ReturnFrom(m:State<'T,'State>) = m
        member this.Bind(m:State<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:State<'T,'State>, h:exn -> State<'T,'State>) : State<'T,'State> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:State<'T,'State>, compensation) : State<'T,'State> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
        [<CustomOperation("get", MaintainsVariableSpaceUsingBind=true)>]
        member this.Get(m, url, processor) =
            this.Bind(m, fun a ->
                this.Bind(getState, fun (app:IAppBuilder) ->
                    putState <| app.UseRequestProcessor("GET", url, processor)))
    let owin = new OwinBuilder()

[<ReflectedDefinition>]
let cheese () =
    "Hello World!"

open Owin
let pipeline = 
    owin {   
        get "/" <@ fun () -> "Hello World!" @> 
        get "/talk" <@ fun name -> sprintf "Hello %s!" name @>
        get "/cheese" <@ cheese @>
    }

let startServer (app:IAppBuilder) =
    let result = State.exec pipeline app
    ()

[<EntryPoint>]
let main argv =
    WebApp.Start("http://localhost:5000", startServer) |> ignore
    Console.ReadLine() |> ignore
    0
