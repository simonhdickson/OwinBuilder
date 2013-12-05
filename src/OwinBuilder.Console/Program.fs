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
        member this.Bind(m:State<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        [<CustomOperation("get", MaintainsVariableSpaceUsingBind=true)>]
        member this.Get(m, url, processor) =
            this.Bind(m, fun a ->
                this.Bind(getState, fun (app:IAppBuilder) ->
                    putState <| app.UseRequestProcessor("GET", url, processor)))
    let owin = new OwinBuilder()

[<ReflectedDefinition>]
let helloWorld () =
    "Hello World!"

open Owin
let pipeline = 
    owin {   
        get "/" <@ fun () -> "Hello World!" @> 
        get "/talk" <@ fun name -> sprintf "Hello %s!" name @>
        get "/cheese" <@ helloWorld @>
    }

let startServer (app:IAppBuilder) =
    let result = State.exec pipeline app
    ()

[<EntryPoint>]
let main argv =
    WebApp.Start("http://localhost:5000", startServer) |> ignore
    Console.ReadLine() |> ignore
    0
