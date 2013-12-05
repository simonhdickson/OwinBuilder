namespace Skynerd
open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading.Tasks

open FSharpx.Linq.QuotationEvaluation
open Microsoft.FSharp.Quotations             
open Microsoft.FSharp.Quotations.Patterns 
open Microsoft.FSharp.Quotations.DerivedPatterns 
open Newtonsoft.Json       
open Owin

module Server =
    type ContentType =
        | PlainText
    
    let requestTarget (enviroment:IDictionary<string,obj>) =
        (enviroment.["owin.RequestMethod"] :?> string, enviroment.["owin.RequestPath"] :?> string)

    let queryString (enviroment:IDictionary<string,obj>) =
        let parameters =
            enviroment.["owin.RequestQueryString"] :?> string
            |> fun queryString -> queryString.Split '&'
            |> Array.toList
        match parameters with
        | "" :: _-> Seq.empty
        | _ -> parameters |> Seq.map (fun x -> ((x.Split '=').[0], (x.Split '=').[1]))

    let headers (enviroment:IDictionary<string,obj>) =
        enviroment.["owin.ResponseHeaders"] :?> IDictionary<string, string[]>

    let requestBody (enviroment:IDictionary<string,obj>)=
        (new StreamReader (enviroment.["owin.RequestBody"] :?> Stream)).ReadToEnd()

    let responseBody (enviroment:IDictionary<string,obj>) =
        enviroment.["owin.ResponseBody"] :?> Stream

    let addResponseHeader (contentLength:int) contentType enviroment =
        let responseHeaders = headers enviroment
        responseHeaders.Add("Content-Length", [|contentLength.ToString()|])
        match contentType with
        | PlainText -> responseHeaders.Add("Content-Type", [|"text/plain"|])

    let nextTask enviroment next =
        Task.Run (fun () -> next enviroment)

    let respondWith (response:string) enviroment =
        let responseBytes = ASCIIEncoding.UTF8.GetBytes response
        let responseStream = responseBody enviroment
        addResponseHeader responseBytes.Length PlainText enviroment
        responseStream.AsyncWrite(responseBytes, 0, responseBytes.Length) |> Async.StartAsTask :> Task

    let rec parseParameters expression queryParameters =
        seq {
            match expression with
                | Lambda (x, expr) when typeof<unit> = x.Type -> ()
                | Lambda (x, expr) ->
                    let value =
                        queryParameters
                        |> Seq.where (fun (i,_) -> x.Name = i)
                        |> Seq.map (fun (x,i) -> i)
                        |> Seq.head
                    yield (value, x.Type)
                    yield! parseParameters expr queryParameters  
                | _ -> ()
        }

    let derserialize (value, valueType:Type) =
        match valueType with
        | t when typeof<string> = t -> value :> obj
        | _ -> JsonConvert.DeserializeObject(value, valueType)

    let invokeFunction (instance:Expr) parameters =
        let instance = instance.EvalUntyped()
        match Array.length parameters with
        | 0 -> instance.GetType().GetMethods().[0].Invoke(instance, [|()|])
        | _ -> instance.GetType().GetMethods().[0].Invoke(instance, parameters)

    let (|>) a f = f a

    let useRequestProcessor verb url (processor:Expr<'a->'b>) next enviroment =
        let target = requestTarget enviroment
        match target with
        | (verb', url') when verb = verb' && url = url' ->
            let response =
                queryString enviroment
                |> parseParameters processor
                |> Seq.map derserialize
                |> Seq.toArray
                |> invokeFunction processor
            respondWith (response.ToString()) enviroment
        | _ ->
            nextTask enviroment next

    let Func2 (x:Func<_,_>) y = x.Invoke(y)

    type IAppBuilder with
        member x.UseRequestProcessor(verb:string, url:string, processor) =
            x.Use(fun next -> useRequestProcessor verb url processor (Func2 next))