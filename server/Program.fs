open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Cors.Infrastructure
open Giraffe
open Giraffe.GiraffeViewEngine
open FSharp.Control.Tasks
open lang.ProjectInterpreter
open lang.ProjectParser
open FParsec


[<CLIMutable>]
type AbcModel = 
    {
        Text: string
    }

type ParseState = 
    | ParseSuccess of string
    | ParseFailure of string

let OneEaToABCHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! abc = ctx.BindJsonAsync<AbcModel>()
            let abc_text = abc.Text
            
            let parsed_str = run grammar abc_text

            match parsed_str with
            | Success (result, _, _) ->
                let output =
                    try
                        eval result
                    with
                    | RuntimeError (message) -> message
                return! json output next ctx
            | Failure (errorMsg, _, _) -> return! (setStatusCode 400 >=> json errorMsg) next ctx

        }

let webApp =
    choose [
        POST >=> choose [
                route "/api/1eaToAbc/" >=> OneEaToABCHandler
        ]
        setStatusCode 404 >=> text "Not Found"
    ]

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:3000").AllowAnyMethod().AllowAnyHeader() |> ignore

let configureApp (app : IApplicationBuilder) =
    app.UseCors configureCors |> ignore
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore
        
[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHost ->
            webHost
                .Configure(configureApp)
                .ConfigureServices(configureServices)
                |> ignore)
        .Build()
        .Run()
    0