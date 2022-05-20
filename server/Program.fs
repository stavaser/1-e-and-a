open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Cors.Infrastructure
open Giraffe
open Giraffe.GiraffeViewEngine
open FSharp.Control.Tasks
open FParsec
open lang.ProjectHandler

[<CLIMutable>]
type AbcModel = 
    {
        Text: string
    }
    override this.ToString() =
        sprintf "%s" this.Text

let indexView =
    html [] [
        head [] [
            title [] [ str "Giraffe Example" ]
        ]
        body [] [
            h1 [] [ str "I |> F#" ]
            p [ _class "some-css-class"; _id "someId" ] [
                str "Hello World"
            ]
        ]
    ]

// let OneEaToABCHandler (ABC:string) =
//     fun (next : HttpFunc) (ctx : HttpContext) ->
//         task {
//             let output = parseAndEval ABC
//             let msg = sprintf "%s" output
//             return! json { Response = msg } next ctx
//         }
let OneEaToABCHandler : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! abc = ctx.BindJsonAsync<AbcModel>()
            let evaluated = parseAndEval (abc.ToString())
            let msg = sprintf "%s" evaluated
            // let my_json = json { Response = msg }
            return! json { Text = msg } next ctx
            // return! json { Response = "hello world!" } next ctx
        }

let webApp =
    choose [
        GET >=> choose [
            route "/" >=> htmlView indexView
            // subRoute "/api"
            //     (choose [
            //             route "" >=> json { Response = "Hello world!!" }
            //             routef "/1eaToAbc/%s" OneEaToABCHandler
            //     ])
        ]
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