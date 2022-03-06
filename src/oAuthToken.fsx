#load @"./../.paket/load/Flurl.Http.fsx"
#load @"./configuration.fsx"

namespace OAuth

type Token = {
    token_type: string
    expires_in: int
    ext_expires_in: int
    access_token: string
}

type ResourceValue = string

type Get = ResourceValue*Configuration.Technical -> Configuration.Context -> Async<Result<Token, string>>

[<RequireQualifiedAccess>]
module Token =

    open Flurl.Http

    let get: Get = fun (resource,tech) -> fun ctx ->
        let eMsg e = $"Failure during token request with timeout after {tech} ms - [{e}]"
        let tryGet =
            try
                $"https://login.microsoftonline.com/%s{ctx.TenantID.ToString()}/oauth2/token"
                    .WithHeaders( {|
                        ``Cache-Control`` = "no-cache"
                        ``Content-Type`` = "application/x-www-form-urlencoded"
                    |})
                    .WithTimeout(tech.httpTimeout)
                    .PostUrlEncodedAsync( {|
                        client_id = ctx.ClientID.ToString()
                        client_secret = ctx.ClientSecret
                        resource = resource
                        grant_type = "client_credentials"
                    |})
                    .ReceiveJson<Token>()
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Ok
            with | e -> e.Message |> eMsg |> Error
        async { return tryGet }