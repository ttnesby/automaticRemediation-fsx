#!/usr/bin/env -S dotnet fsi
#load @"./../.paket/load/Flurl.Http.fsx"
#load @"config.fsx"
#load @"oAuthToken.fsx"
#load @"scope.fsx"
#load @"policyState.fsx"
#load @"remediationJob.fsx"

let report title el =
    let header noOf = printfn $"\n---------- %s{title} [%i{noOf}] ----------\n";
    (el, el |> List.length) |> fun (l, noOf) ->  header noOf; l |> List.iter (fun e -> printfn $"{e}")
    el
let reportError err = printfn $"{err}"
let reportResult = function | Ok _ -> printfn "\nOk - exit code 0"; 0 | Error _ -> printfn "\nError - exit code (1)"; 1

fsi.CommandLineArgs
|> Configuration.Config.init
|> Result.bind (fun cfg ->
    cfg.ctx
    |> (OAuth.Token.get ("https://management.azure.com/", cfg.tech.httpTimeout) >> Async.RunSynchronously)
    |> Result.bind (fun oAuthToken ->
            oAuthToken
            |> Scope.Entity.get
            |> Result.map (report "Azure scopes")
            |> Result.map (Scope.Type.ManagementGroup |> Scope.Entity.filterByType)
            |> Result.map ("nav.*" |> Scope.Entity.filterByDisplayName )
            |> Result.map (report "Filtered scopes")
            |> Result.map (Policy.NonCompliance.get oAuthToken)
            |> Result.map (report "Scope & Non-compliant policies")
            |> Result.map Remediation.JobSpec.policyStatesToJobSpecs
            |> Result.map (report "Remediation specifications")
        )
    )
|> Result.mapError reportError
|> reportResult
|> exit