#!/usr/bin/env -S dotnet fsi
#load @"./../.paket/load/Flurl.Http.fsx"
#load @"parameters.fsx"
#load @"oAuthToken.fsx"
#load @"scope.fsx"
#load @"policyState.fsx"
#load @"remediationJob.fsx"

let reportError err = printfn $"{err}"
let reportResult = function | Ok _ -> printfn "\nOk - exit code 0"; 0 | Error _ -> printfn "\nError - exit code (1)"; 1
let doOAuth = (("https://management.azure.com/", 5) |> OAuth.Token.get) >> Async.RunSynchronously
let asTitle (title, no) = $"\n---------- %s{title} [%d{no}] ----------"
let reportAzureScopes s = s |> Scope.Entity.report (("Azure scopes", s |> Seq.length) |> asTitle); s
let reportFilteredScopes s = s |> Scope.Entity.report (("Filtered scopes", s |> Seq.length) |> asTitle); s
let reportNonCompliance s = s |> Policy.State.report (("Scope & Non-compliant policies", s |> Seq.length) |> asTitle); s
let reportJobSpec s = s |> Remediation.JobSpec.report (("Remediation specifications", s |> Seq.length) |> asTitle); s

fsi.CommandLineArgs
|> Parameters.Context.fromCmdLine
|> Result.bind doOAuth
|> Result.bind (fun oAuthToken ->
        oAuthToken
        |> Scope.Entity.get
        |> Result.map reportAzureScopes
        |> Result.map (Scope.Type.ManagementGroup |> Scope.Entity.filterByType)
        |> Result.map (".*" |> Scope.Entity.filterByDisplayName )
        |> Result.map reportFilteredScopes
        |> Result.map (Policy.State.get oAuthToken)
        |> Result.map reportNonCompliance
        |> Result.map Remediation.JobSpec.policyStatesToJobSpecs
        |> Result.map reportJobSpec
    )
|> Result.mapError reportError
|> reportResult
|> exit