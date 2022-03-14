#load @"./../.paket/load/Flurl.Http.fsx"
#load @"./configuration.fsx"
#load @"./oAuthToken.fsx"

namespace Scope

type Entity = {
    id: string
    eType: Configuration.ScopeFilterType
    name: System.Guid
    displayName: string
}
with
    override e.ToString() = $"%-30s{e.displayName}| %-20s{e.eType.ToString()}| %-25A{e.name}"

type Get = OAuth.Token*Configuration.Technical*Configuration.AppSetting -> Result<Entity list, string>
type FilterByType = Configuration.ScopeFilterType -> Entity list -> Entity list
type FilterByDisplayName = string -> Entity list -> Entity list

type Report = string -> Entity list -> unit

[<RequireQualifiedAccess>]
module Entity =

    open System.Text.RegularExpressions

    let create (id, eType, name, displayName) =
        { Entity.id = id; eType = eType; name = name; displayName = displayName }

    module RestAPI =

        open Flurl
        open Flurl.Http

        type MngGrpChild = {
        id: string
        ``type``: string
        name: string
        displayName: string
        children: array<MngGrpChild> //option
        }

        type MngGrpProps = {
            tenantId: string
            displayName: string
            children: array<MngGrpChild> //option
        }

        type MngGroup ={
            id: string
            ``type``: string
            name: string
            properties: MngGrpProps
        }

        let get (token: OAuth.Token, tech: Configuration.Technical, app: Configuration.AppSetting) =
            let eMsg e = $"Failure during mng grp request - [{e}]"
            let tryGet =
                try
                    "https://management.azure.com/providers/Microsoft.Management/managementGroups"
                        .AppendPathSegment(app.startMngGrp.ToString())
                        .SetQueryParams( {|
                            ``api-version`` = "2020-05-01"
                            expand = "children"
                            recurse = "true"
                        |} )
                        .WithOAuthBearerToken(token.access_token)
                        .WithTimeout(tech.httpTimeout)
                        .GetJsonAsync<MngGroup>()
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                    |> Ok
                with | e -> e.Message |> eMsg |> Error
            async { return tryGet }

    module Mapping =

        let toScopeType = function
                | "Microsoft.Management/managementGroups" -> Configuration.ScopeFilterType.ManagementGroup
                | "/subscriptions" -> Configuration.ScopeFilterType.Subscription
                | _ -> Configuration.ScopeFilterType.Unknown

        let toEntity (mg: RestAPI.MngGroup) =
            let mgToTuple (mg: RestAPI.MngGroup) =
                (mg.id, mg.``type`` |> toScopeType, mg.name |> System.Guid.Parse, mg.properties.displayName)

            let chToTuple (ch: RestAPI.MngGrpChild) =
                (ch.id, ch.``type`` |> toScopeType, ch.name |> System.Guid.Parse, ch.displayName)

            let rec loop (siblings: RestAPI.MngGrpChild list, scopes: Entity list) =
                let toEntity = chToTuple >> create
                match siblings with
                | [] -> scopes
                | [h] -> h.children
                        |> function
                            | null -> scopes@[h |> toEntity]
                            | ca -> loop(ca |> Array.toList, scopes@[h |> toEntity])
                | h::t -> h.children
                        |> function
                            | null -> loop(t, scopes@[h |> toEntity])
                            | ca -> loop((ca |> Array.toList)@t, scopes@[h |> toEntity])

            let root = mg |> mgToTuple |> create

            mg.properties.children
            |> function | null -> [root] | ca -> loop(ca |> Array.toList, [root])

    let get : Get = fun t -> t |> (RestAPI.get >> Async.RunSynchronously >> Result.map Mapping.toEntity)
    let filterByType: FilterByType = fun t -> fun es -> es |> List.filter (fun e -> e.eType = t)
    let filterByDisplayName: FilterByDisplayName = fun pattern -> fun es ->
        es |> List.filter (fun e -> Regex.Match(e.displayName, @$"{pattern}", RegexOptions.Compiled).Success )