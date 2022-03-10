#load @"./../.paket/load/Flurl.Http.fsx"
#load @"./configuration.fsx"
#load @"./oAuthToken.fsx"
#load @"./scope.fsx"

namespace Policy

type AssignmentType =
    | Policy
    | PolicySet of refIds: string list
with
    override du.ToString() =
        du |> function
            | AssignmentType.Policy -> "policy"
            | AssignmentType.PolicySet s -> $"policySet [{s |> Seq.length}] {s |> Seq.toList}"

type NonCompliance = {
    scopeName: string
    scopeId: string
    assignmentId: string
    assignmentType: AssignmentType
}
with
    override s.ToString () =
        $"{s.scopeName} ({s.scopeId.Split('/') |> Array.last})\n{s.assignmentId.Split('/') |> Array.last}" +
        $"\n{s.assignmentType}\n"

type Get = OAuth.Token*Configuration.Technical -> Scope.Entity list -> NonCompliance list
type Report = string -> NonCompliance list -> unit

module NonCompliance =

    let create (name, sId, aId, aType) =
        {NonCompliance.scopeName = name; scopeId = sId; assignmentId = aId; assignmentType = aType }

    module RestAPI =

        open Flurl
        open Flurl.Http
        open Newtonsoft.Json

        [<Literal>]
        let select = "PolicyAssignmentId, PolicyDefinitionReferenceId"

        type RState = {
            policyAssignmentId: string
            policyDefinitionReferenceId: string
        }

        type OData = {
            [<JsonProperty("@odata.nextLink")>]
            nextLink: string
            value: RState list
        }

        let filter mngGrpId =
            "(PolicyDefinitionAction eq 'deployifnotexists' or PolicyDefinitionAction eq 'modify') " +
            "and ComplianceState eq 'NonCompliant' " +
            $"and PolicyAssignmentScope eq '{mngGrpId}'"
        let url mngGrpName =
            $"https://management.azure.com/providers/Microsoft.Management/managementGroups/{mngGrpName}" +
            "/providers/Microsoft.PolicyInsights/policyStates/latest/queryResults"

        let get (t: OAuth.Token, tech: Configuration.Technical) (filter: string) (url: string) =
            let eMsg e = $"Failure during policy state request - [{e}]"
            let rec loop (aUrl: string, values: RState list) =
                match values with
                | [] ->
                    aUrl.SetQueryParams( {|``api-version`` = "2019-10-01";``$filter`` = filter;``$select`` = select|} )
                | _ -> aUrl |> Url
                |> fun (u: Url) ->
                    u
                        .WithOAuthBearerToken(t.access_token)
                        .WithTimeout(tech.httpTimeout)
                        .PostStringAsync("")
                        .ReceiveJson<OData>()
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> fun od ->
                    match od.nextLink with
                    | null -> values@od.value
                    | nextUrl -> loop (nextUrl, values@od.value)

            try loop (url, List.empty<RState>) |> Ok with | e -> e.Message |> eMsg |> Error

    module Mapping =

        let policyStatesByAssignmentId (rss: RestAPI.RState list) = rss |> List.groupBy (fun s -> s.policyAssignmentId)

        let uniqueRefIds (rss: RestAPI.RState list) =
            let mayBeRefId (rs: RestAPI.RState) =
                if (rs.policyDefinitionReferenceId <> "") then Some rs.policyDefinitionReferenceId else None

            rss |> List.map mayBeRefId |> List.choose id |> List.distinct

        let rStateToNonCompliance (e: Scope.Entity) (aId: string, rss: RestAPI.RState list ) =
            let refIds = uniqueRefIds rss
            if (List.isEmpty refIds)
            then create (e.displayName, e.id, aId, AssignmentType.Policy)
            else create (e.displayName, e.id, aId, AssignmentType.PolicySet refIds)

        let rStatesToNonCompliance (e: Scope.Entity) (rssg: (string*RestAPI.RState list) list) = List.map (rStateToNonCompliance e) rssg

    let scopeNonCompliance t (e: Scope.Entity) = async {
        return (
            RestAPI.get t (RestAPI.filter e.id) (RestAPI.url (e.name.ToString()))
            |> Result.map Mapping.policyStatesByAssignmentId
            |> Result.map (e |> Mapping.rStatesToNonCompliance)
        )
    }

    let get : Get = fun t -> fun scopes ->
        scopes
        |> List.map (scopeNonCompliance t) |> Async.Parallel |> Async.RunSynchronously |> Array.toList
        |> List.map (fun r -> r |> function | Ok s -> Some s | Error _ -> None)
        |> List.choose id
        |> List.collect id