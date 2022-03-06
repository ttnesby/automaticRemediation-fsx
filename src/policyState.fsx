#load @"./../.paket/load/Flurl.Http.fsx"
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

type State = {
    scopeName: string
    scopeId: string
    assignmentId: string
    assignmentType: AssignmentType
}
with
    override s.ToString () =
        $"\n{s.scopeName} ({s.scopeId.Split('/') |> Array.last})\n{s.assignmentId.Split('/') |> Array.last}" +
        $"\n{s.assignmentType}"

type Get = OAuth.Token -> Scope.Entity list -> State list
type Report = string -> State list -> unit

module State =

    let create (name, sId, aId, aType) =
        {State.scopeName = name; scopeId = sId; assignmentId = aId; assignmentType = aType }

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

        type InitQP = {|``api-version``: string; ``$filter``:string; ``$select``: string|}
        type RecQP = {|``api-version``: string|}

        type QParams =
        | Init of qp: InitQP
        | Rec of qp: RecQP

        let get (t: OAuth.Token) (filter: string) (url: string) =
            let eMsg e = $"Failure during policy state request - [{e}]"
            let rec loop (aUrl: string, values: RState list) =
                match values with
                | [] ->
                    aUrl.SetQueryParams( {|``api-version`` = "2019-10-01";``$filter`` = filter;``$select`` = select|} )
                | _ -> aUrl |> Url
                |> fun (s: Url) ->
                    s.WithOAuthBearerToken(t.access_token).WithTimeout(5).PostStringAsync("").ReceiveJson<OData>()
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
                if (rs.policyDefinitionReferenceId <> "")
                then rs.policyDefinitionReferenceId |> Some
                else None

            rss |> List.map mayBeRefId |> List.choose id |> List.distinct

        let toState (e: Scope.Entity) (aId: string, rss: RestAPI.RState list ) =
            let refIds = rss |> uniqueRefIds
            if (refIds |> List.isEmpty)
            then (e.displayName, e.id, aId, AssignmentType.Policy) |> create
            else (e.displayName, e.id, aId, refIds |> AssignmentType.PolicySet) |> create

        let toStates (e: Scope.Entity) (rssg: (string*RestAPI.RState list) list) = rssg |> List.map (e |> toState)

    let get : Get = fun t -> fun scopes ->
        scopes
        |> List.map (fun e ->
            RestAPI.get t (e.id |> RestAPI.filter) (e.name.ToString() |> RestAPI.url)
            |> Result.map Mapping.policyStatesByAssignmentId
            |> Result.map (e |> Mapping.toStates)
        )
        |> List.map (fun r -> r |> function | Ok s -> Some s | Error _ -> None)
        |> List.choose id
        |> List.collect id

    let report : Report = fun title -> fun ss ->
        printfn $"{title}"
        ss |> List.iter (fun s -> printfn $"{s}")