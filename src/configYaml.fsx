#load @"./../.paket/load/Legivel.fsx"

open Legivel.Attributes

type ScopeFilter =
    | [<YamlValue("managementGroup")>] ManagementGroup
    | [<YamlValue("subscription")>] Subscription

type AssignmentFilterType =
    | [<YamlValue("include")>] Include
    | [<YamlValue("exclude")>] Exclude

type AssignmentFilter = {
    values: string list
    ``type``: AssignmentFilterType
}

type AllConfig = {
    tenantId: string
    clientId: string
    clientSecret: string
    startMngGrp: string
    scopeFilter: ScopeFilter
    scopeDisplayNameFilter: string
    assignmentFilter: AssignmentFilter option
    httpTimeout: int
}

let read (filePath: string) =
        let eMsg e = $"failure reading {filePath} - {e}"
        try filePath |> System.IO.File.ReadAllText |> Ok with | e -> e.Message |> eMsg |> Error

let extractYamlData (r: Legivel.Serialization.DeserializeResult<AllConfig> list) =
    match r with
    // expecting only 1 yaml doc
    | [h:_] when List.length r = 1 ->
        match h with
        | Legivel.Serialization.DeserializeResult.Success d -> Ok d.Data
        | Legivel.Serialization.DeserializeResult.Error e -> Error $"%A{e}"
    | _ ->
        "Yaml parsing - Either none or too many yaml documents found"
        |> Error

"./src/configuration.navutv.yml"
|> read
|> Result.bind (fun ys -> try Legivel.Serialization.Deserialize<AllConfig> ys |> Ok with | e -> Error e.Message)
|> Result.bind extractYamlData