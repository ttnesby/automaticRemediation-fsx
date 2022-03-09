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

type YamlConfig = {
    tenantId: string
    clientId: string
    clientSecret: string
    startMngGrp: string
    scopeFilter: ScopeFilter
    scopeDisplayNameFilter: string
    assignmentFilter: AssignmentFilter option
    httpTimeout: int
}

type FilePath = string
type Get = FilePath -> Result<YamlConfig,string>

module YamlConfig =

    let private read (filePath: string) =
            let eMsg e = $"Failure reading {filePath} - {e}"
            try filePath |> System.IO.File.ReadAllText |> Ok with | e -> e.Message |> eMsg |> Error

    let private firstDoc =
        function
            | [h] ->
                match h with
                    | Legivel.Serialization.DeserializeResult.Success d -> Ok d.Data
                    | Legivel.Serialization.DeserializeResult.Error e -> Error $"%A{e}"
            | _ -> Error "Failure in yaml parsing, none or too many yaml documents found"

    let private deserialize s =
        try Legivel.Serialization.Deserialize<YamlConfig> s |> Ok with | e -> Error e.Message

    let get : Get = fun fp -> fp |> (read >> Result.bind deserialize >> Result.bind firstDoc)

"./src/configuration.navutv.yml" |> YamlConfig.get