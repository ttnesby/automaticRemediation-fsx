namespace Configuration

type Context = {
    TenantID          : System.Guid
    ClientID          : System.Guid
    ClientSecret      : string
    SubscriptionID    : System.Guid option
}

type AssignmentFilterType =
    | Include
    | Exclude

type AppSetting = {
    startMngGrp: System.Guid
    scopeFilter: string
    assignmentFilter: string list option
    assignmentFiltertype: AssignmentFilterType option
}

type Technical = {
    httpTimeout: int
}

//type ConfigType =
//    | Ctx of ctx: Context
//    | App of app: AppSetting
//    | Tech of tech: Technical
type Config = {ctx: Context; app: AppSetting; tech: Technical}

type Init = string array -> Result<Config, string>

module Config =

    let private create (c,a,t) = {Config.ctx = c; app = a; tech = t}

    [<RequireQualifiedAccess>]
    module Context =

        type FromCmdLine = string array -> Result<Context,string>

        let private create (tenantID, clientID, clientSecret, subscriptionId) = {
            TenantID = tenantID
            ClientID = clientID
            ClientSecret = clientSecret
            SubscriptionID = subscriptionId
        }

        let private maybeArg i fApply (sa:string array) = if Array.length sa < i + 1 then None else Some (sa.[i] |> fApply)
        let private subAsGuid = maybeArg 4 System.Guid.Parse

        let fromCmdLine : FromCmdLine = fun sa ->
            let eMsg e = $"Failure in config - context - [{e}]"
            try create (sa.[1] |> System.Guid.Parse, sa.[2] |> System.Guid.Parse, sa.[3], sa |> subAsGuid) |> Ok
            with | e -> e.Message |> eMsg |> Error

    [<RequireQualifiedAccess>]
    module AppSetting =
        type FromHardCoded = unit -> Result<AppSetting,string>

        let private create (mngGrp, sFilter, aFilter, aFtype) = {
            AppSetting.startMngGrp = mngGrp
            scopeFilter = sFilter
            assignmentFilter = aFilter
            assignmentFiltertype = aFtype
        }

        let fromHardCoded : FromHardCoded = fun () ->
            let eMsg e = $"Failure in config - application - [{e}]"
            try ("e66a3cea-955c-45e8-b388-e962aa80c514" |>System.Guid.Parse,".*",None,None) |> create |> Ok
            with | e -> e.Message |> eMsg |> Error

    [<RequireQualifiedAccess>]
    module Technical =
        type FromHardCoded = unit -> Result<Technical,string>
        let private create timeout = {Technical.httpTimeout = timeout}
        let fromHardCoded : FromHardCoded = fun () ->
            let eMsg e = $"Failure in config - technical - [{e}]"
            try 5 |> create |> Ok
            with | e -> e.Message |> eMsg |> Error

    let init : Init = fun sa ->
        let ctx = Context.fromCmdLine sa
        let app = AppSetting.fromHardCoded ()
        let tech = Technical.fromHardCoded ()
        let toErr (r: Result<_,string>) = r |> function | Ok _ -> None | Error e -> Some e
        let err () =
            [toErr ctx; toErr app; toErr tech]
            |> List.choose id |> List.fold (fun e acc -> acc + e) ""

        match (ctx, app, tech) with
        | Ok c, Ok a, Ok t -> (c, a, t) |> create |> Ok
        | _ -> err () |> Error


