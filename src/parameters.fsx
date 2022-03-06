namespace Parameters

type Context = {
    TenantID          : System.Guid
    ClientID          : System.Guid
    ClientSecret      : string
    SubscriptionID    : System.Guid option
}

type FromCmdLine = string array -> Result<Context,string>

[<RequireQualifiedAccess>]
module Context =

    let create (tenantID, clientID, clientSecret, subscriptionId) = {
        TenantID = tenantID
        ClientID = clientID
        ClientSecret = clientSecret
        SubscriptionID = subscriptionId
    }

    let private maybeArg i fApply (sa:string array) = if Array.length sa < i + 1 then None else Some (sa.[i] |> fApply)
    let private subAsGuid = maybeArg 4 System.Guid.Parse

    let fromCmdLine : FromCmdLine = fun sa ->
        let eMsg e = $"Missing tenantID, clientID, clientSecret [, subscriptionID] - or invalid GUID(s) - [{e}]"
        try create (sa.[1] |> System.Guid.Parse, sa.[2] |> System.Guid.Parse, sa.[3], sa |> subAsGuid) |> Ok
        with | e -> e.Message |> eMsg |> Error