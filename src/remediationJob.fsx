#load @"scope.fsx"
#load @"policyState.fsx"

namespace Remediation

type JobSpec = {
    name: string
    asJob: bool
    scopeName: string
    scopeId: string
    policyAssignmentId: string
    policyDefinitionReferenceId: string option
}
with
    override js.ToString() =
        $"Name: {js.name}\nScope: {js.scopeName}" +
        $"\npolicyAssignmentId: {js.policyAssignmentId.Split('/') |> Array.last}" +
        $"\nRefId: {js.policyDefinitionReferenceId}"

type PolicyStatesToJobSpecs = Policy.NonCompliance list -> JobSpec list

type Report = string -> JobSpec list -> unit

[<RequireQualifiedAccess>]
module JobSpec =
    let private create (sName, sId, aId, refId) =
        {
            JobSpec.name = "autoRemediation-" + System.Guid.NewGuid().ToString()
            asJob = true
            scopeName = sName
            scopeId = sId
            policyAssignmentId = aId
            policyDefinitionReferenceId = refId
        }

    let private policyStateToJobs (ps: Policy.NonCompliance) =
        match ps.assignmentType with
        | Policy.AssignmentType.Policy ->
            (ps.scopeName, ps.scopeId, ps.assignmentId, None) |> create |> List.singleton
        | Policy.AssignmentType.PolicySet refIds ->
            refIds |> List.map (fun refId -> (ps.scopeName, ps.scopeId, ps.assignmentId, refId |> Some) |> create)

    let policyStatesToJobSpecs : PolicyStatesToJobSpecs = fun pss ->
        pss |> List.map policyStateToJobs |> List.collect id