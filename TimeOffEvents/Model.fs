﻿namespace TimeOff

open System
open EventStorage

type User = | Employee | Manager

type HalfDay = | AM | PM

type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type UserId = int

type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type Command =
    | RequestTimeOff of TimeOffRequest * User
    | RefuseRequest of UserId * Guid * User
    | EmployeeCancelRequest of UserId * Guid * User
    | ManagerCancelRequest of UserId * Guid * User
    | ValidateRequest of UserId * Guid * User with
    member this.UserId =
        match this with
        | RequestTimeOff (request, _) -> request.UserId
        | RefuseRequest (userId, _, _) -> userId
        | EmployeeCancelRequest (userId, _, _) -> userId
        | ManagerCancelRequest (userId, _, _) -> userId
        | ValidateRequest (userId, _, _) -> userId
    member this.User =
        match this with
        | RequestTimeOff (_, user) -> Some user
        | RefuseRequest (_, _, user) -> Some user
        | EmployeeCancelRequest (_, _, user) -> Some user
        | ManagerCancelRequest (_, _, user) -> Some user
        | ValidateRequest (_, _, user) -> Some user

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestEmployeeCancelled of TimeOffRequest
    | RequestManagerCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestRefused request -> request
        | RequestEmployeeCancelled request -> request
        | RequestManagerCancelled request -> request
        | RequestValidated request -> request

module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Refused of TimeOffRequest
        | EmployeeCancelled of TimeOffRequest
        | ManagerCancelled of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Refused request -> request
            | EmployeeCancelled request -> request
            | ManagerCancelled request -> request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Refused _ -> false
            | EmployeeCancelled _ -> false
            | ManagerCancelled _ -> false
            | Validated _ -> true

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestRefused request -> Refused request
        | RequestEmployeeCancelled request -> EmployeeCancelled request
        | RequestManagerCancelled request -> ManagerCancelled request
        | RequestValidated request -> Validated request

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) request =
        let overlapsWith request otherRequest =
            let beforeOrEqual boundary1 boundary2 =
                // Before
                (boundary1.Date < boundary2.Date) ||
                (boundary1.Date.Equals boundary2.Date && 
                    // Equal
                    (boundary1.HalfDay.Equals boundary2.HalfDay ||
                    // Before
                     boundary1.HalfDay.Equals AM && boundary2.HalfDay.Equals PM))

            beforeOrEqual otherRequest.Start request.End &&
            beforeOrEqual request.Start otherRequest.End
        
        Seq.exists (overlapsWith request) previousRequests

    let createRequest previousRequests request =
        let today = DateTime.Now;

        // Un employé ne peut pas faire une demande qui superpose une plage existante
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        // "Un employé peut uniquement effectuer des demandes qui commencent à une date future"
        elif request.Start.Date < today || request.Start.Date.Equals today then
            Error "The request should start at least tomorrow"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
    
    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | Refused _ ->
            Error "Request already refused"
        | _ ->
            Error "Request cannot be refused"

    let employeeCancelRequest requestState =
        match requestState with
        | PendingValidation request
        | Validated request ->
            Ok [RequestEmployeeCancelled request]
        | _ ->
            Error "Request cannot be refused"

    let managerCancelRequest requestState =
        match requestState with
        | PendingValidation request
        | Validated request ->
            Ok [RequestManagerCancelled request]
        | _ ->
            Error "Request cannot be refused"

    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userId = command.UserId
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events

        match command.User with
        | Some Manager ->
            match command with
            | RequestTimeOff (request, _) ->
                let activeRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeRequests request

            | ValidateRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                validateRequest requestState 

            | RefuseRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                refuseRequest requestState

            | ManagerCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                managerCancelRequest requestState

            | EmployeeCancelRequest _ -> Error "Manager cannot disguise in employee"

        | Some Employee ->
            match command with
            | RequestTimeOff (request, _) ->
                let activeRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeRequests request

            | EmployeeCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                employeeCancelRequest requestState

            | ValidateRequest _ -> Error "Employee cannot validate request"

            | RefuseRequest _ -> Error "Employee cannot refuse request"

            | ManagerCancelRequest _ -> Error "Employee cannot disguise in manager"

        | None -> Error "Cannot process unknown User"
