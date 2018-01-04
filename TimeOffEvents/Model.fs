namespace TimeOff

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

type TimeOffDashboard = {
    UserId: UserId
    CumulativeBalance: float
    EffectiveBalance: float
    PlannedBalance: float
    AvailableBalance: float
    Requests: List<TimeOffRequest>
}

type Command =
    | RequestTimeOff of TimeOffRequest * User
    | RefuseRequest of UserId * Guid * User
    | EmployeeCancelRequest of UserId * Guid * User
    | ManagerCancelRequest of UserId * Guid * User
    | AskCancelRequest of UserId * Guid * User
    | RefuseCancelRequest of UserId * Guid * User
    | ValidateRequest of UserId * Guid * User with
    member this.UserId =
        match this with
        | RequestTimeOff (request, _) -> request.UserId
        | RefuseRequest (userId, _, _) -> userId
        | EmployeeCancelRequest (userId, _, _) -> userId
        | ManagerCancelRequest (userId, _, _) -> userId
        | AskCancelRequest (userId, _, _) -> userId
        | RefuseCancelRequest (userId, _, _) -> userId
        | ValidateRequest (userId, _, _) -> userId
    member this.User =
        match this with
        | RequestTimeOff (_, user) -> Some user
        | RefuseRequest (_, _, user) -> Some user
        | EmployeeCancelRequest (_, _, user) -> Some user
        | ManagerCancelRequest (_, _, user) -> Some user
        | AskCancelRequest (_, _, user) -> Some user
        | RefuseCancelRequest (_, _, user) -> Some user
        | ValidateRequest (_, _, user) -> Some user

type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestEmployeeCancelled of TimeOffRequest
    | RequestManagerCancelled of TimeOffRequest
    | RequestAskCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestRefused request -> request
        | RequestEmployeeCancelled request -> request
        | RequestManagerCancelled request -> request
        | RequestAskCancelled request -> request
        | RequestValidated request -> request

module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Refused of TimeOffRequest
        | EmployeeCancelled of TimeOffRequest
        | ManagerCancelled of TimeOffRequest
        | AskToCancel of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Refused request -> request
            | EmployeeCancelled request -> request
            | ManagerCancelled request -> request
            | AskToCancel request -> request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | Refused _ -> false
            | EmployeeCancelled _ -> false
            | ManagerCancelled _ -> false
            | AskToCancel _ -> true
            | Validated _ -> true

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestRefused request -> Refused request
        | RequestEmployeeCancelled request -> EmployeeCancelled request
        | RequestManagerCancelled request -> ManagerCancelled request
        | RequestAskCancelled request -> AskToCancel request
        | RequestValidated request -> Validated request

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let getUserRequests (store: IStore<UserId, RequestEvent>) userId = 
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        getAllRequests events

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
            if requestState.Request.Start.Date > DateTime.Now then
                Ok [RequestEmployeeCancelled request]
            else
                Error "Cannor cancel a timeoff that already started"
        | _ ->
            Error "Request cannot be cancelled"

    let managerCancelRequest (requestState: RequestState) =
        if requestState.IsActive then
            Ok [RequestManagerCancelled requestState.Request]
        else
            Error "Request cannot be cancelled"

    let askCancelRequest requestState =
        match requestState with
        | Validated request ->
            if requestState.Request.Start.Date < DateTime.Now || requestState.Request.Start.Date.Equals DateTime.Now then
                Ok [RequestAskCancelled request]
            else
                Error "Cannot ask cancel for a timeoff in the future"
        | PendingValidation _ ->
            Error "You can cancel that by yourself!"
        | _ ->
            Error "Request cannot be asked cancellation"

    let refuseCancelRequest requestState =
        match requestState with
        | AskToCancel request ->
            Ok [RequestValidated request]
        | ManagerCancelled _ ->
            Error "Manager already cancelled request"
        | _ ->
            Error "Request cannot be refused cancellation"

    let effectiveTimeoffRequests (date: DateTime) activeRequests = 
        let effectiveRequests = 
            activeRequests
            |> Seq.filter (fun request -> request.Start.Date.Year = date.Year)
            |> Seq.filter (fun request -> request.Start.Date < date)

        Ok (Seq.toList effectiveRequests)

    let plannedTimeoffRequests (date: DateTime) activeRequests = 
        let plannedRequests =
            activeRequests
            |> Seq.filter (fun request -> request.Start.Date.Year = date.Year)
            |> Seq.filter (fun request -> request.Start.Date > date)

        Ok (Seq.toList plannedRequests)

    let userRequestsToStates requests = 
        requests
        |> Map.toSeq
        |> Seq.map (fun (_, state) -> state)

    let filterActives (requests: Map<Guid, RequestState>) =
        requests
        |> userRequestsToStates
        |> Seq.where (fun state -> state.IsActive)
        |> Seq.map (fun state -> state.Request)

    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userRequests = getUserRequests store command.UserId

        match command.User with
        | Some Manager ->
            match command with
            | RequestTimeOff (request, _) ->
                let activeRequests = filterActives userRequests
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

            | AskCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                askCancelRequest requestState

            | RefuseCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                refuseCancelRequest requestState

            | EmployeeCancelRequest _ -> Error "Manager cannot disguise in employee"

        | Some Employee ->
            match command with
            | RequestTimeOff (request, _) ->
                let activeRequests = filterActives userRequests
                createRequest activeRequests request

            | EmployeeCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                employeeCancelRequest requestState

            | AskCancelRequest (_, requestId, _) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                askCancelRequest requestState

            | ValidateRequest _ -> Error "Employee cannot validate request"

            | RefuseRequest _ -> Error "Employee cannot refuse request"

            | ManagerCancelRequest _ -> Error "Employee cannot disguise in manager"

            | RefuseCancelRequest _ -> Error "Employee cannot refuse cancellation"

        | None -> Error "Cannot process unknown User"

    let cumulativeBalance (date: DateTime) (timeoffPerMonth: float) = 
        (float date.Month - 1.) * timeoffPerMonth

    let requestDuration request = 
        let plainDays = float (request.End.Date.Subtract request.Start.Date).Days + 1.
        let deducedStartMorning = 
            match request.Start.HalfDay with
            | AM -> plainDays
            | PM -> plainDays - 0.5
        let deducedEndAfternoon = 
            match request.End.HalfDay with
            | AM -> deducedStartMorning - 0.5
            | PM -> deducedStartMorning

        deducedEndAfternoon

    let requestsDuration (requests: seq<TimeOffRequest>) = 
        Seq.sumBy requestDuration requests

    let effectiveRequests date store userId = 
        let userRequests = getUserRequests store userId
        let activeRequests = filterActives userRequests
        
        effectiveTimeoffRequests date activeRequests

    let effectiveRequestsDuration date store userId = 
        match effectiveRequests date store userId with
        | Ok requests -> requestsDuration requests
        | Error _ -> 0.

    let plannedRequests date store userId = 
        let userRequests = getUserRequests store userId
        let activeRequests = filterActives userRequests
        
        plannedTimeoffRequests date activeRequests

    let plannedRequestsDuration date store userId = 
        match plannedRequests date store userId with
        | Ok requests -> requestsDuration requests
        | Error _ -> 0.

    let availableBalanceAt date store userId = 
        let timeoffPerMonth = 2.5

        let balance = cumulativeBalance date timeoffPerMonth
        let effective = effectiveRequestsDuration date store userId
        let planned = plannedRequestsDuration date store userId

        balance - (float (effective + planned))
    
    let getUserRequestsChronologicallySorted (date: DateTime) store userId = 
        getUserRequests store userId
        |> userRequestsToStates
        |> Seq.map (fun state -> state.Request)
        |> Seq.filter (fun req -> req.Start.Date.Year = date.Year)
        |> Seq.sortBy (fun req -> req.Start.Date)
        |> Seq.toList

    let aggregateDashboard date store userId = 
        let timeoffPerMonth = 2.5

        {
            UserId = userId
            CumulativeBalance = cumulativeBalance date timeoffPerMonth
            EffectiveBalance = effectiveRequestsDuration date store userId
            PlannedBalance = plannedRequestsDuration date store userId
            AvailableBalance = availableBalanceAt date store userId
            Requests = getUserRequestsChronologicallySorted date store userId
        }