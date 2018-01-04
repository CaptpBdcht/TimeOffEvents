module TimeOff.Tests

open Expecto
open EventStorage

let Given events = events
let When command events = events, command
let Then expected message (events: RequestEvent list, command) =
    let store = InMemoryStore.Create<UserId, RequestEvent>()
    for event in events do
      let stream = store.GetStream event.Request.UserId
      stream.Append [event]
    let result = Logic.handleCommand store command
    Expect.equal result expected message
    
let WhenIsCalled func events = events, func
let ThenCallForUser userId expected message (events: RequestEvent list, func) =
    let store = InMemoryStore.Create<UserId, RequestEvent>()
    for event in events do
      let stream = store.GetStream event.Request.UserId
      stream.Append [event]
    let result = func store userId
    Expect.equal result expected message

open System

let TODAY = DateTime.Now;
let YESTERDAY = TODAY.AddDays(-1.);
let TOMORROW = TODAY.AddDays(1.);
let INNDAYS n = TODAY.AddDays(n);

let requestMock = {
  UserId = 1
  RequestId = Guid.Empty
  Start = { Date = TOMORROW; HalfDay = AM }
  End = { Date = INNDAYS 5.; HalfDay = PM }
}

let creationTests =
  testList "Creation tests" [
    test "A request is created in the future" {
      Given [ ]
      |> When (RequestTimeOff (requestMock, User.Employee))
      |> Then (Ok [RequestCreated requestMock]) "The request has been created"
    }

    test "A request is created today" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TODAY; HalfDay = AM }
        End = { Date = INNDAYS 10.; HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff (request, User.Employee))
      |> Then (Error "The request should start at least tomorrow") ""
    }

    test "A new request doesn't overlap" {
      let notOverlapping = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = INNDAYS 10.; HalfDay = AM }
        End = { Date = INNDAYS 15.; HalfDay = PM } }

      Given [ RequestValidated requestMock]
      |> When (RequestTimeOff (notOverlapping, User.Employee))
      |> Then (Ok [RequestCreated notOverlapping]) "The request has been created"
    }

    test "A request overlaps" {
      let requestStart = { Date = INNDAYS 3.; HalfDay = AM }
      let requestEnd = { Date = INNDAYS 10.; HalfDay = PM }

      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = requestStart
        End = requestEnd }

      let overlappingAll = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TOMORROW; HalfDay = AM }
        End = { Date = INNDAYS 12.; HalfDay = PM } }

      let overlappingLeft = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TODAY; HalfDay = AM }
        End = requestStart }

      let overlappingRight = {
        UserId = 1
        RequestId = Guid.Empty
        Start = requestEnd
        End = { Date = INNDAYS 12.; HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (RequestTimeOff (overlappingAll, User.Employee))
      |> Then (Error "Overlapping request") ""

      Given [ RequestValidated request ]
      |> When (RequestTimeOff (overlappingLeft, User.Employee))
      |> Then (Error "Overlapping request") ""

      Given [ RequestValidated request ]
      |> When (RequestTimeOff (overlappingRight, User.Employee))
      |> Then (Error "Overlapping request") ""
    }
  ]

let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      Given [ RequestCreated requestMock ]
      |> When (ValidateRequest (1, Guid.Empty, User.Manager))
      |> Then (Ok [RequestValidated requestMock]) "The request has been validated"
    }

    test "A request is not validated by employee" {
      Given [ RequestCreated requestMock ]
      |> When (ValidateRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Employee cannot validate request") ""
    }
  ]

let refusalTests =
  testList "Refusal tests" [
    test "A request is refused" {
      Given [ RequestCreated requestMock ]
      |> When (RefuseRequest (1, Guid.Empty, User.Manager))
      |> Then (Ok [RequestRefused requestMock]) "The request has been refused"
    }

    test "A request is not refused by employee" {
      Given [ RequestCreated requestMock ]
      |> When (RefuseRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Employee cannot refuse request") ""
    }

    test "A request is already refused" {
      Given [ RequestRefused requestMock ]
      |> When (RefuseRequest (1, Guid.Empty, User.Manager))
      |> Then (Error "Request already refused") ""
    }
    
    test "A request cannot be refused" {
      Given [ RequestValidated requestMock ]
      |> When (RefuseRequest (1, Guid.Empty, User.Manager))
      |> Then (Error "Request cannot be refused") ""
    }
  ]

let cancelTests =
  testList "Cancellation tests" [
    test "A pending request is cancelled by manager" {
      Given [ RequestCreated requestMock ]
      |> When (ManagerCancelRequest (1, Guid.Empty, User.Manager))
      |> Then (Ok [RequestManagerCancelled requestMock]) "The pending request has been cancelled"
    }

    test "A pending request is cancelled by employee" {
      Given [ RequestCreated requestMock ]
      |> When (EmployeeCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Ok [RequestEmployeeCancelled requestMock]) "The pending request has been cancelled"
    }

    test "A validated request is cancelled by employee" {
      Given [ RequestValidated requestMock ]
      |> When (EmployeeCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Ok [RequestEmployeeCancelled requestMock]) "The validated has been cancelled"
    }

    test "Other requests cannot be cancelled by employee" {
      Given [ RequestManagerCancelled requestMock ]
      |> When (EmployeeCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Request cannot be cancelled") ""
    }
  ]

let askCancelTests =
  testList "Ask cancel tests" [
    test "Validated requests with start in past can be asked cancel" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = YESTERDAY; HalfDay = AM }
        End = { Date = INNDAYS 5.; HalfDay = PM }
      }

      Given [ RequestValidated request ]
      |> When (AskCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Ok [RequestAskCancelled request]) "The validated has been asked cancelation"
    }

    test "Validated requests with start in future cannot be asked cancel" {
      Given [ RequestValidated requestMock ]
      |> When (AskCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Cannot ask cancel for a timeoff in the future") ""
    }

    test "Pending ones shows lazy boys" {
      Given [ RequestCreated requestMock ]
      |> When (AskCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "You can cancel that by yourself!") ""
    }

    test "Others can't be" {
      Given [ RequestRefused requestMock ]
      |> When (AskCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Request cannot be asked cancellation") ""
    }
  ]

let refuseCancelTests =
  testList "Refuse cancel tests" [
    test "Requests can be refused cancellation" {
      Given [ RequestAskCancelled requestMock ]
      |> When (RefuseCancelRequest (1, Guid.Empty, User.Manager))
      |> Then (Ok [RequestValidated requestMock]) "The cancellation request has been refused"
    }

    test "But only if asked to" {
      Given [ RequestValidated requestMock ]
      |> When (RefuseCancelRequest (1, Guid.Empty, User.Manager))
      |> Then (Error "Request cannot be refused cancellation") ""
    }

    test "Employees cannot refuse cancellation" {
      Given [ RequestAskCancelled requestMock ]
      |> When (RefuseCancelRequest (1, Guid.Empty, User.Employee))
      |> Then (Error "Employee cannot refuse cancellation") ""
    }
  ]

let timeoffCalculationTests =
  let mockedStoreContent = [
    RequestValidated {
      UserId = 1
      RequestId = Guid.NewGuid()
      Start = { Date = YESTERDAY; HalfDay = AM }
      End = { Date = INNDAYS 3.; HalfDay = AM }
    }
    RequestValidated {
      UserId = 1
      RequestId = Guid.NewGuid()
      Start = { Date = TOMORROW; HalfDay = PM }
      End = { Date = INNDAYS 5.; HalfDay = PM }
    }
    RequestValidated {
      UserId = 2
      RequestId = Guid.NewGuid()
      Start = { Date = TOMORROW; HalfDay = AM }
      End = { Date = INNDAYS 5.; HalfDay = PM }
    }
  ]
  
  testList "Timeoff calculation tests" [
    test "Sum of timeoffs during 6 months" {
      let expected = 5.

      let date = DateTime(2017, 3, 1, 0, 0, 0, 0)
      let timeoffPerMonth = 2.5
      let sumOfTimeoffs = Logic.cumulativeBalance date timeoffPerMonth

      Expect.equal sumOfTimeoffs expected "The sum of timeoffs didn't match the expected result"
    }

    test "Should return effective requests" {
      let expected = Ok [ (List.head mockedStoreContent).Request ]

      Given mockedStoreContent
      |> WhenIsCalled Logic.effectiveRequests
      |> ThenCallForUser 1 expected "The returned requests didn't match the expected result"
    }
    test "Should return effective requests duration" {
      let expected = 4.5

      Given mockedStoreContent
      |> WhenIsCalled Logic.effectiveRequestsDuration
      |> ThenCallForUser 1 expected "The duration of effective requests didn't match the expected result"
    }

    test "Should return planned requests" {
      let expected = Ok [ mockedStoreContent.[1].Request ]

      Given mockedStoreContent
      |> WhenIsCalled Logic.plannedRequests
      |> ThenCallForUser 1 expected "The returned requests didn't match the expected result"
    }
    test "Should return planned requests duration" {
      let expected = 4.5

      Given mockedStoreContent
      |> WhenIsCalled Logic.plannedRequestsDuration
      |> ThenCallForUser 1 expected "The duration of planned requests didn't match the expected result"
    }

    test "Available balance should be equal to 3.5" {
      let date = DateTime(2017, 6, 1, 0, 0, 0, 0)
      let expected = 3.5

      Given mockedStoreContent
      |> WhenIsCalled (Logic.availableBalanceAt date)
      |> ThenCallForUser 1 expected "The available balance didn't match the expected result"
    }
  ]

let tests =
  testList "All tests" [
    creationTests
    validationTests
    refusalTests
    cancelTests
    askCancelTests
    refuseCancelTests
    timeoffCalculationTests
  ]