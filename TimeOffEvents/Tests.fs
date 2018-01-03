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

open System

let YESTERDAY = (DateTime.Now).AddDays(-1.);
let TODAY = DateTime.Now;
let TOMORROW = (DateTime.Now).AddDays(1.);
let INNDAYS n = (DateTime.Now).AddDays(n);

let requestMock = {
  UserId = 1
  RequestId = Guid.Empty
  Start = { Date = TOMORROW; HalfDay = AM }
  End = { Date = INNDAYS 5.; HalfDay = PM } }

let creationTests =
  testList "Creation tests" [
    test "A request is created in the future" {
      Given [ ]
      |> When (RequestTimeOff requestMock)
      |> Then (Ok [RequestCreated requestMock]) "The request has been created"
    }

    test "A request is created today" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TODAY; HalfDay = AM }
        End = { Date = INNDAYS 10.; HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff request)
      |> Then (Error "The request should start at least tomorrow") ""
    }

    test "A new request doesn't overlap" {
      let notOverlapping = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = INNDAYS 10.; HalfDay = AM }
        End = { Date = INNDAYS 15.; HalfDay = PM } }

      Given [ RequestValidated requestMock]
      |> When (RequestTimeOff notOverlapping)
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
      |> When (RequestTimeOff overlappingAll)
      |> Then (Error "Overlapping request") ""

      Given [ RequestValidated request ]
      |> When (RequestTimeOff overlappingLeft)
      |> Then (Error "Overlapping request") ""

      Given [ RequestValidated request ]
      |> When (RequestTimeOff overlappingRight)
      |> Then (Error "Overlapping request") ""
    }
  ]

let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      Given [ RequestCreated requestMock ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Ok [RequestValidated requestMock]) "The request has been validated"
    }
  ]

let refusalTests =
  testList "Refusal tests" [
    test "A request is refused" {
      Given [ RequestCreated requestMock ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Ok [RequestRefused requestMock]) "The request has been refused"
    }

    test "A request is already refused" {
      Given [ RequestRefused requestMock ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "Request already refused") ""
    }
    
    test "A request cannot be refused" {
      Given [ RequestValidated requestMock ]
      |> When (RefuseRequest (1, Guid.Empty))
      |> Then (Error "Request cannot be refused") ""
    }
  ]

let cancelTests =
  testList "Validation tests" [
    test "A request is cancelled when pending validation" {
      Given [ RequestCreated requestMock ]
      |> When (ManagerCancelRequest (1, Guid.Empty))
      |> Then (Ok [RequestManagerCancelled requestMock]) "The pending request has been cancelled"
    }
  ]

let tests =
  testList "All tests" [
    creationTests
    validationTests
    refusalTests
    cancelTests
  ]