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

let creationTests =
  testList "Creation tests" [
    test "A request is created in the future" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TOMORROW; HalfDay = AM }
        End = { Date = INNDAYS 10.; HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request has been created"
    }

    // test "A request is created today" {
    //   let request = {
    //     UserId = 1
    //     RequestId = Guid.Empty
    //     Start = { Date = TODAY; HalfDay = AM }
    //     End = { Date = INNDAYS 10.; HalfDay = PM } }

    //   Given [ ]
    //   |> When (RequestTimeOff request)
    //   |> Then (Ok [RequestCreated request]) "The request has been created"
    // }

    test "A new request doesn't overlap" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TOMORROW; HalfDay = AM }
        End = { Date = INNDAYS 5.; HalfDay = PM } }

      let notOverlapping = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = INNDAYS 10.; HalfDay = AM }
        End = { Date = INNDAYS 15.; HalfDay = PM } }

      Given [ RequestValidated request]
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

      // This edge case doesn't work right now
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
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = TOMORROW; HalfDay = AM }
        End = { Date = INNDAYS 5.; HalfDay = PM } }

      Given [ RequestCreated request ]
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Ok [RequestValidated request]) "The request has been validated"
    }
  ]

let tests =
  testList "All tests" [
    creationTests
    validationTests
  ]