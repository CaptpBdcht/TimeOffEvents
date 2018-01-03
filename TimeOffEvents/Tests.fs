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

let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 2, 7); HalfDay = AM }
        End = { Date = DateTime(2018, 2, 10); HalfDay = PM } }

      Given [ ]
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request has been created"
    }

    test "A new request doesn't overlap" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 2, 7); HalfDay = PM }
        End = { Date = DateTime(2018, 2, 10); HalfDay = PM } }

      let notOverlapping = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 2, 5); HalfDay = AM }
        End = { Date = DateTime(2018, 2, 7); HalfDay = AM } }

      Given [ RequestValidated request]
      |> When (RequestTimeOff notOverlapping)
      |> Then (Ok [RequestCreated notOverlapping]) "The request has been created"
    }

    test "A request overlaps" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 2, 7); HalfDay = AM }
        End = { Date = DateTime(2018, 2, 10); HalfDay = PM } }

      let overlapping = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 2, 5); HalfDay = AM }
        End = { Date = DateTime(2018, 2, 8); HalfDay = PM } }

      Given [ RequestValidated request ]
      |> When (RequestTimeOff overlapping)
      |> Then (Error "Overlapping request") ""
    }
  ]

let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2017, 12, 30); HalfDay = AM }
        End = { Date = DateTime(2017, 12, 30); HalfDay = PM } }

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