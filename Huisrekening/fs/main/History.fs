module History

type EventTracker() =
    let mutable events = []

    member this.Add event = events <- event :: events
    member this.History = events

    member this.Current = []//apply interpreter on all events