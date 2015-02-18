module EventTests

open Swensen.Unquote
open NUnit.Framework

open core
open DeelnameTypes
open Time
open Participant
open Participation
open Fund
open History

let Anita = {name= "Anita"
             birthday= Date(1964,10, 25)
             accounts=[]}

let Mark = {name = "Mark"
            birthday = Date(1965,5,18)
            accounts=[]}

let Jolien = {name = "Jolien"
              birthday = Date(1973,9,27)
              accounts=[]}


let commands = [
    Create("huis", Date(2001,1,1))
    Participate(Anita, Date(2002,4,1), standard)
    Participate(Jolien, Date(2015,5,1), standard)
    Participate(Mark, Date(2004,1,1), standard)
    ]

let fundrunner = Aggregate.runner Fund.aggregate
let perform = fundrunner.ExecAll Fund.zero
let fund = perform commands

let initTwice = commands @ [Create("huis", Date(2011,1,1))]
let participateTwice = commands @ [Participate(Mark, Date(2004,1,1),standard)]
let updateShare = commands @ [Participate(Mark, Date(2007,8,1), extraRoom)]

let belongingTo name = Seq.filter (fun p -> p.participant.name = name) 


[<Test>] 
let ``adding three participants to an emty fund yields 3``() = 
     test <@ fund.Participations.Length = 3 @>


[<Test>]
let ``updates are recorded and change history``()=
    let ps = (perform (commands @ [Participate(Mark, Date(2007,8,1), extraRoom)])).Participations
             |> per (Date(2008,1,1))
             |> belongingTo "Mark" 
             |> List.ofSeq   
    ps.Length =? 1 


[<Test>] 
let ``adding three participants yields 3``() = 
    fund.Participations.Length =? 3
   

[<Test>] 
let ``initializing twice will throw an exception``()=
    raises<System.Exception> <@ perform initTwice @> 

[<Test>] 
let ``participating twice is allowed, and recorded``()=
    (perform participateTwice).Participations.Length =? 4





