module TimelessShares

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

let Bernard = {name = "Bernard"
               birthday = Date(1960,3,15)
               accounts= []} 


let commands = [
    Create("huis")
    Participate(Anita, standard)
    Participate(Jolien, standard)
    Participate(Mark, standard)
    ]

let fundrunner = Aggregate.runner Fund.aggregate
let perform = fundrunner.ExecAll Fund.zero
let fund = perform commands

let initTwice = commands @ [Create("huis")]
let participateTwice = commands @ [Participate(Mark,standard)]
let updateShare = commands @ [Participate(Mark, extraRoom)]

[<Test>] 
let ``adding three participants to an emty fund yields 3``() = 
     test <@ fund.Participations.Length = 3 @>


[<Test>]
let ``updates are recorded and change history``()=
    let ps = (perform (commands @ [Participate(Mark, extraRoom)])).Participations
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

[<Test>]
let ``living together will change the participation``() =
    let ps = (perform (commands 
                       @ [Join(Anita, Bernard)])).Participations
             |> belongingTo "Anita"
             |> List.ofSeq
    ps.Length =? 1
    
[<Test>]
let ``joining a participant will add you as a participant``() =
    let ps = (perform (commands 
                       @ [Join(Anita, Bernard)])).Participations
             |> belongingTo "Anita"
             |> List.ofSeq
    ps.Length =? 1
    
