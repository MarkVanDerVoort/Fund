module EventTests

open Swensen.Unquote
open NUnit.Framework

open core
open DeelnameTypes
open Time
open Participant
open Participation
open Fund

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

let contains (date:Date) = function
    | {from=Some start ; upto=None}       -> start <= date
    | {from=Some start ; upto=Some upto } -> start <= date &&  date < upto
    | {from=None;        upto=Some upto } -> date < upto
    | {from=None;        upto=None}       -> true

let validAt date p = p.period |> contains date 

///Per ``name`` there is a single most up-to-date entry
///History doesn't combine earlier entries, as it would for instance on a beer-tab
///Hence a new participation is not an addendum
let lastItems ps = ps 
                   |> Seq.groupBy (fun p -> p.participant.name)
                   |> Seq.map( fun g -> (snd g) |> Seq.maxBy (fun p -> p.period.from) )

let per date ps = ps 
                  |> List.filter (validAt date)
                  |> lastItems 

let belongingTo name = Seq.filter (fun p -> p.participant.name = name) 


[<Test>]
let ``updates are recorded and change history``()=
    let ps = (perform (commands @ [Participate(Mark, Date(2007,8,1), extraRoom)])).Participations
             |> per (Date(2008,1,1))
             |> lastItems 
             |> belongingTo "Mark" 
             |> List.ofSeq   
    test <@ ps.Length = 1 @>  

