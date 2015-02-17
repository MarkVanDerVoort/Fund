module coretest

open FsCheck
open NUnit.Framework
open FsUnit


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

[<TestFixture>]
type ``Given an empty fund``() =

    let initTwice = commands @ [Create("huis", Date(2011,1,1))]
    let participateTwice = commands @ [Participate(Mark, Date(2004,1,1),standard)]
    let updateShare = commands @ [Participate(Mark, Date(2007,8,1), extraRoom)]

    let contains (date:Date) = function
        | {from=Some start ; upto=None}       -> start <= date
        | {from=Some start ; upto=Some upto } -> start <= date &&  date < upto
        | {from=None;        upto=Some upto } -> date < upto
        | {from=None;        upto=None}       -> true

    let validAt date p = p.period |> contains date 
    let per date ps = ps |> Seq.filter (validAt date)

    [<Test>] member test.
     ``adding three participants yields 3``() = 
         fund.Participations.Length |> should equal 3

    [<Test>] member test.
     ``initializing twice will throw an exception``()=
         (fun () -> perform initTwice |> ignore) 
         |> should throw typeof<System.Exception>

    [<Test>] member test.
     ``participating twice is allowed, and recorded``()=
          (perform participateTwice).Participations.Length |> should equal 4

(* //I am having problems with let expression in fsUnit tests
    [<Test>] member test.
     ``updates are recorded and change history``()=
         let ps = (perform updateShare).Participations
                  |> per (Date(2008,1,1))
         ps.Length |> should equal 3
*)   



