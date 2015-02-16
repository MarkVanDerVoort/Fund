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
    Participate(Anita, Date(2002,4,1))
    Participate(Jolien, Date(2015,5,1))
    Participate(Mark, Date(2004,1,1))
    ]

[<TestFixture>]
type ``Given an empty fund``() =

    let fundrunner = CQRS.runner Fund.aggregate

    let fund = fundrunner.ExecAll Fund.zero commands

    let commandsE = commands @ [Create("huis", Date(2011,1,1))]

    [<Test>] member test.
     ``adding three participants yields 3``() = 
         fund.Participations.Length |> should equal 3

    [<Test>] member test.
     ``initializing twice will throw an exception``()=
         (fun () -> fundrunner.ExecAll Fund.zero commandsE |> ignore) 
         |> should throw typeof<System.Exception>

     
       