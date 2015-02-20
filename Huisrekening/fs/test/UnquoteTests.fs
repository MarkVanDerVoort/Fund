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

(*
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



//At any time there should only be one participation per participant
*)

