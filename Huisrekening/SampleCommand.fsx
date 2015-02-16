#load "fs/main/Events.fs"

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

let fundrunner = CQRS.runner Fund.aggregate

fundrunner.ExecAll Fund.zero commands