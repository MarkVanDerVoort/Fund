module core

module DeelnameTypes =

    type Iban = string
    type Id = string


module Participant = 
    open DeelnameTypes
    open Time

    type Participant = {
        name : string
        birthday : Date
        accounts : Iban list }

module Participation =
    open Participant
    open Time

    type Share = {groot : int; klein : int}
    let standard = {groot=1; klein =0}
    let extraRoom = {groot=1; klein=1}

    type Participation = {
            participant : Participant
            period : Period 
            share : Share}

module Aggregate =
    type Aggregate<'TState, 'TCommand, 'TEvent> = {          
          Zero : 'TState;
          Apply : 'TState -> 'TEvent -> 'TState;
          Exec : 'TState -> 'TCommand -> 'TEvent;
    }

    type Runner<'TState, 'TCommand, 'TEvent> = {
        ExecAll : 'TState -> 'TCommand list -> 'TState
        Replay  : 'TEvent list -> 'TState 
    }

    let runner aggregate = 
          let runOne state command = 
            let event = aggregate.Exec state command 
            aggregate.Apply state event

          let runAll state commands  
                = commands |> Seq.fold runOne state;
          let replay(events) = events |> Seq.fold aggregate.Apply aggregate.Zero
          { ExecAll = runAll
            Replay  = replay }
    
   

module Fund = 
    open DeelnameTypes
    open Participation
    open Participant
    open Time

    type Fund = {
        Id: Id
        Name: string
        Period: Period
        Participations : Participation list 
        }

    type FundCommand =
        | Create of string * Date
        | Close of Id * Date
        | Participate of Participant * Date * Share
       
    type FundEvent = 
        | Created of Id * string * Date
        | Closed of Id * Date
        | Participated of Participant * Date * Share

    let zero = {Id = ""; Name=""; Period = emptyPeriod; Participations = []}

    let apply fund = function
        | Created(id,name,date) -> {fund with Id=id
                                              Name=name
                                              Period={fund.Period with from=Some(date)}}
        | Closed(id,date)       -> {fund with Period={fund.Period with upto=Some(date)}}
        | Participated(p,d,s) -> {fund with Participations = {participant = p
                                                              period = starting d
                                                              share = s} :: fund.Participations} 

    let assertActive fund = if fund.Id = "" then failwith "fund is not active"
    let assertPristine fund = if fund.Id <> "" then failwith "fund is already initialized"
    let assertNewParticipation fund participant period= 
              if fund.Participations 
                 |> Seq.exists (*ion -> amt*) (fun ion -> ion.participant = participant &&
                                                          ion.period = period)
              then failwith "participant is already part of this fund"                               

    let exec fund = function
        | Create(name,date) -> fund |> assertPristine
                               Created("1",name, date)
        | Close(id,date) -> assertActive fund;
                            Closed(id,date)
        | Participate(p, d, s) -> assertActive fund //check for previous participation
                                  Participated(p,d,s)
    
    

    open Aggregate 
    let aggregate : Aggregate<Fund, FundCommand, FundEvent> =
        { Zero = zero 
          Apply = apply                            
          Exec = exec }  
    

    type ParticipationEvent =
        Join of Id * Participation       


