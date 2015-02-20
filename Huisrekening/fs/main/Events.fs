module core

module DeelnameTypes =

    type Iban = string
    type Id = string


module Participant = 
    open DeelnameTypes

    type Person = {
        name : string
        birthday : Time.Date
        accounts : Iban list }

module Participation =
    open Participant

    type Share = {groot : int; klein : int}
    let standard = {groot=1; klein =0}
    let extraRoom = {groot=1; klein=1}

    type Participation = {
            persons : Person list
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
        Active: bool
        Participations : Participation list 
        }

    type FundCommand =
        | Create of string
        | Close of Id
        | Participate of Person * Share
        | Join of Person * Person
       
    type FundEvent = 
        | Created of Id * string
        | Closed of Id
        | Participated of Person * Share
        | Joined of Person list * Share

    let zero = {Id = ""; Name=""; Participations = []; Active=false}

    let apply fund = function
        | Created(id,name) -> {fund with Id=id; Name=name; Active=true}
        | Closed(id)  ->      {fund with Active=false}
        | Participated(p,s) -> {fund with Participations = {persons = [p]; share = s} :: Participations} 
        | Joined(ps,s) -> {fund with Participations = {persons = ps; share = s} :: Participations}

    let assertActive fund = if fund.Id = "" then failwith "fund is not active"
    let assertPristine fund = if fund.Id <> "" then failwith "fund is already initialized"
    let assertNewParticipation fund participant = 
        if fund.Participations 
           |> Seq.exists (*ion -> amt*) (fun ion -> ion.participants |> Seq.exists participper then failwith "participant is already part of this fund"    

    let belongingTo name = Seq.filter (fun p -> p.persons 
                                                |> List.map (fun person -> person.name)
                                                |> Seq.exists name )

    let participation participant fund = 
        fund.Participations 
        |> List.filter (fun ion -> ion.participants |> Seq.exists participcipants)

    let assertParticipation participant fund =  
        if (participation participant fund) = [] 
        then failwith "Participant doesn't take part in this fund" 
         
    let exec fund = function
        | Create(name) -> fund |> assertPristine
                          Created("1",name)
        | Close(id) -> assertActive fund;
                       Closed(id)
        | Participate(p, s) -> assertActive fund //check for previous participation
                               Participated(p,s)
        | Join(a,b) -> fund |> assertParticipation a
                       let participation = participation a fund |> List.head
                       Joined(a,b,s participation)
    
    

    open Aggregate 
    let aggregate : Aggregate<Fund, FundCommand, FundEvent> =
        { Zero = zero 
          Apply = apply                            
          Exec = exec }  
    

  

