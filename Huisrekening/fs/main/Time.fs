module Time

open System

type Date(date:System.DateTime) =
    let d=date
    new(y,m,d) = Date(System.DateTime(y,m,d))
    member x.weekDay = d.DayOfWeek
    member x.dt = d
    override this.ToString() = date.ToString("yyyy-MM-dd")
    interface IComparable with
        member x.CompareTo(y) = match y with
                                | :? Date as yd-> x.dt.CompareTo(yd.dt) 
                                | _ -> failwith "Expected a Date in comparison"

type Period = {
    from :  Date option
    upto  : Date option }

let starting d = {from =  Some(d); upto = None}

let emptyPeriod = {from=None; upto= None}

let contains (date:Date) = function
    | {from=Some start ; upto=None}       -> start <= date
    | {from=Some start ; upto=Some upto } -> start <= date &&  date < upto
    | {from=None;        upto=Some upto } -> date < upto
    | {from=None;        upto=None}       -> true


type When<'event> =
    | At of Date * 'event
    | Through of Period * 'event
