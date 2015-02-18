module History

///Per ``name`` there is a single most up-to-date entry
///History doesn't combine earlier entries, as it would for instance on a beer-tab
///Hence a new participation is not an addendum but a replacement
let lastItems' id start ps = 
    ps 
    |> Seq.groupBy id
    |> Seq.map( fun g -> (snd g) |> Seq.maxBy start )

open Time 
open core.Participation

///LastItems determines how history is functioning
///- Here: the last item in each group
///- but can be any other aggregate function
let lastItems = lastItems' (fun p -> p.participant.name) (fun p -> p.period.from)

///ValidAt determines how a time-bound item (i.e. an item having a period)
///specifies validity
///Here: starting before some ``date``  (implementation is buggy, need a test here)
///but could also have been contains a date
let validAt date p = p.period |> contains date 

let per date ps = 
    ps 
    |> List.filter (validAt date)
    |> lastItems 

