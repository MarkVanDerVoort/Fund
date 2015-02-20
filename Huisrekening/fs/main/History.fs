module History

(*
open Time

    let validAt date p = p.period |> contains date 


type Forgetful<'T,'Id> 
           (id:'T->'Id, 
            start: 'T->Date, 
            validAt: Date -> 'T  -> bool) =

    ///Per ``name`` there is a single most up-to-date entry
    ///History doesn't combine earlier entries, as it would for instance on a beer-tab
    ///Hence a new participation is not an addendum but a replacement
    let lastItems' id start ps = 
        ps 
        |> Seq.groupBy id
        |> Seq.map( fun g -> (snd g) |> Seq.maxBy start )

    ///LastItems determines how history is functioning
    ///- Here: the last item in each group
    ///- but can be any other aggregate function
    let lastItems = lastItems' id start

    //    apart from specifying the behaviour on a collection of elements
    //    we could specify it at a lower level, as the argument to be used in a fold.
    //
    //    Maybe then it could also translate into the intended behaviour of storing updates
    //    Let's see how this works when we get there.


    ///ValidAt determines how a time-bound item (i.e. an item having a period)
    ///specifies validity
    ///Here: starting before some ``date``  (implementation is buggy, need a test here)
    ///but could also have been contains a date
    let validAt date p = p.period |> contains date 

    let per date ps = 
        ps 
        |> List.filter (validAt date)
        |> lastItems 

    let upto date ps = 
        ps 
        |> List.filter (validAt date)

*)