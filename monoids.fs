//Generalizing the first equation
//1 + 2 = 3
(* operation on things yields the same thing *)
//Sum ? Mult ? Div ? Equals ? min ? max ? and ? or ? concat ? ...

(* You might ask: why is it so important that the operation return another thing of the same type? 
The answer is that you can chain together multiple objects using the operation.) *)

(*
For example, because 1 + 2 is another integer, you can add 3 to it. And then because 1 + 2 + 3 is an integer as well, 
you can keep going and add say, 4, to the result. In other words, it is only because integer addition fits the pattern 
that you can write a sequence of additions like this: 1 + 2 + 3 + 4. You couldn’t write 1 = 2 = 3 = 4 in the same way, 
because integer equality doesn’t fit the pattern.

And of course, the chain of combined items can be as long as we like. In other words, this pattern allows us to extend 
a pairwise operation into an operation that works on lists.

Mathematicians call the requirement that “the result is another one of these things” the closure requirement.)
*)

//Generalizing the second equation
//1 + (2 + 3) = (1 + 2) + 3
//Sum ? Mult ? Div ? Equals ? min ? max ? and ? or ? concat ? ...
(* Mathematicians call the requirement that “the order doesn’t matter” the associativity requirement.) *)

//Generalizing the third equation
//1 + 0 = 1
//Sum ? Mult ? Div ? Equals ? min ? max ? and ? or ? strings ? lists ? ...
(* A mathematician would say something like: that’s interesting – there is a special kind of thing (“zero”) that, when you 
combine it with something, just gives you back the original something, as if nothing had happened. *)

//Rules
(*
Rule 1 (Closure): The result of combining two things is always another one of the things.
Rule 2 (Associativity): When combining more than two things, which pairwise combination you do first doesn’t matter.
Rule 3 (Identity element): There is a special thing called “zero” such that when you combine any thing with “zero” you get 
       the original thing back.
*)

//Semigroup: obey only to the first 2 rules => strictly popsitive integers

//Classification: https://fsharpforfunandprofit.com/posts/monoids-without-tears/#a-table-of-classifications

module SampleQuotes =

    type [<Measure>] USD

    type Quote = {
        Product: string
        SharesCount: int
        Price: decimal<USD>
    }

    let addQuotes1 q1 q2 =
        (q1.SharesCount + q2.SharesCount, q1.Price + q2.Price)

    let addQuotes2 q1 q2 =
        {
            Product = "Total"
            SharesCount = q1.SharesCount + q2.SharesCount
            Price = q1.Price + q2.Price
        }

    type QuoteStats = {
        DistinctProducts: string list
        TotalSharesCount: int
        TotalPrice: decimal<USD>
    }

    let addStats qs1 qs2 =
        {
            DistinctProducts = qs1.DistinctProducts @ qs2.DistinctProducts |> List.distinct
            TotalSharesCount = qs1.TotalSharesCount + qs2.TotalSharesCount
            TotalPrice = qs1.TotalPrice + qs2.TotalPrice
        }

    let EmptyStats = {
        DistinctProducts = []
        TotalSharesCount = 0
        TotalPrice = 0.0M<USD>
    }

    let myProducts = [
        {Product="Patate"; SharesCount=13; Price=456.89M<USD>}
        {Product="Poulet"; SharesCount=62; Price=1200.00M<USD>}
        {Product="Tomate"; SharesCount=85; Price=4556.15M<USD>}
    ]

    let quoteToStats q =
        { DistinctProducts = [q.Product]; TotalSharesCount = q.SharesCount; TotalPrice = q.Price }

    let getQuoteStats = 
        List.map quoteToStats >> List.reduce addStats

    type QuoteStats with 
        static member (+) (x,y) = addStats x y
        static member Zero = EmptyStats
