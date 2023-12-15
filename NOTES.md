It took me some time to make it. It looks very simple, but I've spend
a lot of time looking at different function types.

A little problem: I would like to write the types of functions in the code.
But there are problems with `ridid type` or `rigid variable` stuff.
So I decided not to write the types explicitly and it worked!
At first times, I wrote all the functions as global functions (I mean outside
the local scope of gps) and it worked. But with local functions I can create
closures and reference the list of operators in the gps argument instead of passing
it around and makind partial applications.

Types of functions in gps:
```haskell
achieveAll :: (Operator o g) => [g] -> [g] -> Maybe ([g], [o])
achieve :: (Operator o g) => ([g], [o]) -> g -> Maybe ([g], [o])
findApplicable :: [o]
tryApply :: o -> Maybe ([g], [o])
```