# An experimental Dataflow DSL within haskell

The aim is to understand if Arrows are the right fit to express dataflow
style computations in haskell.

# History
this repo started out as a humble interpreter for MIT's tagged token data flow architecutre.
I eventually got around to reading about dataflow architectures, and realised
that this was precisely the sort of abstraction I've been looking for for a while
now. 

I think looked around for haskell libraries that supported the arrow interface
to build dataflow graphs, and found _none_!

Thus, shocked, I began to work on this library.

# Reading
## Prior art in haskell
- [`Opaleye.Queryarr`](http://hackage.haskell.org/package/opaleye-0.6.7003.1/docs/Opaleye-Internal-QueryArr.html)
- [Causal commutative arrows](http://haskell.cs.yale.edu/wp-content/uploads/2012/06/FromJFP.pdf)

## Books and papers
The [`reading/`](reading/) folder contains links to useful paper. In general,
the links revolve around:

- The MIT tagged token architecture
- The `Id` programming language

I've also been looking for the book "Data flow computing: theory and practice",
but have been unable to find it (at a reasonable price: Amazon is ~ $150) 

