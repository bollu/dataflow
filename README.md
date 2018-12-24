# An experimental Dataflow DSL within haskell

The aim is to understand if Arrows are the right fit to express dataflow
style computations in haskell. As we go along this process, we will proceed
to rip of ISPC, shaders, and possibly large parts of Futhark.

# History
this repo started out as a humble interpreter for MIT's tagged token data flow architecutre.
I eventually got around to reading about dataflow architectures, and realised
that this was precisely the sort of abstraction I've been looking for for a while
now. 

I think looked around for haskell libraries that supported the arrow interface
to build dataflow graphs, and found _none_!

Thus, shocked, I began to work on this library.

And now, I have an idea. Make this an [_SPMD_](https://ispc.github.io/) compiler!

# Free arrows

```hs
data FreeA eff a b where
    Pure :: (a -> b) -> FreeA eff a b
    Effect :: eff a b -> FreeA eff a b
    Seq :: FreeA eff a b -> FreeA eff b c -> FreeA eff a c
    Par :: FreeA eff a₁ b₁ -> FreeA eff a₂ b₂ -> FreeA eff (a₁, a₂) (b₁, b₂)

effect :: eff a b -> FreeA eff a b
effect = Effect

instance Category (FreeA eff) where
    id = Pure id
    (.) = flip Seq

instance Arrow (FreeA eff) where
    arr = Pure
    first f = Par f id
    second f = Par id f
    (***) = Par

```

- Turns out, [arrow desugaring is bad in GHC](https://stackoverflow.com/questions/45260173/proc-syntax-in-haskell-arrows-leads-to-severe-performance-penalty).
I suppose it's upto someone who uses arrows to fix this?

-- TODO: allow not having to pass `f -< () ` in arrow syntax if `f :: Arrow () b`.
    `b <- f` should "just work" in this case.

- **TODO: bench opaleye!** Perhaps this can be an interesting part of the
  finished product? (faster arrows)


# Reading
## SPMD
- [Blog posts by Matt pharr on the story of ISPC](https://pharr.org/matt/blog/2018/04/30/ispc-all.html)
- [Intel SPMD compiler](https://ispc.github.io/)
## Prior art in haskell
- [`Opaleye.Queryarr`](http://hackage.haskell.org/package/opaleye-0.6.7003.1/docs/Opaleye-Internal-QueryArr.html)
- [Causal commutative arrows](http://haskell.cs.yale.edu/wp-content/uploads/2012/06/FromJFP.pdf)
- [Inspecting free monads: `isovector/prospect`](https://github.com/isovector/prospect)
- [Free arrows by Dan piponi](http://blog.sigfpe.com/2017/01/building-free-arrows-from-components.html)
- [Notion of computation as monoids](https://arxiv.org/pdf/1406.4823.pdf)
- [Free arrows on SO](https://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows)

## Books and papers
The [`reading/`](reading/) folder contains links to useful paper. In general,
the links revolve around:

- The MIT tagged token architecture
- The `Id` programming language

I've also been looking for the book "Data flow computing: theory and practice",
but have been unable to find it (at a reasonable price: Amazon is ~ $150) 

