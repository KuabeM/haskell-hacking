# Estimation Calculator

Usage:

```ghci
:load Estimation.hs
let start = (DayMonth 30 3, 35752)
let middle = (DayMonth 13 7, 36030)
let end = (DayMonth 2 8, 36094)
prettyEstimate [start, middle, end] 10.12 0.2242
```

With cabal:

```bash
cabal build
cabal exec haskell-hacking
# or execute directly
./dist-newstyle/build/x86_64-linux/ghc-8.8.3/haskell-hacking-0.1.0.0/x/haskell-hacking/build/haskell-hacking/haskell-hacking
```