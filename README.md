# Estimation Calculator

Usage:

```ghci
:load Estimation.hs
let start = (DayMonth 30 3, 35752)
let middle = (DayMonth 13 7, 36030)
let end = (DayMonth 2 8, 36094)
prettyEstimate [start, middle, end] 10.12 0.2242
```
