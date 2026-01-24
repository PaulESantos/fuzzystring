# fuzzystring

fuzzystring provides fast, data.table-backed fuzzy joins for data frames
using string distance metrics from stringdist. Use it when you need to
merge rows that are “close enough” (misspellings, inconsistent labels,
user input, or messy IDs).

Key features:

- inner/left/right/full/semi/anti fuzzy joins
- string distance methods from stringdist
- optional distance column in the result
- C++ row binding for performance on large joins

## Installation

``` r
# Development version from GitHub
# Using pak (recommended)
pak::pak("PaulESantos/fuzzystring")

# Or using remotes
remotes::install_github("PaulESantos/fuzzystring")
```

## Quick start

``` r
library(fuzzystring)

x <- data.frame(name = c("Idea", "Premiom", "Very Good"), id = 1:3)
y <- data.frame(approx_name = c("Ideal", "Premium", "VeryGood"), grp = c("A", "B", "C"))

fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
```

### Example of `fuzzystring_inner_join`: Correcting misspellings against a dictionary

Often you find yourself with a set of words that you want to combine
with a “dictionary”- it could be a literal dictionary (as in this case)
or a domain-specific category system. But you want to allow for small
differences in spelling or punctuation.

The package includes a set of common misspellings ([Wikipedia
list](https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines)):

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(fuzzystring)
data(misspellings)

misspellings
#> # A tibble: 4,505 × 2
#>    misspelling correct   
#>    <chr>       <chr>     
#>  1 abandonned  abandoned 
#>  2 aberation   aberration
#>  3 abilties    abilities 
#>  4 abilty      ability   
#>  5 abondon     abandon   
#>  6 abbout      about     
#>  7 abotu       about     
#>  8 abouta      about a   
#>  9 aboutit     about it  
#> 10 aboutthe    about the 
#> # ℹ 4,495 more rows
```

``` r
# use the dictionary of words from the qdapDictionaries package,
# which is based on the Nettalk corpus.
library(qdapDictionaries)
words <- dplyr::as_tibble(DICTIONARY)

words
#> # A tibble: 20,137 × 2
#>    word  syllables
#>    <chr>     <dbl>
#>  1 hm            1
#>  2 hmm           1
#>  3 hmmm          1
#>  4 hmph          1
#>  5 mmhmm         2
#>  6 mmhm          2
#>  7 mm            1
#>  8 mmm           1
#>  9 mmmm          1
#> 10 pff           1
#> # ℹ 20,127 more rows
```

As an example, we’ll pick 1000 of these words (you could try it on all
of them though), and use `fuzzystring_inner_join` to join them against
our dictionary.

``` r
set.seed(2016)
sub_misspellings <- misspellings %>%
  sample_n(1000)
```

``` r
joined <- sub_misspellings %>%
  fuzzystring_inner_join(words, by = c(misspelling = "word"), max_dist = 1)
```

By default, `fuzzystring_inner_join` uses optimal string alignment
(Damerau–Levenshtein distance), and we’re setting a maximum distance of
1 for a join. Notice that they’ve been joined in cases where
`misspelling` is close to (but not equal to) `word`:

``` r
joined
#>      misspelling    correct       word syllables
#>           <char>     <char>     <char>     <num>
#>   1:   cyclinder   cylinder   cylinder         3
#>   2: beastiality bestiality bestiality         5
#>   3:    affilate  affiliate  affiliate         4
#>   4:     supress   suppress   suppress         2
#>   5:    intevene  intervene  intervene         3
#>  ---                                            
#> 756:        fiel      field       file         1
#> 757:        fiel      field       fill         1
#> 758:        fiel      field       fuel         2
#> 759:        fiel      field       riel         2
#> 760:    aparment  apartment  apartment         3
```

#### Classification accuracy

Note that there are some redundancies; words that could be multiple
items in the dictionary. These end up with one row per “guess” in the
output. How many words did we classify?

``` r
joined %>%
  count(misspelling, correct)
#>      misspelling    correct     n
#>           <char>     <char> <int>
#>   1:      abilty    ability     1
#>   2:   accademic   academic     1
#>   3:    accademy    academy     1
#>   4:   accension  accession     2
#>   5:  acceptence acceptance     1
#>  ---                             
#> 458:        wnat       want     3
#> 459:  wonderfull  wonderful     1
#> 460:       wroet      wrote     1
#> 461:     wupport    support     1
#> 462:      zeebra      zebra     1
```

So we found a match in the dictionary for about half of the
misspellings. In how many of the ones we classified did we get at least
one of our guesses right?

``` r
which_correct <- joined %>%
  group_by(misspelling, correct) %>%
  summarize(guesses = n(), one_correct = any(correct == word))
#> `summarise()` has grouped output by 'misspelling'. You can override using the
#> `.groups` argument.

which_correct
#> # A tibble: 462 × 4
#> # Groups:   misspelling [453]
#>    misspelling correct     guesses one_correct
#>    <chr>       <chr>         <int> <lgl>      
#>  1 abilty      ability           1 TRUE       
#>  2 accademic   academic          1 TRUE       
#>  3 accademy    academy           1 TRUE       
#>  4 accension   accession         2 TRUE       
#>  5 acceptence  acceptance        1 TRUE       
#>  6 acedemic    academic          1 TRUE       
#>  7 achive      achieve           4 TRUE       
#>  8 acommodate  accommodate       1 TRUE       
#>  9 acuracy     accuracy          1 TRUE       
#> 10 addmission  admission         1 TRUE       
#> # ℹ 452 more rows

# percentage of guesses getting at least one right
mean(which_correct$one_correct)
#> [1] 0.8246753

# number uniquely correct (out of the original 1000)
sum(which_correct$guesses == 1 & which_correct$one_correct)
#> [1] 290
```

Not bad.

#### Reporting distance in the joined output

If you wanted to include the distance as a column in your output, you
can use the `distance_col` argument. For example, we may be interested
in how many words were *two* letters apart.

``` r
joined_dists <- sub_misspellings %>%
  fuzzystring_inner_join(words, by = c(misspelling = "word"), max_dist = 2,
                        distance_col = "distance")

joined_dists
#>       misspelling    correct       word syllables distance
#>            <char>     <char>     <char>     <num>    <num>
#>    1:   cyclinder   cylinder   cylinder         3        1
#>    2: beastiality bestiality bestiality         5        1
#>    3:    affilate  affiliate  affiliate         4        1
#>    4:    comitted  committed  committee         3        2
#>    5:    acquited  acquitted    acquire         2        2
#>   ---                                                     
#> 8431:    aparment  apartment   apparent         3        2
#> 8432:    aparment  apartment   averment         3        2
#> 8433:    aparment  apartment    garment         2        2
#> 8434:    aparment  apartment     parent         2        2
#> 8435:    aparment  apartment    payment         2        2
```

Note the extra `distance` column, which in this case will always be less
than or equal to 2. We could then pick the closest match for each, and
examine how many of our closest matches were 1 or 2 away:

``` r
closest <- joined_dists %>%
  group_by(misspelling) %>%
  top_n(1, desc(distance)) %>%
  ungroup()

closest
#> # A tibble: 1,336 × 5
#>    misspelling   correct     word          syllables distance
#>    <chr>         <chr>       <chr>             <dbl>    <dbl>
#>  1 cyclinder     cylinder    cylinder              3        1
#>  2 beastiality   bestiality  bestiality            5        1
#>  3 affilate      affiliate   affiliate             4        1
#>  4 comitted      committed   committee             3        2
#>  5 acquited      acquitted   acquire               2        2
#>  6 acquited      acquitted   acquit                2        2
#>  7 decompositing decomposing decomposition         5        2
#>  8 decieved      deceived    deceive               2        2
#>  9 asociated     associated  associate             4        2
#> 10 supress       suppress    suppress              2        1
#> # ℹ 1,326 more rows

closest %>%
  count(distance)
#> # A tibble: 3 × 2
#>   distance     n
#>      <dbl> <int>
#> 1        0     3
#> 2        1   739
#> 3        2   594
```

#### Other joining functions

Note that `fuzzystring_inner_join` is not the only function we can use.
If we’re interested in including the words that we *couldn’t* classify,
we could have use `fuzzystring_left_join`:

``` r
left_joined <- sub_misspellings %>%
  fuzzystring_left_join(words, by = c(misspelling = "word"), max_dist = 1)

left_joined

left_joined %>%
  filter(is.na(word))
#>        misspelling       correct   word syllables
#>             <char>        <char> <char>     <num>
#>   1:     Sanhedrim     Sanhedrin   <NA>        NA
#>   2: consicousness consciousness   <NA>        NA
#>   3:    repubicans   republicans   <NA>        NA
#>   4:      comitted     committed   <NA>        NA
#>   5:     emmisions     emissions   <NA>        NA
#>  ---                                             
#> 534:   accquainted    acquainted   <NA>        NA
#> 535:   ubiquitious    ubiquitous   <NA>        NA
#> 536:       Januray       January   <NA>        NA
#> 537:   aggaravates    aggravates   <NA>        NA
#> 538:       wayword       wayward   <NA>        NA
```

(To get *just* the ones without matches immediately, we could have used
`fuzzystring_anti_join`). If we increase our distance threshold, we’ll
increase the fraction with a correct guess, but also get more false
positive guesses:

``` r
left_joined2 <- sub_misspellings %>%
  fuzzystring_left_join(words, by = c(misspelling = "word"), max_dist = 2)

left_joined2

left_joined2 %>%
  filter(is.na(word))
#>        misspelling       correct   word syllables
#>             <char>        <char> <char>     <num>
#>   1:     Sanhedrim     Sanhedrin   <NA>        NA
#>   2: consicousness consciousness   <NA>        NA
#>   3:    repubicans   republicans   <NA>        NA
#>   4:     emmisions     emissions   <NA>        NA
#>   5:   commonweath  commonwealth   <NA>        NA
#>  ---                                             
#> 282:    surounding   surrounding   <NA>        NA
#> 283:     stainlees     stainless   <NA>        NA
#> 284:   accquainted    acquainted   <NA>        NA
#> 285:   ubiquitious    ubiquitous   <NA>        NA
#> 286:       wayword       wayward   <NA>        NA
```

Most of the missing words here simply aren’t in our dictionary.

You can try other distance thresholds, other dictionaries, and other
distance metrics (see \[stringdist-metrics\] for more). This function is
especially useful on a domain-specific dataset, such as free-form survey
input that is likely to be close to one of a handful of responses.
