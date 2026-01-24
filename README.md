
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fuzzystring

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fuzzystring)](https://CRAN.R-project.org/package=fuzzystring)
[![R-CMD-check](https://github.com/PaulESantos/fuzzystring/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/fuzzystring/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PaulESantos/fuzzystring/graph/badge.svg)](https://app.codecov.io/gh/PaulESantos/fuzzystring)
<!-- badges: end -->

**fuzzystring** provides fast, flexible fuzzy string joins for data
frames using approximate string matching. Built on top of `data.table`
and `stringdist`, it’s designed for efficiently merging datasets where
exact matches aren’t possible due to misspellings, inconsistent
formatting, or slight variations in text.

## Why fuzzystring?

Real-world data is messy. You might need to:

- Match company names that are spelled slightly differently across
  datasets
- Join customer records with typos or abbreviations
- Reconcile product catalogs with inconsistent naming
- Link survey responses to predefined categories despite spelling errors
- Merge datasets where identifiers have been transcribed with errors

**fuzzystring** makes these tasks straightforward with:

- **All standard join types**: inner, left, right, full, semi, and anti
  joins
- **Multiple distance metrics**: Levenshtein, Damerau-Levenshtein,
  Jaro-Winkler, Soundex, and more
- **High performance**: C++ row binding and `data.table` backend for
  speed
- **Optional distance column**: Track how close matches are
- **Flexible matching**: Case-insensitive options and customizable
  distance thresholds

## Installation

``` r
# Development version from GitHub
# Using pak (recommended)
pak::pak("PaulESantos/fuzzystring")

# Or using remotes
remotes::install_github("PaulESantos/fuzzystring")
```

## Quick start

Here’s a simple example matching diamond cuts with slight misspellings:

``` r
library(fuzzystring)

# Your messy data
x <- data.frame(
  name = c("Idea", "Premiom", "Very Good"), 
  id = 1:3
)

# Reference data
y <- data.frame(
  approx_name = c("Ideal", "Premium", "VeryGood"), 
  grp = c("A", "B", "C")
)

# Fuzzy join with max distance of 2 edits
fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>      name id approx_name grp distance
#> 1:   Idea  1       Ideal   A        1
#> 2: Premiom  2     Premium   B        2
```

## Key features

### All join types supported

``` r
# Inner join - only matching rows
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), max_dist = 2)

# Left join - all rows from x, matching rows from y
fuzzystring_left_join(x, y, by = c(name = "approx_name"), max_dist = 2)

# Right join - all rows from y, matching rows from x
fuzzystring_right_join(x, y, by = c(name = "approx_name"), max_dist = 2)

# Full join - all rows from both tables
fuzzystring_full_join(x, y, by = c(name = "approx_name"), max_dist = 2)

# Semi join - rows from x that have a match in y
fuzzystring_semi_join(x, y, by = c(name = "approx_name"), max_dist = 2)

# Anti join - rows from x that don't have a match in y
fuzzystring_anti_join(x, y, by = c(name = "approx_name"), max_dist = 2)
```

### Multiple distance methods

``` r
# Optimal String Alignment (default)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "osa")

# Damerau-Levenshtein
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "dl")

# Jaro-Winkler (good for names)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "jw")

# Soundex (phonetic matching)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "soundex")
```

### Case-insensitive matching

``` r
fuzzystring_inner_join(
  x, y, 
  by = c(name = "approx_name"),
  ignore_case = TRUE,
  max_dist = 1
)
```

## Extended example: Correcting misspellings

A common use case is matching misspelled words against a dictionary. The
package includes a dataset of 4,500+ common misspellings from Wikipedia.

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

# Load misspellings dataset
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

Let’s match these against a real dictionary:

``` r
# Dictionary from qdapDictionaries package
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

Sample 1,000 misspellings and find their matches:

``` r
set.seed(2016)
sub_misspellings <- misspellings %>%
  sample_n(1000)

# Fuzzy join with max distance of 1
joined <- sub_misspellings %>%
  fuzzystring_inner_join(
    words, 
    by = c(misspelling = "word"), 
    max_dist = 1
  )

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

### Analyzing match quality

Check how many misspellings we successfully matched:

``` r
# Count unique misspellings matched
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

# Calculate accuracy
which_correct <- joined %>%
  group_by(misspelling, correct) %>%
  summarize(
    guesses = n(), 
    one_correct = any(correct == word),
    .groups = "drop"
  )

# Percentage getting at least one correct guess
mean(which_correct$one_correct)
#> [1] 0.8246753

# Number with exactly one correct match
sum(which_correct$guesses == 1 & which_correct$one_correct)
#> [1] 290
```

### Including distance metrics

Add a distance column to see how far apart the matches are:

``` r
joined_dists <- sub_misspellings %>%
  fuzzystring_inner_join(
    words, 
    by = c(misspelling = "word"), 
    max_dist = 2,
    distance_col = "distance"
  )

# Find the closest match for each misspelling
closest <- joined_dists %>%
  group_by(misspelling) %>%
  top_n(1, desc(distance)) %>%
  ungroup()

# Distribution of distances
closest %>%
  count(distance)
#> # A tibble: 3 × 2
#>   distance     n
#>      <dbl> <int>
#> 1        0     3
#> 2        1   739
#> 3        2   594
```

### Using different join types

**Left join** to keep unmatched words:

``` r
left_joined <- sub_misspellings %>%
  fuzzystring_left_join(
    words, 
    by = c(misspelling = "word"), 
    max_dist = 1
  )

# Show words we couldn't match
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

**Anti join** to find only unmatched words:

``` r
unmatched <- sub_misspellings %>%
  fuzzystring_anti_join(
    words, 
    by = c(misspelling = "word"), 
    max_dist = 1
  )

unmatched
#>            misspelling              correct
#> 1            Sanhedrim            Sanhedrin
#> 2        consicousness        consciousness
#> 3           repubicans          republicans
#> 4             comitted            committed
#> 5            emmisions            emissions
#> 6             acquited            acquitted
#> 7        decompositing          decomposing
#> 8             decieved             deceived
#> 9            asociated           associated
#> 10         commonweath         commonwealth
#> 11           supressed           suppressed
#> 12            soliders             soldiers
#> 13        aproximately        approximately
#> 14               htikn                think
#> 15               peoms                poems
#> 16         territorist            terrorist
#> 17         incorprates         incorporates
#> 18          Missisippi          Mississippi
#> 19            lazyness             laziness
#> 20       constituional       constitutional
#> 21            issueing              issuing
#> 22       accomodations       accommodations
#> 23            verisons             versions
#> 24             alusion             illusion
#> 25              lonley               lonely
#> 26         unsuprising         unsurprising
#> 27          succesfuly         successfully
#> 28             witheld             withheld
#> 29          motiviated            motivated
#> 30          resistence           resistance
#> 31            agrieved            aggrieved
#> 32  anthromorphization anthropomorphization
#> 33       incidentially         incidentally
#> 34            tempalte             template
#> 35         techiniques           techniques
#> 36            tyranies            tyrannies
#> 37       overshaddowed         overshadowed
#> 38        ansalisation         nasalisation
#> 39       enviornmental        environmental
#> 40            bradcast            broadcast
#> 41          abreviated          abbreviated
#> 42           noticably           noticeably
#> 43            sppeches             speeches
#> 44          occurences          occurrences
#> 45         effeciently          efficiently
#> 46         poportional         proportional
#> 47       unsuprisingly       unsurprisingly
#> 48              Malcom              Malcolm
#> 49         causalities           casualties
#> 50             futhroc              futhark
#> 51        reorganision       reorganisation
#> 52           ocurrance           occurrence
#> 53            humoural              humoral
#> 54        maintainence          maintenance
#> 55           rebllions           rebellions
#> 56           sountrack           soundtrack
#> 57            differnt            different
#> 58          methaphors            metaphors
#> 59        disemination        dissemination
#> 60           dimenions           dimensions
#> 61          dependancy           dependency
#> 62             hapened             happened
#> 63         breakthough         breakthrough
#> 64         volonteered          volunteered
#> 65              fufill              fulfill
#> 66             aeriels              aerials
#> 67           repubican           republican
#> 68       overwheliming         overwhelming
#> 69          asssassans            assassins
#> 70       juristictions        jurisdictions
#> 71           percepted            perceived
#> 72       anthropolgist       anthropologist
#> 73             hosited              hoisted
#> 74             siezing              seizing
#> 75           maltesian              Maltese
#> 76       volounteering         volunteering
#> 77            fulfiled            fulfilled
#> 78         mischevious          mischievous
#> 79           goverance           governance
#> 80           acheiving            achieving
#> 81          immidately          immediately
#> 82         constituant          constituent
#> 83        manouverable         maneuverable
#> 84    architechturally      architecturally
#> 85          controling          controlling
#> 86       transmissable        transmissible
#> 87        predomiantly        predominately
#> 88             meranda              veranda
#> 89            harrases             harasses
#> 90       consenquently         consequently
#> 91            leibnitz              leibniz
#> 92          councellor            councilor
#> 93          polinators          pollinators
#> 94          substracts            subtracts
#> 95           atributes           attributes
#> 96            noteably              notably
#> 97          messenging            messaging
#> 98         deteoriated         deteriorated
#> 99        assymetrical         asymmetrical
#> 100         fortelling          foretelling
#> 101       completetion           completion
#> 102      archaelogists       archaeologists
#> 103     restauranteurs        restaurateurs
#> 104         agreeement            agreement
#> 105         descisions            decisions
#> 106           greatful             grateful
#> 107             densly              densely
#> 108          millepede            millipede
#> 109         conciously          consciously
#> 110           diferent            different
#> 111      commuications       communications
#> 112    manisfestations       manifestations
#> 113           baceause              because
#> 114         inofficial           unofficial
#> 115          ciriculum           curriculum
#> 116        resignement           resignment
#> 117       charaterized        characterized
#> 118       unsucessfull         unsuccessful
#> 119             acused              accused
#> 120          referiang            referring
#> 121          offically           officially
#> 122      approximitely        approximately
#> 123         beleagured          beleaguered
#> 124         resteraunt           restaurant
#> 125          unwieldly             unwieldy
#> 126          scaleable             scalable
#> 127         realitvely           relatively
#> 128            poweful             powerful
#> 129          origanaly           originally
#> 130        successully         successfully
#> 131        insitutions         institutions
#> 132            relized             realised
#> 133         Fransiscan           Franciscan
#> 134          efficency           efficiency
#> 135        particuarly         particularly
#> 136          atheneans            athenians
#> 137          recidents            residents
#> 138            muscels              mussels
#> 139          excellant            excellent
#> 140         equivelant           equivalent
#> 141       culiminating          culminating
#> 142            breifly              briefly
#> 143              adres              address
#> 144       matheticians       mathematicians
#> 145          admitedly           admittedly
#> 146            shoudln            shouldn't
#> 147       occassionaly         occasionally
#> 148           Michagan             Michigan
#> 149       assasination        assassination
#> 150         marketting            marketing
#> 151      opthamologist      ophthalmologist
#> 152          vaguaries             vagaries
#> 153         practioner         practitioner
#> 154           aquiring            acquiring
#> 155        particually         particularly
#> 156           forbiden            forbidden
#> 157          Carmalite            Carmelite
#> 158            relized             realized
#> 159         terriories          territories
#> 160         assoicated           associated
#> 161             mileau               milieu
#> 162         ocasionaly         occasionally
#> 163         techicians          technicians
#> 164       UnitesStates         UnitedStates
#> 165           ocassion             occasion
#> 166          homestate           home state
#> 167        occurrances          occurrences
#> 168        philosphies         philosophies
#> 169          garantees           guarantees
#> 170               kwno                 know
#> 171          santioned           sanctioned
#> 172         reportadly           reportedly
#> 173          overthere           over there
#> 174            helpped               helped
#> 175   manoeuverability      maneuverability
#> 176         deriviated              derived
#> 177         doccuments            documents
#> 178            intered             interred
#> 179       caracterized        characterized
#> 180         exculsivly          exclusively
#> 181        transfering         transferring
#> 182              gogin                going
#> 183           pomotion            promotion
#> 184             quizes              quizzes
#> 185           funguses                fungi
#> 186        incomptable         incompatible
#> 187           poluting            polluting
#> 188    archetecturally      architecturally
#> 189         poulations          populations
#> 190       authoritiers          authorities
#> 191            meranda              Miranda
#> 192          proffesed            professed
#> 193            exliled               exiled
#> 194       dissobediant          disobedient
#> 195            vasalls              vassals
#> 196          obstruced           obstructed
#> 197          condemmed            condemned
#> 198         precentage           percentage
#> 199        charachters           characters
#> 200          controlls             controls
#> 201           formelly             formerly
#> 202       accomadation        accommodation
#> 203         embarrased          embarrassed
#> 204      dissobediance         disobedience
#> 205            suceeds             succeeds
#> 206        frequentily           frequently
#> 207            seaches             searches
#> 208         comissions          commissions
#> 209          shadasloo             shadaloo
#> 210         insistance           insistence
#> 211          dependant            dependent
#> 212           sophmore            sophomore
#> 213       incorportaed         incorporated
#> 214         electricly         electrically
#> 215         councellor            counselor
#> 216      comminication        communication
#> 217        assertation            assertion
#> 218          syphyllis             syphilis
#> 219      dissagreement         disagreement
#> 220          crticised           criticised
#> 221     unforetunately        unfortunately
#> 222       Rockerfeller          Rockefeller
#> 223           commedic              comedic
#> 224              unsed               unused
#> 225         sucesfully         successfully
#> 226         beautyfull            beautiful
#> 227        catagorized          categorized
#> 228           mchanics            mechanics
#> 229          Phonecian           Phoenecian
#> 230           Cambrige            Cambridge
#> 231      commmemorated         commemorated
#> 232           Carnagie             Carnegie
#> 233         equilibium          equilibrium
#> 234          contendor            contender
#> 235        harrasments          harassments
#> 236       traditionaly        traditionally
#> 237          reedeming            redeeming
#> 238           exicting             exciting
#> 239        repectively         respectively
#> 240           bigining            beginning
#> 241             aleged              alleged
#> 242          exerbated          exacerbated
#> 243           Europian             European
#> 244       substraction          subtraction
#> 245            liasons             liaisons
#> 246           intially            initially
#> 247            intered             interned
#> 248         corridoors            corridors
#> 249        difficulity           difficulty
#> 250         criticists              critics
#> 251           meerkrat              meerkat
#> 252      compatability        compatibility
#> 253         apperances          appearances
#> 254          beleiving            believing
#> 255          relativly           relatively
#> 256           persuits             pursuits
#> 257           strikely           strikingly
#> 258        indigineous           indigenous
#> 259            occured             occurred
#> 260         apparantly           apparently
#> 261          voleyball           volleyball
#> 262           montains            mountains
#> 263            teached               taught
#> 264         hsitorians           historians
#> 265          hieroglph           hieroglyph
#> 266          harrasing            harassing
#> 267          Dravadian            Dravidian
#> 268         propogates           propagates
#> 269        succesfully         successfully
#> 270        manufatured         manufactured
#> 271          paralelly           parallelly
#> 272           shineing              shining
#> 273       strenghtened         strengthened
#> 274          facilites           facilities
#> 275         recipiants           recipients
#> 276          octohedra            octahedra
#> 277         cerimonies           ceremonies
#> 278        delevopment          development
#> 279        eleminating          eliminating
#> 280          Novermber             November
#> 281           yeilding             yielding
#> 282         sucessfull           successful
#> 283          conquerer            conqueror
#> 284           allopone            allophone
#> 285         publishher            publisher
#> 286       unequalities         inequalities
#> 287        catapillars         caterpillars
#> 288          collecton           collection
#> 289       commisioning        commissioning
#> 290       crucifiction          crucifixion
#> 291         publisheed            published
#> 292         transfered          transferred
#> 293         Cincinatti           Cincinnati
#> 294        persistance          persistence
#> 295        specifiying           specifying
#> 296             Pucini              Puccini
#> 297          centruies            centuries
#> 298          criterias             criteria
#> 299      refridgerator         refrigerator
#> 300        inheritence          inheritance
#> 301      relinqushment       relinquishment
#> 302        dificulties         difficulties
#> 303       univeristies         universities
#> 304            harased             harassed
#> 305   phillosophically      philosophically
#> 306           spendour            splendour
#> 307         embezelled            embezzled
#> 308           intented             intended
#> 309       cannotations         connotations
#> 310        continously         continuously
#> 311       consistantly         consistently
#> 312           exellent            excellent
#> 313        critisizing          criticising
#> 314        Cincinnatti           Cincinnati
#> 315          dissapear            disappear
#> 316         restarants          restaurants
#> 317      transcendance        transcendence
#> 318        soverignity          sovereignty
#> 319      archeaologist         archeologist
#> 320             Enlish               enlist
#> 321          beggining            beginning
#> 322              tihkn                think
#> 323         seperating           separating
#> 324      heiroglyphics        hieroglyphics
#> 325              yaers                years
#> 326             casues               causes
#> 327           potrayed            portrayed
#> 328    untranslateable       untranslatable
#> 329        implimented          implemented
#> 330          harrassed             harassed
#> 331          diablical           diabolical
#> 332       compatiblity        compatibility
#> 333          esitmated            estimated
#> 334           dowloads            downloads
#> 335         ditributed          distributed
#> 336         maneouvred           manoeuvred
#> 337          receeding             receding
#> 338         immitating            imitating
#> 339        unneccesary          unnecessary
#> 340       volonteering         volunteering
#> 341            effords              efforts
#> 342           surveyer             surveyor
#> 343      subcatagories        subcategories
#> 344          algoritms           algorithms
#> 345            adequit             adequate
#> 346          Farenheit           Fahrenheit
#> 347            altough             although
#> 348          appenines            Apennines
#> 349        univesities         universities
#> 350         enforceing            enforcing
#> 351          triguered            triggered
#> 352          facinated           fascinated
#> 353          everthing           everything
#> 354        convienient           convenient
#> 355        continueing           continuing
#> 356           assasins            assassins
#> 357           succsess              success
#> 358         catagories           categories
#> 359         challanged           challenged
#> 360         acustommed           accustomed
#> 361          condamned            condemned
#> 362        unsuccesful         unsuccessful
#> 363         definately           definitely
#> 364           catagory             category
#> 365        asphyxation         asphyxiation
#> 366           anounced            announced
#> 367          permenant            permanent
#> 368          discribed            described
#> 369             illess              illness
#> 370         unsuprised          unsurprised
#> 371          performes            performed
#> 372          publisger            publisher
#> 373         adressable          addressable
#> 374          marrtyred             martyred
#> 375           emmiting             emitting
#> 376           Mythraic             Mithraic
#> 377     coincedentally       coincidentally
#> 378        signifigant          significant
#> 379          controled           controlled
#> 380       distingished        distinguished
#> 381         dimunitive           diminutive
#> 382           athenean             athenian
#> 383          addresing           addressing
#> 384             offred              offered
#> 385          propperly             properly
#> 386            infered             inferred
#> 387             habeus               habeas
#> 388            careing               caring
#> 389          adquiring            acquiring
#> 390         Guaduloupe            Guadalupe
#> 391       contributers         contributors
#> 392         aggreement            agreement
#> 393          introdued           introduced
#> 394      demographical          demographic
#> 395           acccused              accused
#> 396       speciallized          specialized
#> 397     contamporaries       contemporaries
#> 398     restaraunteurs        restaurateurs
#> 399       circumsicion         circumcision
#> 400             useing                using
#> 401          satelites           satellites
#> 402          countains             contains
#> 403           diffrent            different
#> 404      transcripting        transcription
#> 405           inventer             inventor
#> 406         cartilidge            cartilage
#> 407         bombarment          bombardment
#> 408          dipthongs           diphthongs
#> 409     archeaologists        archeologists
#> 410          survivied             survived
#> 411        rechargable         rechargeable
#> 412          mispelled           misspelled
#> 413      transcripting         transcribing
#> 414         descriibes            describes
#> 415        conotations         connotations
#> 416            visting             visiting
#> 417          collonies             colonies
#> 418           Godounov              Godunov
#> 419           agreemnt            agreement
#> 420           anwsered             answered
#> 421         consistant           consistent
#> 422      decomissioned       decommissioned
#> 423           pessiary              pessary
#> 424         parituclar           particular
#> 425         gaurenteed           guaranteed
#> 426      independendet          independent
#> 427       manouverable         manoeuvrable
#> 428         phenonmena            phenomena
#> 429      accomadations       accommodations
#> 430         rennovated            renovated
#> 431           reffered             referred
#> 432        accomodates         accommodates
#> 433        disasterous           disastrous
#> 434        unsurprized          unsurprised
#> 435           siezures             seizures
#> 436        appereances          appearances
#> 437          repentent            repentant
#> 438       controveries        controversies
#> 439         octohedral           octahedral
#> 440            dirived              derived
#> 441            cervial               serval
#> 442       inprisonment         imprisonment
#> 443         aggregious            egregious
#> 444        expiditions          expeditions
#> 445        distruction          destruction
#> 446           godesses            goddesses
#> 447           thikning             thinking
#> 448           alledges              alleges
#> 449       unfortunatly        unfortunately
#> 450         proffesion           profession
#> 451           linnaena             linnaean
#> 452             Guilia               Giulia
#> 453         addressess            addresses
#> 454       ressurection         resurrection
#> 455            shoudln               should
#> 456          imprioned           imprisoned
#> 457         developped            developed
#> 458              eyasr                 eyas
#> 459        indictement           indictment
#> 460         achivement          achievement
#> 461          nessecary            necessary
#> 462       negociations         negotiations
#> 463       neigbourhood        neighbourhood
#> 464          diphtongs           diphthongs
#> 465          disatrous           disastrous
#> 466        reminescent          reminiscent
#> 467       reccommended          recommended
#> 468            nucular              nuclear
#> 469         provisonal          provisional
#> 470           fimilies             families
#> 471           sucessot            successor
#> 472      carthographer         cartographer
#> 473           milennia            millennia
#> 474        critisising          criticising
#> 475       speciallized          specialised
#> 476            ocurred             occurred
#> 477           annouced            announced
#> 478          Europians            Europeans
#> 479          competive          competitive
#> 480           seceeded            succeeded
#> 481       catergorized          categorized
#> 482             reched              reached
#> 483          gaurentee            guarantee
#> 484        Napoleonian           Napoleonic
#> 485         publisherr            publisher
#> 486           portayed            portrayed
#> 487          eventally           eventually
#> 488          publihser            publisher
#> 489         exapansion            expansion
#> 490              endig               ending
#> 491      dissappointed         disappointed
#> 492           campains            campaigns
#> 493             rigeur                rigor
#> 494              konws                knows
#> 495          massmedia           mass media
#> 496           progroms              pogroms
#> 497         dispencing           dispensing
#> 498           iritated            irritated
#> 499        comissioned         commissioned
#> 500       traditionnal          traditional
#> 501           Guiseppe             Giuseppe
#> 502        acheivments         achievements
#> 503           persuing             pursuing
#> 504        autoctonous        autochthonous
#> 505        spectaulars         spectaculars
#> 506        artifically         artificially
#> 507         comisioner         commissioner
#> 508       infrantryman          infantryman
#> 509          definance             defiance
#> 510         deparments          departments
#> 511         Morrisette           Morissette
#> 512          deparment           department
#> 513         oppononent             opponent
#> 514      assasinations       assassinations
#> 515           annoints              anoints
#> 516           abudance            abundance
#> 517         draughtman          draughtsman
#> 518        refereneced           referenced
#> 519         fisionable          fissionable
#> 520       arrengements         arrangements
#> 521       indpendently        independently
#> 522          renedered                rende
#> 523 histocompatability   histocompatibility
#> 524           persuded            persuaded
#> 525          moleclues            molecules
#> 526      noncombatents        noncombatants
#> 527         disapeared          disappeared
#> 528            refrers               refers
#> 529         surounding          surrounding
#> 530           suprised            surprised
#> 531           heridity             heredity
#> 532           critized           criticized
#> 533          stainlees            stainless
#> 534        accquainted           acquainted
#> 535        ubiquitious           ubiquitous
#> 536            Januray              January
#> 537        aggaravates           aggravates
#> 538            wayword              wayward
```

## Performance

**fuzzystring** uses a C++ implementation for row binding combined with
a `data.table` backend for fast performance on large datasets. The
package is optimized for:

- Efficient string distance calculations with prefiltering for
  Levenshtein-like metrics
- Memory-efficient joins using data.table’s reference semantics
- Type-safe column operations preserving factors, dates, and other
  attributes

## Advanced usage

### Multiple column joins

``` r
# Match on both string similarity and numeric proximity
fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name", value = "approx_value"),
  match_fun = list(
    name = function(x, y) stringdist::stringdist(x, y) <= 1,
    value = function(x, y) abs(x - y) < 0.5
  )
)
```

### Custom distance functions

``` r
# Use custom matching logic
fstring_inner_join(
  x, y,
  by = c(name = "approx_name"),
  match_fun = function(x, y) {
    # Your custom matching logic
    result <- your_distance_function(x, y)
    result <= threshold
  }
)
```

## Related packages

- **[fuzzyjoin](https://github.com/dgrtwo/fuzzyjoin)**: Original fuzzy
  join implementation (dplyr-based)
- **[stringdist](https://github.com/markvanderloo/stringdist)**: String
  distance metrics
- **[data.table](https://rdatatable.gitlab.io/data.table/)**:
  High-performance data manipulation

**fuzzystring** builds on these great packages to provide a fast,
data.table-native implementation with C++ optimization.

## Contributing

Contributions are welcome! Please see
[CONTRIBUTING.md](.github/CONTRIBUTING.md) for guidelines.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.

## License

MIT © Paul E. Santos Andrade
