# fuzzystring [![](reference/figures/fuzzystring.png)](https://paulesantos.github.io/fuzzystring/)

**fuzzystring** provides fast, flexible fuzzy string joins for data
frames using approximate string matching. Built on top of `data.table`
and `stringdist`, it uses an expanded compiled C++ backend for match
expansion, row binding, and result assembly, while adaptive candidate
planning reduces unnecessary distance evaluations before calling
`stringdist`.

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
- **High performance**: expanded compiled C++ execution path plus
  `data.table` orchestration for speed
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
#> ✔ dplyr     1.2.0     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
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
#>         misspelling         correct            word syllables
#> 1         cyclinder        cylinder        cylinder         3
#> 2       beastiality      bestiality      bestiality         5
#> 3          affilate       affiliate       affiliate         4
#> 4           supress        suppress        suppress         2
#> 5          intevene       intervene       intervene         3
#> 6         resaurant      restaurant      restaurant         3
#> 7         univesity      university      university         5
#> 8        allegedely       allegedly       allegedly         4
#> 9           emiting        emitting         smiting         2
#> 10          probaly        probably        probably         3
#> 11     architectual   architectural   architectural         5
#> 12       excecution       execution       execution         4
#> 13           beggin         begging           begin         2
#> 14           beggin         begging          leggin         2
#> 15            sinse           since           rinse         1
#> 16            sinse           since           sense         1
#> 17            sinse           since           since         1
#> 18            sinse           since           singe         1
#> 19       analogeous       analogous       analogous         4
#> 20        possibile        possible        possible         3
#> 21      consituency    constituency    constituency         5
#> 22         diversed         diverse         diverse         2
#> 23       technitian      technician      technician         3
#> 24            levle           level           levee         2
#> 25            levle           level           level         2
#> 26    consttruction    construction    construction         3
#> 27         substace       substance       substance         2
#> 28        concensus       consensus       consensus         3
#> 29         proceded       proceeded         proceed         2
#> 30         garantee       guarantee         grantee         2
#> 31         garantee       guarantee       guarantee         3
#> 32          pamplet        pamphlet        pamphlet         2
#> 33         rebounce         rebound        renounce         2
#> 34         arbitary       arbitrary       arbitrary         4
#> 35          tenacle        tentacle         tenable         3
#> 36          tenacle        tentacle        tentacle         3
#> 37         refernce       reference       reference         3
#> 38      prespective     perspective     perspective         3
#> 39      prespective     perspective     prospective         3
#> 40      prespective     perspective      respective         3
#> 41          liasion         liaison         liaison         3
#> 42            diety           deity           deity         3
#> 43            diety           deity            diet         2
#> 44            diety           deity           ditty         2
#> 45            diety           deity           piety         3
#> 46           aswell         as well           swell         1
#> 47        plausable       plausible       plausible         3
#> 48        monestary       monastery        monetary         4
#> 49       unecessary     unnecessary       necessary         4
#> 50            neice           niece           deice         2
#> 51            neice           niece            nice         1
#> 52            neice           niece           niece         1
#> 53        busineses        business        business         2
#> 54      responisble     responsible     responsible         4
#> 55            donig           doing           doing         2
#> 56      contempoary    contemporary    contemporary         5
#> 57            leage          league          league         1
#> 58            leage          league           lease         1
#> 59            leage          league           leave         1
#> 60            leage          league           ledge         1
#> 61    infilitration    infiltration    infiltration         4
#> 62             moil           mohel            boil         1
#> 63             moil           mohel            coil         1
#> 64             moil           mohel            mail         1
#> 65             moil           mohel             mil         1
#> 66             moil           mohel            moil         1
#> 67             moil           mohel            moll         1
#> 68             moil           mohel             oil         1
#> 69             moil           mohel            soil         1
#> 70             moil           mohel            toil         1
#> 71      undreground     underground     underground         3
#> 72         esential       essential       essential         3
#> 73          casuing         causing          casing         2
#> 74             tiem            time            item         2
#> 75             tiem            time            teem         1
#> 76             tiem            time            them         1
#> 77             tiem            time             tie         1
#> 78             tiem            time            tier         1
#> 79             tiem            time            time         1
#> 80       conjecutre      conjecture      conjecture         3
#> 81       attendence      attendance      attendance         3
#> 82            wroet           wrote           wrote         1
#> 83        existince       existence       existence         3
#> 84            theri           their           their         1
#> 85            theri           their           there         1
#> 86        busineses      businesses        business         2
#> 87           exinct         extinct         extinct         2
#> 88            borke           broke            bore         1
#> 89            borke           broke           borne         1
#> 90            borke           broke           broke         1
#> 91           bedore          before          before         2
#> 92          comando        commando        commando         3
#> 93           preiod          period          period         3
#> 94         wherease         whereas         whereas         2
#> 95         attitide        attitude        attitude         3
#> 96          returnd        returned          return         2
#> 97            scoll          scroll           scold         1
#> 98            scoll          scroll           scowl         1
#> 99            scoll          scroll          scroll         1
#> 100           scoll          scroll           scull         1
#> 101           whith            with           which         1
#> 102           whith            with           whish         1
#> 103           whith            with            whit         1
#> 104           whith            with           white         1
#> 105           whith            with            with         1
#> 106           housr           hours            hour         1
#> 107           housr           hours           house         1
#> 108       privilage       privilege       privilege         3
#> 109      inevitible      inevitable      inevitable         5
#> 110         carnige        Carnegie         carnage         2
#> 111          broady         broadly           broad         1
#> 112       psycology      psychology      psychology         4
#> 113           earnt          earned            earn         1
#> 114       negociate       negotiate       negotiate         4
#> 115           theif           thief           their         1
#> 116           theif           thief           thief         1
#> 117         enameld        enameled          enamel         3
#> 118     appeareance      appearance      appearance         3
#> 119         electic        electric        eclectic         3
#> 120         electic        electric        electric         3
#> 121             adn             and              ad         1
#> 122             adn             and             add         1
#> 123             adn             and             ado         2
#> 124             adn             and              an         1
#> 125             adn             and             and         1
#> 126             adn             and             awn         1
#> 127          exampt          exempt          exempt         2
#> 128           rised            rose            rise         1
#> 129           rised            rose           risen         2
#> 130           rised            rose           riser         2
#> 131        synphony        symphony        symphony         3
#> 132          extint         extinct          extant         2
#> 133          extint         extinct         extinct         2
#> 134     govermental    governmental    governmental         4
#> 135           fleed            fled           bleed         1
#> 136           fleed            fled            feed         1
#> 137           fleed            fled            fled         1
#> 138           fleed            fled            flee         1
#> 139           fleed            fled           fleer         1
#> 140           fleed            fled           fleet         1
#> 141      automibile      automobile      automobile         4
#> 142        notabley         notably         notable         3
#> 143           alsot            also           allot         2
#> 144           alsot            also            also         2
#> 145           soley          solely            sole         1
#> 146           soley          solely          solely         2
#> 147           soley          solely           soles         2
#> 148    archetecture    architecture    architecture         4
#> 149           whcih           which           which         1
#> 150        univeral       universal       universal         4
#> 151            wnat            want            gnat         1
#> 152            wnat            want            want         1
#> 153            wnat            want            what         1
#> 154    congresional   congressional   congressional         4
#> 155         emition         emotion         edition         3
#> 156         emition         emotion         emotion         3
#> 157        vengence       vengeance       vengeance         2
#> 158      identicial       identical       identical         4
#> 159           mileu          milieu            mile         1
#> 160           mileu          milieu          milieu         2
#> 161         sensure         censure         censure         2
#> 162       professer       professor       professor         3
#> 163          beteen         between         between         2
#> 164       anomolous       anomalous       anomalous         4
#> 165        seperate        separate        separate         3
#> 166     alltogether      altogether      altogether         4
#> 167         volumne          volume          volume         2
#> 168        polution       pollution        solution         3
#> 169         brethen        brethren        brethren         3
#> 170      informtion     information     information         4
#> 171         repulic        republic        republic         3
#> 172      appearence      appearance      appearance         3
#> 173          liason         liaison         liaison         3
#> 174         artcile         article         article         3
#> 175     preferrably      preferably      preferably         4
#> 176        calander        calender        colander         3
#> 177         confids        confides         confide         2
#> 178     initiaitive      initiative      initiative         4
#> 179         valuble        valuable        valuable         4
#> 180         valuble        valuable         voluble         3
#> 181           tothe          to the           tithe         1
#> 182           tothe          to the            tote         1
#> 183          pumkin         pumpkin         pumpkin         2
#> 184        accademy         academy         academy         4
#> 185           mamal          mammal            mama         2
#> 186           mamal          mammal          mammal         2
#> 187       proceding       preceding       preceding         3
#> 188        cemetary        cemetery        cemetery         4
#> 189      availaible       available       available         4
#> 190            diea            idea             die         1
#> 191            diea            idea            dies         1
#> 192            diea            idea            diet         2
#> 193            diea            idea            diva         2
#> 194            diea            idea            idea         3
#> 195         tremelo         tremolo         tremolo         3
#> 196       opthalmic      ophthalmic      ophthalmic         3
#> 197         emporer         emperor         empower         3
#> 198         epsiode         episode         episode         3
#> 199       parisitic       parasitic       parasitic         4
#> 200       subsquent      subsequent      subsequent         3
#> 201            moil            soil            boil         1
#> 202            moil            soil            coil         1
#> 203            moil            soil            mail         1
#> 204            moil            soil             mil         1
#> 205            moil            soil            moil         1
#> 206            moil            soil            moll         1
#> 207            moil            soil             oil         1
#> 208            moil            soil            soil         1
#> 209            moil            soil            toil         1
#> 210            thru         through            thou         1
#> 211            thru         through           thrum         1
#> 212         cannnot          cannot          cannot         2
#> 213         suposed        supposed        supposed         2
#> 214           idaes           ideas            ides         1
#> 215         excange        exchange        exchange         2
#> 216     bellweather      bellwether      bellwether         3
#> 217      archictect       architect       architect         3
#> 218           deram           dream            dram         1
#> 219           deram           dream           dream         1
#> 220      repatition     repartition      repetition         4
#> 221   perpindicular   perpendicular   perpendicular         5
#> 222          runing         running         running         2
#> 223            owrk            work            work         1
#> 224         mariage        marriage        marriage         2
#> 225           wierd           weird           weird         1
#> 226           wierd           weird           wield         1
#> 227         boundry        boundary        boundary         3
#> 228         boundry        boundary         foundry         2
#> 229          glight          flight          alight         2
#> 230          glight          flight          blight         1
#> 231          glight          flight          flight         1
#> 232          glight          flight           light         1
#> 233          glight          flight          plight         1
#> 234          glight          flight          slight         1
#> 235     deparmental    departmental    departmental         4
#> 236         sideral        sidereal        sidereal         4
#> 237       vincinity        vicinity        vicinity         4
#> 238         capible         capable         capable         3
#> 239        litature      literature        ligature         3
#> 240       nutritent        nutrient        nutrient         3
#> 241         ocuntry         country         country         2
#> 242        opponant        opponent        opponent         3
#> 243      symetrical     symmetrical     symmetrical         4
#> 244      likelyhood      likelihood      likelihood         3
#> 245            wiew            view            view         1
#> 246           noone          no one            none         1
#> 247           noone          no one            noon         1
#> 248           noone          no one           noose         1
#> 249       archetect       architect       architect         3
#> 250   incorruptable   incorruptible   incorruptible         5
#> 251        archtype       archetype       archetype         3
#> 252       componant       component       component         3
#> 253        sargeant        sergeant        sergeant         2
#> 254       precurser       precursor       precursor         3
#> 255         becouse         because         because         2
#> 256       messanger       messenger       messenger         3
#> 257        refernce      references       reference         3
#> 258             gae            game             age         1
#> 259             gae            game             gab         1
#> 260             gae            game             gad         1
#> 261             gae            game            gael         1
#> 262             gae            game             gag         1
#> 263             gae            game            gage         1
#> 264             gae            game            gale         1
#> 265             gae            game            game         1
#> 266             gae            game             gap         1
#> 267             gae            game            gape         1
#> 268             gae            game             gas         1
#> 269             gae            game            gate         1
#> 270             gae            game            gave         1
#> 271             gae            game             gay         1
#> 272             gae            game            gaze         1
#> 273          mounth           month           month         1
#> 274          mounth           month           mount         1
#> 275          mounth           month           mouth         1
#> 276      demolision      demolition      demolition         4
#> 277            bakc            back            back         1
#> 278            bakc            back            bake         1
#> 279      addmission       admission       admission         3
#> 280      colelctive      collective      collective         3
#> 281      elimentary      elementary      alimentary         5
#> 282      elimentary      elementary      elementary         5
#> 283         villify          vilify          vilify         3
#> 284       scientfic      scientific      scientific         4
#> 285    magnificient     magnificent     magnificent         4
#> 286         despict          depict          depict         2
#> 287    inpenetrable    impenetrable    impenetrable         5
#> 288      ancilliary       ancillary       ancillary         4
#> 289         maching        marching         machine         2
#> 290    puritannical     puritanical     puritanical         5
#> 291         electon        electron        election         3
#> 292         electon        electron         elector         3
#> 293         electon        electron        electron         3
#> 294      regularily       regularly      regularity         5
#> 295        standars       standards        standard         2
#> 296         aproval        approval        approval         3
#> 297         ceratin         certain         certain         2
#> 298    presidenital    presidential    presidential         4
#> 299         fianlly         finally         finally         3
#> 300     equivlalent      equivalent      equivalent         4
#> 301     misdameanor     misdemeanor     misdemeanor         4
#> 302           sence           since           fence         1
#> 303           sence           since           hence         1
#> 304           sence           since           pence         1
#> 305           sence           since           sense         1
#> 306           sence           since           since         1
#> 307           stroy           story           story         2
#> 308           stroy           story           stray         1
#> 309           stroy           story           strop         1
#> 310           stroy           story            troy         1
#> 311          howver         however           hover         2
#> 312          howver         however         however         3
#> 313          beween         between         between         2
#> 314          doulbe          double          double         2
#> 315          radify          ratify          ramify         3
#> 316          radify          ratify          ratify         3
#> 317         diamons        diamonds         diamond         3
#> 318       indutrial      industrial      industrial         4
#> 319           maked            made            make         1
#> 320           maked            made          marked         1
#> 321           maked            made           naked         2
#> 322           neice            nice           deice         2
#> 323           neice            nice            nice         1
#> 324           neice            nice           niece         1
#> 325        auxilary       auxiliary       auxiliary         4
#> 326        holliday         holiday         holiday         3
#> 327         calculs        calculus        calculus         3
#> 328      imcomplete      incomplete      incomplete         3
#> 329   indispensible   indispensable   indispensable         5
#> 330         usefull          useful          useful         2
#> 331            fidn            find             fin         1
#> 332            fidn            find            find         1
#> 333            fidn            find            finn         1
#> 334       hesistant        hesitant        hesitant         3
#> 335            thne            then           thane         1
#> 336            thne            then             the         1
#> 337            thne            then            thee         1
#> 338            thne            then            then         1
#> 339            thne            then           thine         1
#> 340            thne            then            tine         1
#> 341            thne            then            tone         1
#> 342            thne            then            tune         1
#> 343            upto           up to            unto         2
#> 344        satelite       satellite       satellite         3
#> 345         omlette        omelette        omelette         3
#> 346       prepatory     preparatory       predatory         4
#> 347       prepatory     preparatory       prefatory         4
#> 348            konw            know            know         1
#> 349           exept          except          except         2
#> 350           exept          except          exempt         2
#> 351           exept          except           exert         2
#> 352       secretery       secretary       secretary         4
#> 353          sumary         summary         summary         3
#> 354    unacompanied   unaccompanied   unaccompanied         5
#> 355         postion        position         portion         2
#> 356         postion        position        position         3
#> 357         postion        position          potion         2
#> 358             gae            gale             age         1
#> 359             gae            gale             gab         1
#> 360             gae            gale             gad         1
#> 361             gae            gale            gael         1
#> 362             gae            gale             gag         1
#> 363             gae            gale            gage         1
#> 364             gae            gale            gale         1
#> 365             gae            gale            game         1
#> 366             gae            gale             gap         1
#> 367             gae            gale            gape         1
#> 368             gae            gale             gas         1
#> 369             gae            gale            gate         1
#> 370             gae            gale            gave         1
#> 371             gae            gale             gay         1
#> 372             gae            gale            gaze         1
#> 373         carreer          career          career         2
#> 374         carreer          career         carrier         3
#> 375        interpet       interpret       interpret         3
#> 376        republis       republics        republic         3
#> 377       noteriety       notoriety       notoriety         5
#> 378         beleive         believe         believe         2
#> 379       gaurantee       guarantee       guarantee         3
#> 380       principly     principally       principle         3
#> 381          trafic         traffic         traffic         2
#> 382          trafic         traffic          tragic         2
#> 383         aircaft        aircraft        aircraft         2
#> 384      wereabouts     whereabouts     whereabouts         3
#> 385          wholey          wholly           whole         1
#> 386          wholey          wholly          wholly         2
#> 387     particulary    particularly      particular         4
#> 388       artillary       artillery       artillery         4
#> 389 incompatability incompatibility incompatibility         7
#> 390          belive         believe           belie         2
#> 391          belive         believe         believe         2
#> 392          belive         believe          belike         2
#> 393        prophacy        prophecy        prophecy         3
#> 394            wass             was             ass         1
#> 395            wass             was            bass         1
#> 396            wass             was            lass         1
#> 397            wass             was            mass         1
#> 398            wass             was            pass         1
#> 399            wass             was             was         1
#> 400            wass             was            wash         1
#> 401            wass             was            wasp         1
#> 402        sedereal        sidereal        sidereal         4
#> 403        acedemic        academic        academic         4
#> 404           greif           grief           grief         1
#> 405       violentce        violence        violence         3
#> 406     archaoelogy     archaeology     archaeology         5
#> 407       pennisula       peninsula       peninsula         4
#> 408           thsoe           those           those         1
#> 409           thsoe           those           throe         1
#> 410           lastr            last            last         1
#> 411      wonderfull       wonderful       wonderful         3
#> 412     consentrate     concentrate     concentrate         3
#> 413    mathematican   mathematician   mathematician         5
#> 414     tempertaure     temperature     temperature         3
#> 415         miltary        military        military         4
#> 416        flourine        fluorine        fluorine         3
#> 417        chracter       character       character         3
#> 418         emition        emission         edition         3
#> 419         emition        emission         emotion         3
#> 420      agricuture     agriculture     agriculture         4
#> 421            pary           party             par         1
#> 422            pary           party            para         2
#> 423            pary           party            pard         1
#> 424            pary           party            pare         1
#> 425            pary           party            park         1
#> 426            pary           party           parry         2
#> 427            pary           party            part         1
#> 428            pary           party           party         2
#> 429            pary           party             pay         1
#> 430            pary           party            pray         1
#> 431            pary           party             pry         1
#> 432            pary           party            vary         2
#> 433            pary           party            wary         2
#> 434            isnt           isn't           isn't         2
#> 435          zeebra           zebra           zebra         2
#> 436        venemous        venomous        venomous         3
#> 437          comany         company         company         3
#> 438            whic           which            chic         1
#> 439            whic           which           which         1
#> 440            whic           which            whig         1
#> 441            whic           which            whim         1
#> 442            whic           which            whip         1
#> 443            whic           which            whit         1
#> 444           wholy            holy            holy         2
#> 445           wholy            holy           whole         1
#> 446           wholy            holy          wholly         2
#> 447           wholy            holy           wooly         2
#> 448      inbalanced      imbalanced      unbalanced         3
#> 449        comittee       committee       committee         3
#> 450      legitamate      legitimate      legitimate         4
#> 451       ancestory        ancestry        ancestor         3
#> 452        juducial        judicial        judicial         3
#> 453         essense         essence         essence         2
#> 454           tiome            time            time         1
#> 455           tiome            time            tome         1
#> 456         pallete         palette          pallet         2
#> 457   contstruction    construction    construction         3
#> 458         remaing       remaining          remain         2
#> 459       parrallel        parallel        parallel         3
#> 460           stlye           style            stye         1
#> 461           stlye           style           style         1
#> 462      translater      translator       translate         2
#> 463        vegtable       vegetable       vegetable         4
#> 464        manuever        maneuver        maneuver         3
#> 465       privelege       privilege       privilege         3
#> 466            ened            need             end         1
#> 467            ened            need            need         1
#> 468      acommodate     accommodate     accommodate         4
#> 469          hvaing          having          having         2
#> 470      propoganda      propaganda      propaganda         4
#> 471      pedestrain      pedestrian      pedestrian         4
#> 472      instuction     instruction     instruction         3
#> 473          achive         achieve         achieve         2
#> 474          achive         achieve          active         2
#> 475          achive         achieve         archive         2
#> 476          achive         achieve           chive         1
#> 477          intial         initial         initial         3
#> 478          abilty         ability         ability         4
#> 479       univerity      university      university         5
#> 480     cannotation     connotation      annotation         4
#> 481     cannotation     connotation     connotation         4
#> 482      temperture     temperature     temperature         3
#> 483            veyr            very            veer         1
#> 484            veyr            very            very         2
#> 485   proclaimation    proclamation    proclamation         4
#> 486            ther           their           ether         2
#> 487            ther           their             her         1
#> 488            ther           their           other         2
#> 489            ther           their             the         1
#> 490            ther           their            thee         1
#> 491            ther           their           their         1
#> 492            ther           their            them         1
#> 493            ther           their            then         1
#> 494            ther           their           there         1
#> 495            ther           their            thew         1
#> 496            ther           their            they         1
#> 497            ther           their            tier         1
#> 498        vacinity        vicinity        vicinity         4
#> 499            loev            love             lev         1
#> 500            loev            love            love         1
#> 501           payed            paid           payee         2
#> 502           payed            paid           payer         2
#> 503        currenly       currently        currency         3
#> 504        vehicule         vehicle         vehicle         3
#> 505       accademic        academic        academic         4
#> 506            halp            help             alp         1
#> 507            halp            help            hale         1
#> 508            halp            help            half         1
#> 509            halp            help            hall         1
#> 510            halp            help            halo         2
#> 511            halp            help            halt         1
#> 512            halp            help             hap         1
#> 513            halp            help            harp         1
#> 514            halp            help            hasp         1
#> 515            halp            help            help         1
#> 516       defintion      definition      definition         4
#> 517       chemicaly      chemically        chemical         3
#> 518       unanymous       unanimous       unanimous         4
#> 519           boook            book            book         1
#> 520           boook            book           brook         1
#> 521           sytem          system            stem         1
#> 522           sytem          system          system         2
#> 523          thiunk           think           think         1
#> 524            wich           which            rich         1
#> 525            wich           which           which         1
#> 526            wich           which            wick         1
#> 527            wich           which           winch         1
#> 528            wich           which            wish         1
#> 529            wich           which           witch         1
#> 530            wich           which            with         1
#> 531            weas             was             was         1
#> 532            weas             was            weak         1
#> 533            weas             was            weal         1
#> 534            weas             was            wean         1
#> 535            weas             was            wear         1
#> 536       ocurrence      occurrence      occurrence         3
#> 537       existance       existence       existence         3
#> 538          strnad          strand          strand         1
#> 539        bouyancy        buoyancy        buoyancy         3
#> 540       withdrawl      withdrawal        withdraw         2
#> 541      pretection      protection      protection         3
#> 542             teh             the             tea         1
#> 543             teh             the             tee         1
#> 544             teh             the             ten         1
#> 545             teh             the             the         1
#> 546        collegue       colleague       colleague         2
#> 547        collegue       colleague         college         2
#> 548         varient         variant         variant         3
#> 549           leanr          leaner            lean         1
#> 550           leanr          leaner           learn         1
#> 551      propietary     proprietary     proprietary         5
#> 552         tomatos        tomatoes          tomato         3
#> 553     settelement      settlement      settlement         3
#> 554        nuisanse        nuisance        nuisance         2
#> 555            vrey            very            prey         1
#> 556            vrey            very            trey         1
#> 557            vrey            very            very         2
#> 558    longitudonal    longitudinal    longitudinal         5
#> 559       portugues      Portuguese      portuguese         3
#> 560            soem            some            poem         2
#> 561            soem            some            seem         1
#> 562            soem            some            some         1
#> 563            soem            some            stem         1
#> 564         acuracy        accuracy        accuracy         4
#> 565           arund          around          around         2
#> 566      assocation     association     association         5
#> 567        territoy       territory       territory         4
#> 568       heirarchy       hierarchy       hierarchy         4
#> 569       pseudonyn       pseudonym       pseudonym         3
#> 570        alcholic       alcoholic       alcoholic         4
#> 571      proceedure       procedure       procedure         3
#> 572         wupport         support         support         2
#> 573          addres          adders         address         2
#> 574         scinece         science         science         2
#> 575         magolia        magnolia        magnolia         3
#> 576         repubic        republic        republic         3
#> 577      equippment       equipment       equipment         3
#> 578       literture      literature      literature         4
#> 579          missle         missile         missile         2
#> 580           agina          angina          angina         3
#> 581           agina          angina          vagina         3
#> 582            hten             the             hen         1
#> 583            hten             the             ten         1
#> 584            hten             the            then         1
#> 585        throught         thought         thought         1
#> 586        throught         thought         through         1
#> 587        mackeral        mackerel        mackerel         3
#> 588       filiament        filament        filament         3
#> 589      hinderance       hindrance       hindrance         2
#> 590     libguistics     linguistics     linguistics         3
#> 591       privalege       privilege       privilege         3
#> 592       propogate       propagate       propagate         3
#> 593         arogent        arrogant          argent         2
#> 594          husban         husband         husband         2
#> 595     diminuitive      diminutive      diminutive         4
#> 596          staion         station           stain         1
#> 597          staion         station         station         2
#> 598       addoptive        adoptive        adoptive         3
#> 599           layed            laid           layer         2
#> 600      seperation      separation      separation         4
#> 601           seing          seeing           being         2
#> 602           seing          seeing           seine         1
#> 603           seing          seeing          sewing         2
#> 604           seing          seeing            sing         1
#> 605           seing          seeing           sling         1
#> 606           seing          seeing           sting         1
#> 607           seing          seeing           swing         1
#> 608       valueable        valuable        valuable         4
#> 609          futher         further          father         2
#> 610          futher         further         further         2
#> 611           timne            time            time         1
#> 612           timne            time            tine         1
#> 613     consitution    constitution    constitution         4
#> 614      apropriate     appropriate     appropriate         4
#> 615      naturually       naturally       naturally         4
#> 616           assit          assist           asset         2
#> 617           assit          assist          assist         2
#> 618          doesnt         doesn't         doesn't         2
#> 619        generaly       generally         general         3
#> 620         fertily       fertility         fertile         2
#> 621      indpendent     independent     independent         4
#> 622            alot           a lot            alit         2
#> 623            alot           a lot           allot         2
#> 624            alot           a lot            aloe         2
#> 625            alot           a lot           aloft         2
#> 626            alot           a lot            alto         2
#> 627            alot           a lot            blot         1
#> 628            alot           a lot            clot         1
#> 629            alot           a lot             lot         1
#> 630            alot           a lot            plot         1
#> 631            alot           a lot            slot         1
#> 632      principial       principal       principal         3
#> 633      sepulchure       sepulcher       sepulchre         3
#> 634    possiblility     possibility     possibility         5
#> 635            wiht            with            whit         1
#> 636            wiht            with           wight         1
#> 637            wiht            with            wilt         1
#> 638            wiht            with             wit         1
#> 639            wiht            with            with         1
#> 640         cathlic        catholic        catholic         3
#> 641     resemblence     resemblance     resemblance         3
#> 642         perhasp         perhaps         perhaps         2
#> 643       rennovate        renovate        renovate         3
#> 644        morrocco         morocco         morocco         3
#> 645         relient         reliant          relent         2
#> 646     protaganist     protagonist     protagonist         4
#> 647          indite          indict          incite         2
#> 648          indite          indict          indite         2
#> 649          indite          indict          invite         2
#> 650         remeber        remember        remember         3
#> 651      acceptence      acceptance      acceptance         3
#> 652          specif        specific          specie         2
#> 653          specif        specific         specify         3
#> 654            tahn            than             tan         1
#> 655            tahn            than            tarn         1
#> 656            tahn            than            than         1
#> 657       passerbys       passersby        passerby         3
#> 658       surrended      surrounded       surrender         3
#> 659     hierarcical    hierarchical    hierarchical         5
#> 660           higer          higher           tiger         2
#> 661           oging           going           going         2
#> 662    commemmorate     commemorate     commemorate         4
#> 663          betwen         between         between         2
#> 664       monestary        monetary        monetary         4
#> 665        embarass       embarrass       embarrass         3
#> 666   protruberance    protuberance    protuberance         4
#> 667   reconaissance  reconnaissance  reconnaissance         4
#> 668       phongraph      phonograph      phonograph         3
#> 669       restraunt      restaurant       restraint         2
#> 670         movment        movement          moment         2
#> 671           willk            will            will         1
#> 672         potrait        portrait        portrait         2
#> 673     neverthless    nevertheless    nevertheless         4
#> 674           htere           there            here         1
#> 675           htere           there           there         1
#> 676        repetion      repetition       repletion         3
#> 677         pratice        practice        practice         2
#> 678       vigilence       vigilance       vigilance         3
#> 679     independant     independent     independent         4
#> 680         childen        children         chidden         2
#> 681         childen        children        children         2
#> 682       opression      oppression      oppression         3
#> 683        manouver        maneuver        maneuver         3
#> 684    preocupation   preoccupation   preoccupation         5
#> 685      alledgedly       allegedly       allegedly         4
#> 686         emision        emission         elision         3
#> 687       exagerate      exaggerate      exaggerate         4
#> 688         usefuly        usefully          useful         2
#> 689       inmigrant       immigrant       immigrant         3
#> 690           thrid           third           third         1
#> 691       accension       accession       accession         3
#> 692       accension       accession       ascension         3
#> 693           boaut           about           boast         1
#> 694           boaut           about            boat         1
#> 695           boaut           about            bout         1
#> 696    irresistable    irresistible    irresistible         5
#> 697       thoughout      throughout      throughout         2
#> 698     charasmatic     charismatic     charismatic         4
#> 699          arised           arose           arise         2
#> 700     constuction    construction    construction         3
#> 701          refect         reflect          defect         2
#> 702          refect         reflect         prefect         2
#> 703          refect         reflect         reflect         2
#> 704          refect         reflect          reject         2
#> 705           sinse           sines           rinse         1
#> 706           sinse           sines           sense         1
#> 707           sinse           sines           since         1
#> 708           sinse           sines           singe         1
#> 709        naturual         natural         natural         3
#> 710       plebicite      plebiscite      plebiscite         3
#> 711        againnst         against         against         2
#> 712         positon        positron        position         3
#> 713         positon        positron        positron         3
#> 714            eyar            eyas             ear         1
#> 715            eyar            eyas            year         1
#> 716      reponsible     responsible     responsible         4
#> 717        obession       obsession       obsession         3
#> 718         consern         concern         concern         2
#> 719        quantaty        quantity        quantity         3
#> 720      substatial     substantial     substantial         3
#> 721           fouth          fourth           forth         1
#> 722           fouth          fourth          fourth         1
#> 723           fouth          fourth           mouth         1
#> 724           fouth          fourth           south         1
#> 725           fouth          fourth           youth         1
#> 726        embargos       embargoes         embargo         3
#> 727            vyer            very            veer         1
#> 728            vyer            very            vier         2
#> 729           coudl           cloud           could         1
#> 730           chuch          church           chuck         1
#> 731           chuch          church          church         1
#> 732          arival         arrival         arrival         3
#> 733          arival         arrival           rival         2
#> 734      demorcracy       democracy       democracy         4
#> 735      commerical      commercial      commercial         3
#> 736        mamalian       mammalian       mammalian         4
#> 737       artifical      artificial      artificial         4
#> 738           swaer           swear           swear         1
#> 739            thta            that            that         1
#> 740        adecuate        adequate        adequate         3
#> 741        apparant        apparent        apparent         3
#> 742           soudn           sound           sound         1
#> 743          tourch           torch           torch         1
#> 744          tourch           torch           touch         1
#> 745        calander        calendar        colander         3
#> 746           whihc           which           which         1
#> 747          sepina        subpoena           sepia         3
#> 748           crowm           crown            crow         1
#> 749           crowm           crown           crowd         1
#> 750           crowm           crown           crown         1
#> 751    independance    independence    independence         4
#> 752            fiel           field            feel         1
#> 753            fiel           field             fie         1
#> 754            fiel           field            fief         1
#> 755            fiel           field           field         1
#> 756            fiel           field            file         1
#> 757            fiel           field            fill         1
#> 758            fiel           field            fuel         2
#> 759            fiel           field            riel         2
#> 760        aparment       apartment       apartment         3
```

### Analyzing match quality

Check how many misspellings we successfully matched:

``` r
# Count unique misspellings matched
joined %>%
  count(misspelling, correct)
#>         misspelling         correct  n
#> 1            abilty         ability  1
#> 2         accademic        academic  1
#> 3          accademy         academy  1
#> 4         accension       accession  2
#> 5        acceptence      acceptance  1
#> 6          acedemic        academic  1
#> 7            achive         achieve  4
#> 8        acommodate     accommodate  1
#> 9           acuracy        accuracy  1
#> 10       addmission       admission  1
#> 11        addoptive        adoptive  1
#> 12           addres          adders  1
#> 13         adecuate        adequate  1
#> 14              adn             and  6
#> 15         affilate       affiliate  1
#> 16         againnst         against  1
#> 17            agina          angina  2
#> 18       agricuture     agriculture  1
#> 19          aircaft        aircraft  1
#> 20         alcholic       alcoholic  1
#> 21       alledgedly       allegedly  1
#> 22       allegedely       allegedly  1
#> 23      alltogether      altogether  1
#> 24             alot           a lot 10
#> 25            alsot            also  2
#> 26       analogeous       analogous  1
#> 27        ancestory        ancestry  1
#> 28       ancilliary       ancillary  1
#> 29        anomolous       anomalous  1
#> 30         aparment       apartment  1
#> 31         apparant        apparent  1
#> 32      appeareance      appearance  1
#> 33       appearence      appearance  1
#> 34       apropriate     appropriate  1
#> 35          aproval        approval  1
#> 36         arbitary       arbitrary  1
#> 37      archaoelogy     archaeology  1
#> 38        archetect       architect  1
#> 39     archetecture    architecture  1
#> 40       archictect       architect  1
#> 41     architectual   architectural  1
#> 42         archtype       archetype  1
#> 43           arised           arose  1
#> 44           arival         arrival  2
#> 45          arogent        arrogant  1
#> 46          artcile         article  1
#> 47        artifical      artificial  1
#> 48        artillary       artillery  1
#> 49            arund          around  1
#> 50            assit          assist  2
#> 51       assocation     association  1
#> 52           aswell         as well  1
#> 53       attendence      attendance  1
#> 54         attitide        attitude  1
#> 55       automibile      automobile  1
#> 56         auxilary       auxiliary  1
#> 57       availaible       available  1
#> 58             bakc            back  2
#> 59      beastiality      bestiality  1
#> 60          becouse         because  1
#> 61           bedore          before  1
#> 62           beggin         begging  2
#> 63          beleive         believe  1
#> 64           belive         believe  3
#> 65      bellweather      bellwether  1
#> 66           beteen         between  1
#> 67           betwen         between  1
#> 68           beween         between  1
#> 69            boaut           about  3
#> 70            boook            book  2
#> 71            borke           broke  3
#> 72          boundry        boundary  2
#> 73         bouyancy        buoyancy  1
#> 74          brethen        brethren  1
#> 75           broady         broadly  1
#> 76        busineses        business  1
#> 77        busineses      businesses  1
#> 78         calander        calendar  1
#> 79         calander        calender  1
#> 80          calculs        calculus  1
#> 81          cannnot          cannot  1
#> 82      cannotation     connotation  2
#> 83          capible         capable  1
#> 84          carnige        Carnegie  1
#> 85          carreer          career  2
#> 86          casuing         causing  1
#> 87          cathlic        catholic  1
#> 88         cemetary        cemetery  1
#> 89          ceratin         certain  1
#> 90      charasmatic     charismatic  1
#> 91        chemicaly      chemically  1
#> 92          childen        children  2
#> 93         chracter       character  1
#> 94            chuch          church  2
#> 95       colelctive      collective  1
#> 96         collegue       colleague  2
#> 97          comando        commando  1
#> 98           comany         company  1
#> 99         comittee       committee  1
#> 100    commemmorate     commemorate  1
#> 101      commerical      commercial  1
#> 102       componant       component  1
#> 103       concensus       consensus  1
#> 104         confids        confides  1
#> 105    congresional   congressional  1
#> 106      conjecutre      conjecture  1
#> 107     consentrate     concentrate  1
#> 108         consern         concern  1
#> 109     consituency    constituency  1
#> 110     consitution    constitution  1
#> 111   consttruction    construction  1
#> 112     constuction    construction  1
#> 113     contempoary    contemporary  1
#> 114   contstruction    construction  1
#> 115           coudl           cloud  1
#> 116           crowm           crown  3
#> 117        currenly       currently  1
#> 118       cyclinder        cylinder  1
#> 119       defintion      definition  1
#> 120      demolision      demolition  1
#> 121      demorcracy       democracy  1
#> 122     deparmental    departmental  1
#> 123           deram           dream  2
#> 124         despict          depict  1
#> 125         diamons        diamonds  1
#> 126            diea            idea  5
#> 127           diety           deity  4
#> 128     diminuitive      diminutive  1
#> 129        diversed         diverse  1
#> 130          doesnt         doesn't  1
#> 131           donig           doing  1
#> 132          doulbe          double  1
#> 133           earnt          earned  1
#> 134         electic        electric  2
#> 135         electon        electron  3
#> 136      elimentary      elementary  2
#> 137        embarass       embarrass  1
#> 138        embargos       embargoes  1
#> 139         emision        emission  1
#> 140         emiting        emitting  1
#> 141         emition        emission  2
#> 142         emition         emotion  2
#> 143         emporer         emperor  1
#> 144         enameld        enameled  1
#> 145            ened            need  2
#> 146         epsiode         episode  1
#> 147      equippment       equipment  1
#> 148     equivlalent      equivalent  1
#> 149        esential       essential  1
#> 150         essense         essence  1
#> 151       exagerate      exaggerate  1
#> 152          exampt          exempt  1
#> 153         excange        exchange  1
#> 154      excecution       execution  1
#> 155           exept          except  3
#> 156          exinct         extinct  1
#> 157       existance       existence  1
#> 158       existince       existence  1
#> 159          extint         extinct  2
#> 160            eyar            eyas  2
#> 161         fertily       fertility  1
#> 162         fianlly         finally  1
#> 163            fidn            find  3
#> 164            fiel           field  8
#> 165       filiament        filament  1
#> 166           fleed            fled  6
#> 167        flourine        fluorine  1
#> 168           fouth          fourth  5
#> 169          futher         further  2
#> 170             gae            gale 15
#> 171             gae            game 15
#> 172        garantee       guarantee  2
#> 173       gaurantee       guarantee  1
#> 174        generaly       generally  1
#> 175          glight          flight  6
#> 176     govermental    governmental  1
#> 177           greif           grief  1
#> 178            halp            help 10
#> 179       heirarchy       hierarchy  1
#> 180       hesistant        hesitant  1
#> 181     hierarcical    hierarchical  1
#> 182           higer          higher  1
#> 183      hinderance       hindrance  1
#> 184        holliday         holiday  1
#> 185           housr           hours  2
#> 186          howver         however  2
#> 187            hten             the  3
#> 188           htere           there  2
#> 189          husban         husband  1
#> 190          hvaing          having  1
#> 191           idaes           ideas  1
#> 192      identicial       identical  1
#> 193      imcomplete      incomplete  1
#> 194      inbalanced      imbalanced  1
#> 195 incompatability incompatibility  1
#> 196   incorruptable   incorruptible  1
#> 197    independance    independence  1
#> 198     independant     independent  1
#> 199   indispensible   indispensable  1
#> 200          indite          indict  3
#> 201      indpendent     independent  1
#> 202       indutrial      industrial  1
#> 203      inevitible      inevitable  1
#> 204   infilitration    infiltration  1
#> 205      informtion     information  1
#> 206     initiaitive      initiative  1
#> 207       inmigrant       immigrant  1
#> 208    inpenetrable    impenetrable  1
#> 209      instuction     instruction  1
#> 210        interpet       interpret  1
#> 211        intevene       intervene  1
#> 212          intial         initial  1
#> 213    irresistable    irresistible  1
#> 214            isnt           isn't  1
#> 215        juducial        judicial  1
#> 216            konw            know  1
#> 217           lastr            last  1
#> 218           layed            laid  1
#> 219           leage          league  4
#> 220           leanr          leaner  2
#> 221      legitamate      legitimate  1
#> 222           levle           level  2
#> 223         liasion         liaison  1
#> 224          liason         liaison  1
#> 225     libguistics     linguistics  1
#> 226      likelyhood      likelihood  1
#> 227        litature      literature  1
#> 228       literture      literature  1
#> 229            loev            love  2
#> 230    longitudonal    longitudinal  1
#> 231         maching        marching  1
#> 232        mackeral        mackerel  1
#> 233    magnificient     magnificent  1
#> 234         magolia        magnolia  1
#> 235           maked            made  3
#> 236           mamal          mammal  2
#> 237        mamalian       mammalian  1
#> 238        manouver        maneuver  1
#> 239        manuever        maneuver  1
#> 240         mariage        marriage  1
#> 241    mathematican   mathematician  1
#> 242       messanger       messenger  1
#> 243           mileu          milieu  2
#> 244         miltary        military  1
#> 245     misdameanor     misdemeanor  1
#> 246          missle         missile  1
#> 247            moil           mohel  9
#> 248            moil            soil  9
#> 249       monestary       monastery  1
#> 250       monestary        monetary  1
#> 251        morrocco         morocco  1
#> 252          mounth           month  3
#> 253         movment        movement  1
#> 254        naturual         natural  1
#> 255      naturually       naturally  1
#> 256       negociate       negotiate  1
#> 257           neice            nice  3
#> 258           neice           niece  3
#> 259     neverthless    nevertheless  1
#> 260           noone          no one  3
#> 261        notabley         notably  1
#> 262       noteriety       notoriety  1
#> 263        nuisanse        nuisance  1
#> 264       nutritent        nutrient  1
#> 265        obession       obsession  1
#> 266         ocuntry         country  1
#> 267       ocurrence      occurrence  1
#> 268           oging           going  1
#> 269         omlette        omelette  1
#> 270        opponant        opponent  1
#> 271       opression      oppression  1
#> 272       opthalmic      ophthalmic  1
#> 273            owrk            work  1
#> 274         pallete         palette  1
#> 275         pamplet        pamphlet  1
#> 276       parisitic       parasitic  1
#> 277       parrallel        parallel  1
#> 278     particulary    particularly  1
#> 279            pary           party 13
#> 280       passerbys       passersby  1
#> 281           payed            paid  2
#> 282      pedestrain      pedestrian  1
#> 283       pennisula       peninsula  1
#> 284         perhasp         perhaps  1
#> 285   perpindicular   perpendicular  1
#> 286       phongraph      phonograph  1
#> 287       plausable       plausible  1
#> 288       plebicite      plebiscite  1
#> 289        polution       pollution  1
#> 290       portugues      Portuguese  1
#> 291         positon        positron  2
#> 292       possibile        possible  1
#> 293    possiblility     possibility  1
#> 294         postion        position  3
#> 295         potrait        portrait  1
#> 296         pratice        practice  1
#> 297       precurser       precursor  1
#> 298     preferrably      preferably  1
#> 299          preiod          period  1
#> 300    preocupation   preoccupation  1
#> 301       prepatory     preparatory  2
#> 302    presidenital    presidential  1
#> 303     prespective     perspective  3
#> 304      pretection      protection  1
#> 305      principial       principal  1
#> 306       principly     principally  1
#> 307       privalege       privilege  1
#> 308       privelege       privilege  1
#> 309       privilage       privilege  1
#> 310         probaly        probably  1
#> 311        proceded       proceeded  1
#> 312       proceding       preceding  1
#> 313      proceedure       procedure  1
#> 314   proclaimation    proclamation  1
#> 315       professer       professor  1
#> 316        prophacy        prophecy  1
#> 317      propietary     proprietary  1
#> 318      propoganda      propaganda  1
#> 319       propogate       propagate  1
#> 320     protaganist     protagonist  1
#> 321   protruberance    protuberance  1
#> 322       pseudonyn       pseudonym  1
#> 323       psycology      psychology  1
#> 324          pumkin         pumpkin  1
#> 325    puritannical     puritanical  1
#> 326        quantaty        quantity  1
#> 327          radify          ratify  2
#> 328        rebounce         rebound  1
#> 329   reconaissance  reconnaissance  1
#> 330          refect         reflect  4
#> 331        refernce       reference  1
#> 332        refernce      references  1
#> 333      regularily       regularly  1
#> 334         relient         reliant  1
#> 335         remaing       remaining  1
#> 336         remeber        remember  1
#> 337       rennovate        renovate  1
#> 338      repatition     repartition  1
#> 339        repetion      repetition  1
#> 340      reponsible     responsible  1
#> 341         repubic        republic  1
#> 342        republis       republics  1
#> 343         repulic        republic  1
#> 344       resaurant      restaurant  1
#> 345     resemblence     resemblance  1
#> 346     responisble     responsible  1
#> 347       restraunt      restaurant  1
#> 348         returnd        returned  1
#> 349           rised            rose  3
#> 350          runing         running  1
#> 351        sargeant        sergeant  1
#> 352        satelite       satellite  1
#> 353       scientfic      scientific  1
#> 354         scinece         science  1
#> 355           scoll          scroll  4
#> 356       secretery       secretary  1
#> 357        sedereal        sidereal  1
#> 358           seing          seeing  7
#> 359           sence           since  5
#> 360         sensure         censure  1
#> 361        seperate        separate  1
#> 362      seperation      separation  1
#> 363          sepina        subpoena  1
#> 364      sepulchure       sepulcher  1
#> 365     settelement      settlement  1
#> 366         sideral        sidereal  1
#> 367           sinse           since  4
#> 368           sinse           sines  4
#> 369            soem            some  4
#> 370           soley          solely  3
#> 371           soudn           sound  1
#> 372          specif        specific  2
#> 373          staion         station  2
#> 374        standars       standards  1
#> 375           stlye           style  2
#> 376          strnad          strand  1
#> 377           stroy           story  4
#> 378       subsquent      subsequent  1
#> 379        substace       substance  1
#> 380      substatial     substantial  1
#> 381          sumary         summary  1
#> 382         suposed        supposed  1
#> 383         supress        suppress  1
#> 384       surrended      surrounded  1
#> 385           swaer           swear  1
#> 386      symetrical     symmetrical  1
#> 387        synphony        symphony  1
#> 388           sytem          system  2
#> 389            tahn            than  3
#> 390      technitian      technician  1
#> 391             teh             the  4
#> 392     tempertaure     temperature  1
#> 393      temperture     temperature  1
#> 394         tenacle        tentacle  2
#> 395        territoy       territory  1
#> 396           theif           thief  2
#> 397            ther           their 12
#> 398           theri           their  2
#> 399          thiunk           think  1
#> 400            thne            then  8
#> 401       thoughout      throughout  1
#> 402           thrid           third  1
#> 403        throught         thought  2
#> 404            thru         through  2
#> 405           thsoe           those  2
#> 406            thta            that  1
#> 407            tiem            time  6
#> 408           timne            time  2
#> 409           tiome            time  2
#> 410         tomatos        tomatoes  1
#> 411           tothe          to the  2
#> 412          tourch           torch  2
#> 413          trafic         traffic  2
#> 414      translater      translator  1
#> 415         tremelo         tremolo  1
#> 416    unacompanied   unaccompanied  1
#> 417       unanymous       unanimous  1
#> 418     undreground     underground  1
#> 419      unecessary     unnecessary  1
#> 420        univeral       universal  1
#> 421       univerity      university  1
#> 422       univesity      university  1
#> 423            upto           up to  1
#> 424         usefull          useful  1
#> 425         usefuly        usefully  1
#> 426        vacinity        vicinity  1
#> 427         valuble        valuable  2
#> 428       valueable        valuable  1
#> 429         varient         variant  1
#> 430        vegtable       vegetable  1
#> 431        vehicule         vehicle  1
#> 432        venemous        venomous  1
#> 433        vengence       vengeance  1
#> 434            veyr            very  2
#> 435       vigilence       vigilance  1
#> 436         villify          vilify  1
#> 437       vincinity        vicinity  1
#> 438       violentce        violence  1
#> 439         volumne          volume  1
#> 440            vrey            very  3
#> 441            vyer            very  2
#> 442            wass             was  8
#> 443            weas             was  5
#> 444      wereabouts     whereabouts  1
#> 445           whcih           which  1
#> 446        wherease         whereas  1
#> 447            whic           which  6
#> 448           whihc           which  1
#> 449           whith            with  5
#> 450          wholey          wholly  2
#> 451           wholy            holy  4
#> 452            wich           which  7
#> 453           wierd           weird  2
#> 454            wiew            view  1
#> 455            wiht            with  5
#> 456           willk            will  1
#> 457       withdrawl      withdrawal  1
#> 458            wnat            want  3
#> 459      wonderfull       wonderful  1
#> 460           wroet           wrote  1
#> 461         wupport         support  1
#> 462          zeebra           zebra  1

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
#>            misspelling              correct word syllables
#> 1            Sanhedrim            Sanhedrin <NA>        NA
#> 2        consicousness        consciousness <NA>        NA
#> 3           repubicans          republicans <NA>        NA
#> 4             comitted            committed <NA>        NA
#> 5            emmisions            emissions <NA>        NA
#> 6             acquited            acquitted <NA>        NA
#> 7        decompositing          decomposing <NA>        NA
#> 8             decieved             deceived <NA>        NA
#> 9            asociated           associated <NA>        NA
#> 10         commonweath         commonwealth <NA>        NA
#> 11           supressed           suppressed <NA>        NA
#> 12            soliders             soldiers <NA>        NA
#> 13        aproximately        approximately <NA>        NA
#> 14               htikn                think <NA>        NA
#> 15               peoms                poems <NA>        NA
#> 16         territorist            terrorist <NA>        NA
#> 17         incorprates         incorporates <NA>        NA
#> 18          Missisippi          Mississippi <NA>        NA
#> 19            lazyness             laziness <NA>        NA
#> 20       constituional       constitutional <NA>        NA
#> 21            issueing              issuing <NA>        NA
#> 22       accomodations       accommodations <NA>        NA
#> 23            verisons             versions <NA>        NA
#> 24             alusion             illusion <NA>        NA
#> 25              lonley               lonely <NA>        NA
#> 26         unsuprising         unsurprising <NA>        NA
#> 27          succesfuly         successfully <NA>        NA
#> 28             witheld             withheld <NA>        NA
#> 29          motiviated            motivated <NA>        NA
#> 30          resistence           resistance <NA>        NA
#> 31            agrieved            aggrieved <NA>        NA
#> 32  anthromorphization anthropomorphization <NA>        NA
#> 33       incidentially         incidentally <NA>        NA
#> 34            tempalte             template <NA>        NA
#> 35         techiniques           techniques <NA>        NA
#> 36            tyranies            tyrannies <NA>        NA
#> 37       overshaddowed         overshadowed <NA>        NA
#> 38        ansalisation         nasalisation <NA>        NA
#> 39       enviornmental        environmental <NA>        NA
#> 40            bradcast            broadcast <NA>        NA
#> 41          abreviated          abbreviated <NA>        NA
#> 42           noticably           noticeably <NA>        NA
#> 43            sppeches             speeches <NA>        NA
#> 44          occurences          occurrences <NA>        NA
#> 45         effeciently          efficiently <NA>        NA
#> 46         poportional         proportional <NA>        NA
#> 47       unsuprisingly       unsurprisingly <NA>        NA
#> 48              Malcom              Malcolm <NA>        NA
#> 49         causalities           casualties <NA>        NA
#> 50             futhroc              futhark <NA>        NA
#> 51        reorganision       reorganisation <NA>        NA
#> 52           ocurrance           occurrence <NA>        NA
#> 53            humoural              humoral <NA>        NA
#> 54        maintainence          maintenance <NA>        NA
#> 55           rebllions           rebellions <NA>        NA
#> 56           sountrack           soundtrack <NA>        NA
#> 57            differnt            different <NA>        NA
#> 58          methaphors            metaphors <NA>        NA
#> 59        disemination        dissemination <NA>        NA
#> 60           dimenions           dimensions <NA>        NA
#> 61          dependancy           dependency <NA>        NA
#> 62             hapened             happened <NA>        NA
#> 63         breakthough         breakthrough <NA>        NA
#> 64         volonteered          volunteered <NA>        NA
#> 65              fufill              fulfill <NA>        NA
#> 66             aeriels              aerials <NA>        NA
#> 67           repubican           republican <NA>        NA
#> 68       overwheliming         overwhelming <NA>        NA
#> 69          asssassans            assassins <NA>        NA
#> 70       juristictions        jurisdictions <NA>        NA
#> 71           percepted            perceived <NA>        NA
#> 72       anthropolgist       anthropologist <NA>        NA
#> 73             hosited              hoisted <NA>        NA
#> 74             siezing              seizing <NA>        NA
#> 75           maltesian              Maltese <NA>        NA
#> 76       volounteering         volunteering <NA>        NA
#> 77            fulfiled            fulfilled <NA>        NA
#> 78         mischevious          mischievous <NA>        NA
#> 79           goverance           governance <NA>        NA
#> 80           acheiving            achieving <NA>        NA
#> 81          immidately          immediately <NA>        NA
#> 82         constituant          constituent <NA>        NA
#> 83        manouverable         maneuverable <NA>        NA
#> 84    architechturally      architecturally <NA>        NA
#> 85          controling          controlling <NA>        NA
#> 86       transmissable        transmissible <NA>        NA
#> 87        predomiantly        predominately <NA>        NA
#> 88             meranda              veranda <NA>        NA
#> 89            harrases             harasses <NA>        NA
#> 90       consenquently         consequently <NA>        NA
#> 91            leibnitz              leibniz <NA>        NA
#> 92          councellor            councilor <NA>        NA
#> 93          polinators          pollinators <NA>        NA
#> 94          substracts            subtracts <NA>        NA
#> 95           atributes           attributes <NA>        NA
#> 96            noteably              notably <NA>        NA
#> 97          messenging            messaging <NA>        NA
#> 98         deteoriated         deteriorated <NA>        NA
#> 99        assymetrical         asymmetrical <NA>        NA
#> 100         fortelling          foretelling <NA>        NA
#> 101       completetion           completion <NA>        NA
#> 102      archaelogists       archaeologists <NA>        NA
#> 103     restauranteurs        restaurateurs <NA>        NA
#> 104         agreeement            agreement <NA>        NA
#> 105         descisions            decisions <NA>        NA
#> 106           greatful             grateful <NA>        NA
#> 107             densly              densely <NA>        NA
#> 108          millepede            millipede <NA>        NA
#> 109         conciously          consciously <NA>        NA
#> 110           diferent            different <NA>        NA
#> 111      commuications       communications <NA>        NA
#> 112    manisfestations       manifestations <NA>        NA
#> 113           baceause              because <NA>        NA
#> 114         inofficial           unofficial <NA>        NA
#> 115          ciriculum           curriculum <NA>        NA
#> 116        resignement           resignment <NA>        NA
#> 117       charaterized        characterized <NA>        NA
#> 118       unsucessfull         unsuccessful <NA>        NA
#> 119             acused              accused <NA>        NA
#> 120          referiang            referring <NA>        NA
#> 121          offically           officially <NA>        NA
#> 122      approximitely        approximately <NA>        NA
#> 123         beleagured          beleaguered <NA>        NA
#> 124         resteraunt           restaurant <NA>        NA
#> 125          unwieldly             unwieldy <NA>        NA
#> 126          scaleable             scalable <NA>        NA
#> 127         realitvely           relatively <NA>        NA
#> 128            poweful             powerful <NA>        NA
#> 129          origanaly           originally <NA>        NA
#> 130        successully         successfully <NA>        NA
#> 131        insitutions         institutions <NA>        NA
#> 132            relized             realised <NA>        NA
#> 133         Fransiscan           Franciscan <NA>        NA
#> 134          efficency           efficiency <NA>        NA
#> 135        particuarly         particularly <NA>        NA
#> 136          atheneans            athenians <NA>        NA
#> 137          recidents            residents <NA>        NA
#> 138            muscels              mussels <NA>        NA
#> 139          excellant            excellent <NA>        NA
#> 140         equivelant           equivalent <NA>        NA
#> 141       culiminating          culminating <NA>        NA
#> 142            breifly              briefly <NA>        NA
#> 143              adres              address <NA>        NA
#> 144       matheticians       mathematicians <NA>        NA
#> 145          admitedly           admittedly <NA>        NA
#> 146            shoudln            shouldn't <NA>        NA
#> 147       occassionaly         occasionally <NA>        NA
#> 148           Michagan             Michigan <NA>        NA
#> 149       assasination        assassination <NA>        NA
#> 150         marketting            marketing <NA>        NA
#> 151      opthamologist      ophthalmologist <NA>        NA
#> 152          vaguaries             vagaries <NA>        NA
#> 153         practioner         practitioner <NA>        NA
#> 154           aquiring            acquiring <NA>        NA
#> 155        particually         particularly <NA>        NA
#> 156           forbiden            forbidden <NA>        NA
#> 157          Carmalite            Carmelite <NA>        NA
#> 158            relized             realized <NA>        NA
#> 159         terriories          territories <NA>        NA
#> 160         assoicated           associated <NA>        NA
#> 161             mileau               milieu <NA>        NA
#> 162         ocasionaly         occasionally <NA>        NA
#> 163         techicians          technicians <NA>        NA
#> 164       UnitesStates         UnitedStates <NA>        NA
#> 165           ocassion             occasion <NA>        NA
#> 166          homestate           home state <NA>        NA
#> 167        occurrances          occurrences <NA>        NA
#> 168        philosphies         philosophies <NA>        NA
#> 169          garantees           guarantees <NA>        NA
#> 170               kwno                 know <NA>        NA
#> 171          santioned           sanctioned <NA>        NA
#> 172         reportadly           reportedly <NA>        NA
#> 173          overthere           over there <NA>        NA
#> 174            helpped               helped <NA>        NA
#> 175   manoeuverability      maneuverability <NA>        NA
#> 176         deriviated              derived <NA>        NA
#> 177         doccuments            documents <NA>        NA
#> 178            intered             interred <NA>        NA
#> 179       caracterized        characterized <NA>        NA
#> 180         exculsivly          exclusively <NA>        NA
#> 181        transfering         transferring <NA>        NA
#> 182              gogin                going <NA>        NA
#> 183           pomotion            promotion <NA>        NA
#> 184             quizes              quizzes <NA>        NA
#> 185           funguses                fungi <NA>        NA
#> 186        incomptable         incompatible <NA>        NA
#> 187           poluting            polluting <NA>        NA
#> 188    archetecturally      architecturally <NA>        NA
#> 189         poulations          populations <NA>        NA
#> 190       authoritiers          authorities <NA>        NA
#> 191            meranda              Miranda <NA>        NA
#> 192          proffesed            professed <NA>        NA
#> 193            exliled               exiled <NA>        NA
#> 194       dissobediant          disobedient <NA>        NA
#> 195            vasalls              vassals <NA>        NA
#> 196          obstruced           obstructed <NA>        NA
#> 197          condemmed            condemned <NA>        NA
#> 198         precentage           percentage <NA>        NA
#> 199        charachters           characters <NA>        NA
#> 200          controlls             controls <NA>        NA
#> 201           formelly             formerly <NA>        NA
#> 202       accomadation        accommodation <NA>        NA
#> 203         embarrased          embarrassed <NA>        NA
#> 204      dissobediance         disobedience <NA>        NA
#> 205            suceeds             succeeds <NA>        NA
#> 206        frequentily           frequently <NA>        NA
#> 207            seaches             searches <NA>        NA
#> 208         comissions          commissions <NA>        NA
#> 209          shadasloo             shadaloo <NA>        NA
#> 210         insistance           insistence <NA>        NA
#> 211          dependant            dependent <NA>        NA
#> 212           sophmore            sophomore <NA>        NA
#> 213       incorportaed         incorporated <NA>        NA
#> 214         electricly         electrically <NA>        NA
#> 215         councellor            counselor <NA>        NA
#> 216      comminication        communication <NA>        NA
#> 217        assertation            assertion <NA>        NA
#> 218          syphyllis             syphilis <NA>        NA
#> 219      dissagreement         disagreement <NA>        NA
#> 220          crticised           criticised <NA>        NA
#> 221     unforetunately        unfortunately <NA>        NA
#> 222       Rockerfeller          Rockefeller <NA>        NA
#> 223           commedic              comedic <NA>        NA
#> 224              unsed               unused <NA>        NA
#> 225         sucesfully         successfully <NA>        NA
#> 226         beautyfull            beautiful <NA>        NA
#> 227        catagorized          categorized <NA>        NA
#> 228           mchanics            mechanics <NA>        NA
#> 229          Phonecian           Phoenecian <NA>        NA
#> 230           Cambrige            Cambridge <NA>        NA
#> 231      commmemorated         commemorated <NA>        NA
#> 232           Carnagie             Carnegie <NA>        NA
#> 233         equilibium          equilibrium <NA>        NA
#> 234          contendor            contender <NA>        NA
#> 235        harrasments          harassments <NA>        NA
#> 236       traditionaly        traditionally <NA>        NA
#> 237          reedeming            redeeming <NA>        NA
#> 238           exicting             exciting <NA>        NA
#> 239        repectively         respectively <NA>        NA
#> 240           bigining            beginning <NA>        NA
#> 241             aleged              alleged <NA>        NA
#> 242          exerbated          exacerbated <NA>        NA
#> 243           Europian             European <NA>        NA
#> 244       substraction          subtraction <NA>        NA
#> 245            liasons             liaisons <NA>        NA
#> 246           intially            initially <NA>        NA
#> 247            intered             interned <NA>        NA
#> 248         corridoors            corridors <NA>        NA
#> 249        difficulity           difficulty <NA>        NA
#> 250         criticists              critics <NA>        NA
#> 251           meerkrat              meerkat <NA>        NA
#> 252      compatability        compatibility <NA>        NA
#> 253         apperances          appearances <NA>        NA
#> 254          beleiving            believing <NA>        NA
#> 255          relativly           relatively <NA>        NA
#> 256           persuits             pursuits <NA>        NA
#> 257           strikely           strikingly <NA>        NA
#> 258        indigineous           indigenous <NA>        NA
#> 259            occured             occurred <NA>        NA
#> 260         apparantly           apparently <NA>        NA
#> 261          voleyball           volleyball <NA>        NA
#> 262           montains            mountains <NA>        NA
#> 263            teached               taught <NA>        NA
#> 264         hsitorians           historians <NA>        NA
#> 265          hieroglph           hieroglyph <NA>        NA
#> 266          harrasing            harassing <NA>        NA
#> 267          Dravadian            Dravidian <NA>        NA
#> 268         propogates           propagates <NA>        NA
#> 269        succesfully         successfully <NA>        NA
#> 270        manufatured         manufactured <NA>        NA
#> 271          paralelly           parallelly <NA>        NA
#> 272           shineing              shining <NA>        NA
#> 273       strenghtened         strengthened <NA>        NA
#> 274          facilites           facilities <NA>        NA
#> 275         recipiants           recipients <NA>        NA
#> 276          octohedra            octahedra <NA>        NA
#> 277         cerimonies           ceremonies <NA>        NA
#> 278        delevopment          development <NA>        NA
#> 279        eleminating          eliminating <NA>        NA
#> 280          Novermber             November <NA>        NA
#> 281           yeilding             yielding <NA>        NA
#> 282         sucessfull           successful <NA>        NA
#> 283          conquerer            conqueror <NA>        NA
#> 284           allopone            allophone <NA>        NA
#> 285         publishher            publisher <NA>        NA
#> 286       unequalities         inequalities <NA>        NA
#> 287        catapillars         caterpillars <NA>        NA
#> 288          collecton           collection <NA>        NA
#> 289       commisioning        commissioning <NA>        NA
#> 290       crucifiction          crucifixion <NA>        NA
#> 291         publisheed            published <NA>        NA
#> 292         transfered          transferred <NA>        NA
#> 293         Cincinatti           Cincinnati <NA>        NA
#> 294        persistance          persistence <NA>        NA
#> 295        specifiying           specifying <NA>        NA
#> 296             Pucini              Puccini <NA>        NA
#> 297          centruies            centuries <NA>        NA
#> 298          criterias             criteria <NA>        NA
#> 299      refridgerator         refrigerator <NA>        NA
#> 300        inheritence          inheritance <NA>        NA
#> 301      relinqushment       relinquishment <NA>        NA
#> 302        dificulties         difficulties <NA>        NA
#> 303       univeristies         universities <NA>        NA
#> 304            harased             harassed <NA>        NA
#> 305   phillosophically      philosophically <NA>        NA
#> 306           spendour            splendour <NA>        NA
#> 307         embezelled            embezzled <NA>        NA
#> 308           intented             intended <NA>        NA
#> 309       cannotations         connotations <NA>        NA
#> 310        continously         continuously <NA>        NA
#> 311       consistantly         consistently <NA>        NA
#> 312           exellent            excellent <NA>        NA
#> 313        critisizing          criticising <NA>        NA
#> 314        Cincinnatti           Cincinnati <NA>        NA
#> 315          dissapear            disappear <NA>        NA
#> 316         restarants          restaurants <NA>        NA
#> 317      transcendance        transcendence <NA>        NA
#> 318        soverignity          sovereignty <NA>        NA
#> 319      archeaologist         archeologist <NA>        NA
#> 320             Enlish               enlist <NA>        NA
#> 321          beggining            beginning <NA>        NA
#> 322              tihkn                think <NA>        NA
#> 323         seperating           separating <NA>        NA
#> 324      heiroglyphics        hieroglyphics <NA>        NA
#> 325              yaers                years <NA>        NA
#> 326             casues               causes <NA>        NA
#> 327           potrayed            portrayed <NA>        NA
#> 328    untranslateable       untranslatable <NA>        NA
#> 329        implimented          implemented <NA>        NA
#> 330          harrassed             harassed <NA>        NA
#> 331          diablical           diabolical <NA>        NA
#> 332       compatiblity        compatibility <NA>        NA
#> 333          esitmated            estimated <NA>        NA
#> 334           dowloads            downloads <NA>        NA
#> 335         ditributed          distributed <NA>        NA
#> 336         maneouvred           manoeuvred <NA>        NA
#> 337          receeding             receding <NA>        NA
#> 338         immitating            imitating <NA>        NA
#> 339        unneccesary          unnecessary <NA>        NA
#> 340       volonteering         volunteering <NA>        NA
#> 341            effords              efforts <NA>        NA
#> 342           surveyer             surveyor <NA>        NA
#> 343      subcatagories        subcategories <NA>        NA
#> 344          algoritms           algorithms <NA>        NA
#> 345            adequit             adequate <NA>        NA
#> 346          Farenheit           Fahrenheit <NA>        NA
#> 347            altough             although <NA>        NA
#> 348          appenines            Apennines <NA>        NA
#> 349        univesities         universities <NA>        NA
#> 350         enforceing            enforcing <NA>        NA
#> 351          triguered            triggered <NA>        NA
#> 352          facinated           fascinated <NA>        NA
#> 353          everthing           everything <NA>        NA
#> 354        convienient           convenient <NA>        NA
#> 355        continueing           continuing <NA>        NA
#> 356           assasins            assassins <NA>        NA
#> 357           succsess              success <NA>        NA
#> 358         catagories           categories <NA>        NA
#> 359         challanged           challenged <NA>        NA
#> 360         acustommed           accustomed <NA>        NA
#> 361          condamned            condemned <NA>        NA
#> 362        unsuccesful         unsuccessful <NA>        NA
#> 363         definately           definitely <NA>        NA
#> 364           catagory             category <NA>        NA
#> 365        asphyxation         asphyxiation <NA>        NA
#> 366           anounced            announced <NA>        NA
#> 367          permenant            permanent <NA>        NA
#> 368          discribed            described <NA>        NA
#> 369             illess              illness <NA>        NA
#> 370         unsuprised          unsurprised <NA>        NA
#> 371          performes            performed <NA>        NA
#> 372          publisger            publisher <NA>        NA
#> 373         adressable          addressable <NA>        NA
#> 374          marrtyred             martyred <NA>        NA
#> 375           emmiting             emitting <NA>        NA
#> 376           Mythraic             Mithraic <NA>        NA
#> 377     coincedentally       coincidentally <NA>        NA
#> 378        signifigant          significant <NA>        NA
#> 379          controled           controlled <NA>        NA
#> 380       distingished        distinguished <NA>        NA
#> 381         dimunitive           diminutive <NA>        NA
#> 382           athenean             athenian <NA>        NA
#> 383          addresing           addressing <NA>        NA
#> 384             offred              offered <NA>        NA
#> 385          propperly             properly <NA>        NA
#> 386            infered             inferred <NA>        NA
#> 387             habeus               habeas <NA>        NA
#> 388            careing               caring <NA>        NA
#> 389          adquiring            acquiring <NA>        NA
#> 390         Guaduloupe            Guadalupe <NA>        NA
#> 391       contributers         contributors <NA>        NA
#> 392         aggreement            agreement <NA>        NA
#> 393          introdued           introduced <NA>        NA
#> 394      demographical          demographic <NA>        NA
#> 395           acccused              accused <NA>        NA
#> 396       speciallized          specialized <NA>        NA
#> 397     contamporaries       contemporaries <NA>        NA
#> 398     restaraunteurs        restaurateurs <NA>        NA
#> 399       circumsicion         circumcision <NA>        NA
#> 400             useing                using <NA>        NA
#> 401          satelites           satellites <NA>        NA
#> 402          countains             contains <NA>        NA
#> 403           diffrent            different <NA>        NA
#> 404      transcripting        transcription <NA>        NA
#> 405           inventer             inventor <NA>        NA
#> 406         cartilidge            cartilage <NA>        NA
#> 407         bombarment          bombardment <NA>        NA
#> 408          dipthongs           diphthongs <NA>        NA
#> 409     archeaologists        archeologists <NA>        NA
#> 410          survivied             survived <NA>        NA
#> 411        rechargable         rechargeable <NA>        NA
#> 412          mispelled           misspelled <NA>        NA
#> 413      transcripting         transcribing <NA>        NA
#> 414         descriibes            describes <NA>        NA
#> 415        conotations         connotations <NA>        NA
#> 416            visting             visiting <NA>        NA
#> 417          collonies             colonies <NA>        NA
#> 418           Godounov              Godunov <NA>        NA
#> 419           agreemnt            agreement <NA>        NA
#> 420           anwsered             answered <NA>        NA
#> 421         consistant           consistent <NA>        NA
#> 422      decomissioned       decommissioned <NA>        NA
#> 423           pessiary              pessary <NA>        NA
#> 424         parituclar           particular <NA>        NA
#> 425         gaurenteed           guaranteed <NA>        NA
#> 426      independendet          independent <NA>        NA
#> 427       manouverable         manoeuvrable <NA>        NA
#> 428         phenonmena            phenomena <NA>        NA
#> 429      accomadations       accommodations <NA>        NA
#> 430         rennovated            renovated <NA>        NA
#> 431           reffered             referred <NA>        NA
#> 432        accomodates         accommodates <NA>        NA
#> 433        disasterous           disastrous <NA>        NA
#> 434        unsurprized          unsurprised <NA>        NA
#> 435           siezures             seizures <NA>        NA
#> 436        appereances          appearances <NA>        NA
#> 437          repentent            repentant <NA>        NA
#> 438       controveries        controversies <NA>        NA
#> 439         octohedral           octahedral <NA>        NA
#> 440            dirived              derived <NA>        NA
#> 441            cervial               serval <NA>        NA
#> 442       inprisonment         imprisonment <NA>        NA
#> 443         aggregious            egregious <NA>        NA
#> 444        expiditions          expeditions <NA>        NA
#> 445        distruction          destruction <NA>        NA
#> 446           godesses            goddesses <NA>        NA
#> 447           thikning             thinking <NA>        NA
#> 448           alledges              alleges <NA>        NA
#> 449       unfortunatly        unfortunately <NA>        NA
#> 450         proffesion           profession <NA>        NA
#> 451           linnaena             linnaean <NA>        NA
#> 452             Guilia               Giulia <NA>        NA
#> 453         addressess            addresses <NA>        NA
#> 454       ressurection         resurrection <NA>        NA
#> 455            shoudln               should <NA>        NA
#> 456          imprioned           imprisoned <NA>        NA
#> 457         developped            developed <NA>        NA
#> 458              eyasr                 eyas <NA>        NA
#> 459        indictement           indictment <NA>        NA
#> 460         achivement          achievement <NA>        NA
#> 461          nessecary            necessary <NA>        NA
#> 462       negociations         negotiations <NA>        NA
#> 463       neigbourhood        neighbourhood <NA>        NA
#> 464          diphtongs           diphthongs <NA>        NA
#> 465          disatrous           disastrous <NA>        NA
#> 466        reminescent          reminiscent <NA>        NA
#> 467       reccommended          recommended <NA>        NA
#> 468            nucular              nuclear <NA>        NA
#> 469         provisonal          provisional <NA>        NA
#> 470           fimilies             families <NA>        NA
#> 471           sucessot            successor <NA>        NA
#> 472      carthographer         cartographer <NA>        NA
#> 473           milennia            millennia <NA>        NA
#> 474        critisising          criticising <NA>        NA
#> 475       speciallized          specialised <NA>        NA
#> 476            ocurred             occurred <NA>        NA
#> 477           annouced            announced <NA>        NA
#> 478          Europians            Europeans <NA>        NA
#> 479          competive          competitive <NA>        NA
#> 480           seceeded            succeeded <NA>        NA
#> 481       catergorized          categorized <NA>        NA
#> 482             reched              reached <NA>        NA
#> 483          gaurentee            guarantee <NA>        NA
#> 484        Napoleonian           Napoleonic <NA>        NA
#> 485         publisherr            publisher <NA>        NA
#> 486           portayed            portrayed <NA>        NA
#> 487          eventally           eventually <NA>        NA
#> 488          publihser            publisher <NA>        NA
#> 489         exapansion            expansion <NA>        NA
#> 490              endig               ending <NA>        NA
#> 491      dissappointed         disappointed <NA>        NA
#> 492           campains            campaigns <NA>        NA
#> 493             rigeur                rigor <NA>        NA
#> 494              konws                knows <NA>        NA
#> 495          massmedia           mass media <NA>        NA
#> 496           progroms              pogroms <NA>        NA
#> 497         dispencing           dispensing <NA>        NA
#> 498           iritated            irritated <NA>        NA
#> 499        comissioned         commissioned <NA>        NA
#> 500       traditionnal          traditional <NA>        NA
#> 501           Guiseppe             Giuseppe <NA>        NA
#> 502        acheivments         achievements <NA>        NA
#> 503           persuing             pursuing <NA>        NA
#> 504        autoctonous        autochthonous <NA>        NA
#> 505        spectaulars         spectaculars <NA>        NA
#> 506        artifically         artificially <NA>        NA
#> 507         comisioner         commissioner <NA>        NA
#> 508       infrantryman          infantryman <NA>        NA
#> 509          definance             defiance <NA>        NA
#> 510         deparments          departments <NA>        NA
#> 511         Morrisette           Morissette <NA>        NA
#> 512          deparment           department <NA>        NA
#> 513         oppononent             opponent <NA>        NA
#> 514      assasinations       assassinations <NA>        NA
#> 515           annoints              anoints <NA>        NA
#> 516           abudance            abundance <NA>        NA
#> 517         draughtman          draughtsman <NA>        NA
#> 518        refereneced           referenced <NA>        NA
#> 519         fisionable          fissionable <NA>        NA
#> 520       arrengements         arrangements <NA>        NA
#> 521       indpendently        independently <NA>        NA
#> 522          renedered                rende <NA>        NA
#> 523 histocompatability   histocompatibility <NA>        NA
#> 524           persuded            persuaded <NA>        NA
#> 525          moleclues            molecules <NA>        NA
#> 526      noncombatents        noncombatants <NA>        NA
#> 527         disapeared          disappeared <NA>        NA
#> 528            refrers               refers <NA>        NA
#> 529         surounding          surrounding <NA>        NA
#> 530           suprised            surprised <NA>        NA
#> 531           heridity             heredity <NA>        NA
#> 532           critized           criticized <NA>        NA
#> 533          stainlees            stainless <NA>        NA
#> 534        accquainted           acquainted <NA>        NA
#> 535        ubiquitious           ubiquitous <NA>        NA
#> 536            Januray              January <NA>        NA
#> 537        aggaravates           aggravates <NA>        NA
#> 538            wayword              wayward <NA>        NA
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

**fuzzystring** now keeps more of the join execution on a compiled C++
path while using `data.table` to orchestrate candidate generation. The
package is optimized for:

- Compiled row expansion, row binding, and final assembly across join
  modes
- Adaptive candidate planning with prefiltering for Levenshtein-like
  metrics and larger dense blocks for low-duplication workloads
- Memory-efficient joins using data.table’s reference semantics
- Type-safe column operations preserving factors, dates, times, factors,
  and list-columns

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

## Credits

- **David Robinson**: Credited for the `fuzzyjoin` package, which served
  as the base for `fuzzystring`; some core `fuzzyjoin` functions were
  adapted and reinterpreted using `data.table`.

**fuzzystring** builds on these great packages to provide a fast,
data.table-native implementation with a larger compiled C++ execution
path.
