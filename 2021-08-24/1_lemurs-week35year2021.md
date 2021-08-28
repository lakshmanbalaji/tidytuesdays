Lemur mortality
================
Lakshman Balaji
8/28/2021

# Background - the Lemurs dataset

  - This week’s *tidytuesday* plot focuses on lemur data originally from
    [Zehr et al from the Duke Lemur
    Center.](https://www.nature.com/articles/sdata201419).
  - The link to the Duke Lemur Center is
    [here.](https://lemur.duke.edu/)
  - In a nutshell, this data contains information such as lemur date of
    birth, death, month of conception, number of offspring etc.
  - To learn more about the dataset, please navigate to the [TidyTuesday
    Github page \> year 2021 \>
    week 35.](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-24/readme.md)
    Many thanks to [Jesse Mostipak](https://www.jessemaegan.com/) for
    cleaning this data.
  - My goal for this week is to try and see if I can calculate an infant
    mortality rate for lemurs, and see how that changes over time. The
    challenges are mainly going to be figuring how to define infant
    mortality as having occurred, and dealing with missingness in date
    of birth/date of death.

<!-- end list -->

``` r
library(tidytuesdayR)
library(tidyverse)
library(kableExtra) # print table outputs
library(lubridate)  # calculate age of lemurs
```

  - This is week 35, year 2021 data.

<!-- end list -->

``` r
dat <- tidytuesdayR::tt_load(2021, week = 35)
lemurs <- dat$lemur_data
taxon <- dat$taxonomy
# alternatively lemurs <- readRDS('0_lemurs') taxon <-
# readRDS('0_taxon')
rm(dat)
```

# Subset to the Duke Lemur Center

  - We do this because a good 80 percent of the data is just from this
    one site (\~64k records)

<!-- end list -->

``` r
# most data is from duke
lemurs %>%
    group_by(birth_institution) %>%
    summarize(n = n(), percent = round(n()/nrow(lemurs) * 100,
        2)) %>%
    arrange(-n)
```

    ## # A tibble: 88 x 3
    ##    birth_institution                        n percent
    ##    <chr>                                <int>   <dbl>
    ##  1 Duke Lemur Center                    64443   78.0 
    ##  2 Madagascar /                          4931    5.97
    ##  3 General Public                        1996    2.42
    ##  4 Museum National d'Histoire Naturelle  1929    2.34
    ##  5 Unknown Location                       850    1.03
    ##  6 Cincinnati Zoo & Botanical Garden      550    0.67
    ##  7 N. VIETNAM                             523    0.63
    ##  8 Skansen Akvariet                       470    0.57
    ##  9 Comoros /                              464    0.56
    ## 10 Parc Zoologique de Paris (MNHN)        464    0.56
    ## # ... with 78 more rows

``` r
# create the duke subset
duke <- lemurs %>%
    filter(birth_institution == "Duke Lemur Center")
```

## Remove duplicates

  - Notice that this is longitudinal data, as mentioned in the Nature
    paper.
  - I will keep only the most recent record for each lemur.
  - I will do this by taking advantage of the fact that each animal has
    a date on which it was weighed. This date has zero missingness. I
    will sort by `dlc_id` and `weight_date`, and then pick the highest
    `weight_date` within `dlc_id`.
  - I enjoyed adding this footnote to the table. Many thanks to [Hao
    Zhu](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
    for his vignette on using `knitr::kable` and `kableExtra`.

<!-- end list -->

``` r
# notice duplicates
duke %>%
    filter(dlc_id == "0009") %>%
    select(dlc_id, name, dob, sire_name, weight_date, weight_g) %>%
    kable() %>%
    footnote(general = "Pooh bear seems to be losing weight",
        footnote_as_chunk = T)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

dlc\_id

</th>

<th style="text-align:left;">

name

</th>

<th style="text-align:left;">

dob

</th>

<th style="text-align:left;">

sire\_name

</th>

<th style="text-align:left;">

weight\_date

</th>

<th style="text-align:right;">

weight\_g

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

0009

</td>

<td style="text-align:left;">

POOH BEAR

</td>

<td style="text-align:left;">

1963-09-30

</td>

<td style="text-align:left;">

HERMIT

</td>

<td style="text-align:left;">

1972-02-16

</td>

<td style="text-align:right;">

899

</td>

</tr>

<tr>

<td style="text-align:left;">

0009

</td>

<td style="text-align:left;">

POOH BEAR

</td>

<td style="text-align:left;">

1963-09-30

</td>

<td style="text-align:left;">

HERMIT

</td>

<td style="text-align:left;">

1972-02-29

</td>

<td style="text-align:right;">

917

</td>

</tr>

<tr>

<td style="text-align:left;">

0009

</td>

<td style="text-align:left;">

POOH BEAR

</td>

<td style="text-align:left;">

1963-09-30

</td>

<td style="text-align:left;">

HERMIT

</td>

<td style="text-align:left;">

1972-06-22

</td>

<td style="text-align:right;">

910

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<span style="font-style: italic;">Note: </span> <sup></sup> Pooh bear
seems to be losing weight

</td>

</tr>

</tfoot>

</table>

``` r
# remove duplicates, ensure that most recent entry is
# retained
duke <- duke %>%
    group_by(dlc_id) %>%
    arrange(dlc_id, weight_date) %>%
    mutate(weighingnum = row_number()) %>%
    filter(weighingnum == max(weighingnum)) %>%
    ungroup()


nrow(duke)
```

    ## [1] 1898

  - After removing duplicates, we have a little under 2k unique
    individuals represented from the Duke Lemur Center.

# Looking at death.

  - How many lemurs at the Duke Lemur Center have a date of death?

<!-- end list -->

``` r
sum(!(is.na(duke$dod)))
```

    ## [1] 1183

  - This tells us that 62.3% of the lemurs at Duke have been confirmed
    to have died. We could easily calculate an infant mortality rate
    from this data. However, the complication is in the way the `dod`
    date of death variable is defined. On the TidyTuesday github page,
    it says
    [this](Date%20of%20death:%20Verified%20date%20an%20animal%20died.%20Missing%20indicates%20animal%20is%20either%20alive%20or%20status%20is%20unknown)
    in the data dictionary: *“Date of death: Verified date an animal
    died. Missing indicates animal is either alive or status is
    unknown.”*
  - That still leaves n = 715 (around 38% of the lemurs at Duke) that
    could either be alive, or could be dead and the death was not
    recorded.
  - If the animal died and the death was not recorded, we would have an
    inaccurate picture of the infant mortality rate in lemurs.
  - I am going to try to fix this by looking at another variable in the
    data, the `age_last_verified_y`. This variable tells us the age that
    the animal’s age was last verified. This could be useful to
    calculate infant mortality because we would now know to what age the
    animal lived, at the least.
  - Additionally, to add to the information provided by
    `age_last_verified_y`, I am going to create a new variable called
    `age_last_verified_lb` (the ‘lb’ suffix signifies that this variable
    was added by me, as opposed to the variable already in the dataset),
    which will be the difference between the `dob` and `weight_date`.
  - We are basically taking advantage of the fact that each record in
    the `duke` dataset is the most recent weighing for the lemur.
  - So, if `age_last_verified_y` is missing for an animal that doesn’t
    have a `dod`, we can use `age_last_verified_lb` to figure out the
    minimum age to which we know the animal lived for sure.

## For lemurs at Duke with missing dates of death, figure out how long the animal lived for.

  - Let’s see when the dates of birth were.

<!-- end list -->

``` r
# all the duke animals that are missing a date of death are
# now represented in the dataset mysterylemurs
mysterylemurs <- duke %>%
    filter(is.na(duke$dod))

# date-of-birth missigness for these mystery lemurs: none
# seem to be missing a date-of-birth
sum(is.na(mysterylemurs$dob))
```

    ## [1] 0

  - For these lemurs with missing dates of death, look at the
    `age_last_verified_y`, and also prepare a `age_last_verified_lb`
    variable that is calculated by subtracting `dob` from `weight_date`.

<!-- end list -->

``` r
mysterylemurs <- mysterylemurs %>%
    mutate(age_last_verified_lb = interval(dob, weight_date)/years(1)) %>%
    select(dlc_id, name, dob, weight_date, age_last_verified_y,
        age_last_verified_lb)
```

  - Now, this is interesting. Notice that for any lemur that has a
    missing `age_last_verified_y`, we have the `age_last_verified_lb`
    variable that tells us how old the animal was on its most recent
    weighing.

<!-- end list -->

``` r
set.seed(1234)

mysterylemurs %>%
    filter(is.na(age_last_verified_y)) %>%
    filter(row_number() %in% sample(1:nrow(.), size = 10)) %>%
    kable() %>%
    footnote(general = "For example, we now know that Gordian lived to atleast 23 years of age, and didn't die as an infant.",
        footnote_as_chunk = TRUE)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

dlc\_id

</th>

<th style="text-align:left;">

name

</th>

<th style="text-align:left;">

dob

</th>

<th style="text-align:left;">

weight\_date

</th>

<th style="text-align:right;">

age\_last\_verified\_y

</th>

<th style="text-align:right;">

age\_last\_verified\_lb

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

6650

</td>

<td style="text-align:left;">

Gordian

</td>

<td style="text-align:left;">

1995-12-17

</td>

<td style="text-align:left;">

2019-01-22

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

23.098630

</td>

</tr>

<tr>

<td style="text-align:left;">

6996

</td>

<td style="text-align:left;">

Brigitta

</td>

<td style="text-align:left;">

2011-05-16

</td>

<td style="text-align:left;">

2019-01-29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

7.706849

</td>

</tr>

<tr>

<td style="text-align:left;">

7075

</td>

<td style="text-align:left;">

Remus

</td>

<td style="text-align:left;">

2012-01-12

</td>

<td style="text-align:left;">

2019-01-07

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

6.986301

</td>

</tr>

<tr>

<td style="text-align:left;">

7109

</td>

<td style="text-align:left;">

Shiso

</td>

<td style="text-align:left;">

2012-07-03

</td>

<td style="text-align:left;">

2019-02-01

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

6.583562

</td>

</tr>

<tr>

<td style="text-align:left;">

7151

</td>

<td style="text-align:left;">

Rees

</td>

<td style="text-align:left;">

2013-05-17

</td>

<td style="text-align:left;">

2019-01-13

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

5.660274

</td>

</tr>

<tr>

<td style="text-align:left;">

7152

</td>

<td style="text-align:left;">

Amor Jr. 

</td>

<td style="text-align:left;">

2013-05-17

</td>

<td style="text-align:left;">

2019-01-13

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

5.660274

</td>

</tr>

<tr>

<td style="text-align:left;">

7161

</td>

<td style="text-align:left;">

Domestic Chicken

</td>

<td style="text-align:left;">

2013-06-26

</td>

<td style="text-align:left;">

2019-01-29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

5.594520

</td>

</tr>

<tr>

<td style="text-align:left;">

7171

</td>

<td style="text-align:left;">

Eleanor of Portugal

</td>

<td style="text-align:left;">

2014-01-05

</td>

<td style="text-align:left;">

2018-04-06

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

4.249315

</td>

</tr>

<tr>

<td style="text-align:left;">

7183

</td>

<td style="text-align:left;">

Anhotep

</td>

<td style="text-align:left;">

2014-05-14

</td>

<td style="text-align:left;">

2019-01-25

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

4.701370

</td>

</tr>

<tr>

<td style="text-align:left;">

7210

</td>

<td style="text-align:left;">

Thea

</td>

<td style="text-align:left;">

2015-05-04

</td>

<td style="text-align:left;">

2019-01-29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

3.739726

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<span style="font-style: italic;">Note: </span> <sup></sup> For example,
we now know that Gordian lived to atleast 23 years of age, and didn’t
die as an infant.

</td>

</tr>

</tfoot>

</table>

# Define infant mortality.

  - This paper, [Lifespan and Reproductive Senescene in a Free-Ranging
    Ring Tailed Lemur Population at Berenty,
    Madagascar](https://www.karger.com/article/FullText/368670) by
    *Ichini et al* in Folia Primatologica found that “The 1-year
    mortality rate (from birth to 1 year old) was 26.0%”
  - I am no expert on lemurs, but I do know that this likely differs by
    lemur species. But, for the sake of simplicity, I will assume
    one-year as the cut-off to define if infant mortality has occured.
  - The average last verified age of all the Duke Lemurs (regardless of
    whether date of death was missing or not) was 8.7314228
  - So, here’s the plan—
      - For a lemur with a missing date-of-death, if the last known age
        was greater than 1, then that lemur did not experience infant
        mortality.
      - For a lemur with a missing date-of-death, and the last known age
        lesser than 1, if the date of birth was before 2010 (2019 - the
        average age of all lemurs in the dataset), then I assume the
        lemur experienced infant mortality.
      - For a lemur with a missing date-of-death, and the last known age
        lesser than 1, if the date of birth was after 2010, then I
        assume that lemur is still alive and simply hasn’t been weighed
        in the last \~9 years.

## Determine infant mortality for lemurs with a missing date-of-death.

``` r
mysterylemurs <- mysterylemurs %>%
                                          # if the age_last_verified_y column is present and indicates
                                          # a verified age > 0, then the lemur did not experience 
                                          # infant mortality
                 mutate(infantmortality = ifelse(!(is.na(age_last_verified_y)) & age_last_verified_y > 1, 0,
                                          # if the age_last_verified_y column is missing, and 
                                          # the back-up column (age_last_verified_lb) indicates an age 
                                          # greater than 1 on last weigh-in, then the lemur did not 
                                          # experience infant mortality
                                          ifelse((is.na(age_last_verified_y)) & age_last_verified_lb > 1, 0,
                                          # if the age_last_verified_y column is present and shows
                                          # an age < 1, or if the age_last_verified_y column is absent
                                          # and the age_last_verified_lb shows an age < 1, then tag
                                          # these lemurs with a 2
                                                                                                        2)))

table(mysterylemurs$infantmortality)
```

    ## 
    ##   0   2 
    ## 675  40

  - It looks like \(\frac{675}{715}\) lemurs with missing dates of death
    were classified as not having experienced infant mortality.
  - Only \(\frac{40}{715}\) lemurs with missing dates of death were
    flagged for further review with a 2.
  - Here’s a sample of the `mysterylemurs` dataset.

<!-- end list -->

``` r
mysterylemurs %>%
    filter(row_number() %in% 93:96) %>%
    kable() %>%
    footnote(general = "Demas and Parmena are lemurs that we know for sure survived beyond the age of one. But we need to look at Tychicus and Carpus closer",
        footnote_as_chunk = TRUE)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

dlc\_id

</th>

<th style="text-align:left;">

name

</th>

<th style="text-align:left;">

dob

</th>

<th style="text-align:left;">

weight\_date

</th>

<th style="text-align:right;">

age\_last\_verified\_y

</th>

<th style="text-align:right;">

age\_last\_verified\_lb

</th>

<th style="text-align:right;">

infantmortality

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

1645

</td>

<td style="text-align:left;">

DEMAS

</td>

<td style="text-align:left;">

1985-07-08

</td>

<td style="text-align:left;">

1989-09-20

</td>

<td style="text-align:right;">

4.42

</td>

<td style="text-align:right;">

4.2027397

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

1649

</td>

<td style="text-align:left;">

PARMENA

</td>

<td style="text-align:left;">

1985-07-14

</td>

<td style="text-align:left;">

1986-01-20

</td>

<td style="text-align:right;">

2.91

</td>

<td style="text-align:right;">

0.5205479

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

1650

</td>

<td style="text-align:left;">

TYCHICUS

</td>

<td style="text-align:left;">

1985-07-18

</td>

<td style="text-align:left;">

1986-02-17

</td>

<td style="text-align:right;">

0.90

</td>

<td style="text-align:right;">

0.5863014

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

1651

</td>

<td style="text-align:left;">

CARPUS

</td>

<td style="text-align:left;">

1985-07-18

</td>

<td style="text-align:left;">

1986-03-17

</td>

<td style="text-align:right;">

0.90

</td>

<td style="text-align:right;">

0.6630137

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<span style="font-style: italic;">Note: </span> <sup></sup> Demas and
Parmena are lemurs that we know for sure survived beyond the age of one.
But we need to look at Tychicus and Carpus closer

</td>

</tr>

</tfoot>

</table>

  - Now, look at Tychicus. Tychicus is missing a date of death. The last
    verified age was when Tychicus was a little under a year old (last
    weigh-in was a little after 6 months).
  - But Tychicus’ date of birth was July 1985\!
  - Since this script is being written in 2021, and since if there had
    been any more recent weigh-ins, we would have extracted those
    datapoints, I can only assume that Tychicus hasn’t ever been weighed
    since the mid 1980s. I am going to assume that this has happened
    because Tychicus died soon after that first data point was
    collected, and I am going to assign him (and Carpus, who is similar
    to him) with a 1 for infant mortality. In fact, I will do this for
    all lemurs whose date of birth was prior to 2010.
  - This method does have its drawbacks and could be overestimating
    infant mortality (Tychicus could have been weighed and the data not
    entered, Tychicus could have survived to age 3 or 4 and died right
    before his next scheduled weighing, Tychicus could have been
    transferred to another Lemur Conservation Center etc.), but I just
    want a rough approximation of infant mortality for this
    visualization and so this will do. There is no way to approach the
    truth without having complete `dod` data.

<!-- end list -->

``` r
mysterylemurs <- mysterylemurs %>%
                 # set infant mortality to 1 for all lemurs with a missing date of death and
                 # with last verified age < 1 and DOB prior to 2010.
                 mutate(infantmortality = ifelse(infantmortality == 2 & lubridate::year(dob) < 2010,  1,
                # set infant mortality to 0 for all lemurs with a missing date of death, with
                # last verified age < 1 and DOB in or after 2010.
                                          ifelse(infantmortality == 2 & lubridate::year(dob) >= 2010, 0,   
                                                 infantmortality)))
```

## Determine infant mortality for lemurs with a non-missing date-of-death.

  - This is straightforward. We just pull out the lemurs with a
    non-missing `dod` from the `duke` dataset and calculate their age at
    death. If it was greater than 1, we assign a 0 to `infantmortality`.
    If it was lesser than 1, we assign a 1 to `infantmortality`.

<!-- end list -->

``` r
# notice one lemur with a non-missing date of death has a
# missing dob and has absolutely no age data
duke %>%
    filter(!is.na(dod)) %>%
    filter(is.na(dob)) %>%
    select(dlc_id, name, age_last_verified_y)
```

    ## # A tibble: 1 x 3
    ##   dlc_id name       age_last_verified_y
    ##   <chr>  <chr>                    <dbl>
    ## 1 7014   SANDALWOOD                  NA

``` r
nonmysterylemurs <- duke %>%
    # remove one lemur with a missing DOB
filter(!(dlc_id == 7014)) %>%
    filter(!(is.na(dod))) %>%
    mutate(ageatdeath = interval(dob, dod)/years(1)) %>%
    mutate(infantmortality = ifelse(ageatdeath > 1, 0, 1)) %>%
    select(dlc_id, name, dob, infantmortality)
```

## Combine both datasets (lemurs with missing date of death and non-missing date of death)

``` r
dukemortality <- rbind(nonmysterylemurs, mysterylemurs[, c("dlc_id",
    "name", "dob", "infantmortality")])

# get rid of clutter
rm(mysterylemurs, nonmysterylemurs)
```

# Look at infant mortality.

``` r
dukemortality %>%
    group_by(infantmortality) %>%
    summarize(n = n(), prop = round(100 * n/1898, 2))
```

    ## # A tibble: 2 x 3
    ##   infantmortality     n  prop
    ##             <dbl> <int> <dbl>
    ## 1               0  1632  86.0
    ## 2               1   265  14.0

  - The infant mortality rate in this Duke Lemur Center dataset is
    around *14%*
  - Does it change at all over the years? If so, how?

## Infant mortality over the years

  - Here, we define the year as the year in which the lemur was born.
  - For each five-year cohort, we find out what the mortality rate was.

<!-- end list -->

``` r
# prep data
dukemortality <- dukemortality %>%
    mutate(year = lubridate::year(dob)) %>%
    # 1960-1964 is yearcohort 1 1965-1969 is yearcohort 2..
    # ....  2015-2020 is yearcohort 3
mutate(yearcohort = findInterval(year, seq(1960, 2020, 5)))

# prep data some more
dukemortsum <- dukemortality %>%
    group_by(yearcohort) %>%
    summarize(mortality = sum(infantmortality)/n(), ninfantdeaths = sum(infantmortality),
        nlemurs = n())

# View data
dukemortsum %>%
    kable() %>%
    footnote(general = "We don't see a clear pattern in mortality",
        footnote_as_chunk = TRUE)
```

<table>

<thead>

<tr>

<th style="text-align:right;">

yearcohort

</th>

<th style="text-align:right;">

mortality

</th>

<th style="text-align:right;">

ninfantdeaths

</th>

<th style="text-align:right;">

nlemurs

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

8

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0.0555556

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

18

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0.1473684

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

95

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0.1735160

</td>

<td style="text-align:right;">

38

</td>

<td style="text-align:right;">

219

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.0876712

</td>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

365

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.1274725

</td>

<td style="text-align:right;">

58

</td>

<td style="text-align:right;">

455

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.1785714

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

280

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.2288136

</td>

<td style="text-align:right;">

27

</td>

<td style="text-align:right;">

118

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

0.2615385

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

65

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

0.1333333

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

60

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

0.0774648

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

142

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

0.1250000

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

72

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<span style="font-style: italic;">Note: </span> <sup></sup> We don’t see
a clear pattern in mortality

</td>

</tr>

</tfoot>

</table>

## A simple ggplot visualization

``` r
ggplot(data = dukemortsum, aes(x = yearcohort, y = mortality *
    100)) + geom_point() + geom_line() + scale_x_continuous(breaks = c(1:12),
    labels = c(as.character(seq(1960, 2020, 5))[1:12])) + theme_bw() +
    labs(x = "Five-year period", y = "Infant mortality rate (%)",
        title = "Infant mortality rate in lemurs.", subtitle = "Mortality defined as death < 1 year",
        caption = "Date from 1897 individuals from the Duke Lemur Center | TidyTuesday | @LakshmanBalaji9")
```

![](1_lemurs-week35year2021_files/figure-gfm/Infant%20mortality-1.png)<!-- -->
