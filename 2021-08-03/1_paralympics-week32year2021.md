Paralympics
================
Lakshman Balaji
08/03/2021

# Background - the Paralympics dataset

-   This week’s *tidytuesday* plot comes from [The International
    Paralympics
    Committee](https://db.ipc-services.org/sdms/hira/web/index).
-   This is the link to the [TidyTuesday Github
    page](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-03/readme.md)
    for week 32, 2021 data.
-   For this week’s analysis, I will be using last week’s data as well,
    and looking at *the number of medals won by Olympians and
    Paralympians (for sports in common) from 1980 to 2016 for 5 select
    countries (USA, China, Canada, Great Britain and Australia)*.
-   I will create an interactive circle packing widget using the awesome
    `circlepackeR` package from
    [jeromefroe](https://github.com/jeromefroe/circlepackeR).

# Libraries

``` r
library(tidytuesdayR)
library(tidyverse)
library(kableExtra)

# for figure
# devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(data.tree)
library(htmlwidgets)
```

# Load data

## This week’s data: Paralympians

-   This is *week 32, year 2021* data. (date: 2021-08-03)
-   Contains name, country, type of event, medal and year for
    Paralympian medal winners from *1980-2016*.

``` r
dat <- tidytuesdayR::tt_load(2021, week = 32)$athletes

# alternatively ,load excel worksheet
# library(openxlsx)
# dat <- read.xlsx("0_paralympics.xlsx")

# sample of certain columns
dat %>%
select(type, year, athlete, abb, medal) %>%
head(n = 5) %>%
kable(caption = "Sample of paralympics data")
```

## Previous week’s data: Olympians

-   Load in previous week’s data as well.
-   This data is for all Olympians (not just medallists) who
    participated in the Olympics from *1896-2016*.

``` r
olympics <- tidytuesdayR::tt_load(2021, week = 31)$olympics

# alternatively, load excel worksheet
# olympics <- read.xlsx("0_olympics.xlsx")

# get names of sports represented in data
sports <- data.frame(unique(olympics$event))                         %>% 
          arrange(unique.olympics.event.)                            %>%
          mutate(sportname = str_split(unique.olympics.event., " ")) %>%
          mutate(sportname2 = word(unique.olympics.event., 1, sep = fixed(' ')))

# clean labels
sports <- sports %>% 
          mutate(sportname3 = sportname2) %>%
          mutate(sportname3 = str_replace(sportname3, "Alpinism", "Alpine"))                      %>%
          mutate(sportname3 = str_replace(sportname3, "Beach", "Volleyball (Beach)"))             %>%  
          mutate(sportname3 = str_replace(sportname3, "Cross", "Skiing (Cross Country)"))         %>%
          mutate(sportname3 = str_replace(sportname3, "Figure", "Skating (Figure)"))              %>%
          mutate(sportname3 = str_replace(sportname3, "Ice", "Hockey (Ice)"))                     %>%
          mutate(sportname3 = str_replace(sportname3, "Jeu", "Jeu De Paume"))                     %>%
          mutate(sportname3 = str_replace(sportname3, "Freestyle", "Skiing (Freestyle)"))         %>%
          mutate(sportname3 = str_replace(sportname3, "Military", "Miltary Ski Patrol"))          %>%
          mutate(sportname3 = str_replace(sportname3, "Modern", "Modern Pentathlon"))             %>%
          mutate(sportname3 = str_replace(sportname3, "Rhythmic", "Gymnastics (Rhythmic)"))       %>% 
          mutate(sportname3 = str_replace(sportname3, "^Ski$", "Ski Jumping"))                    %>% # be sure to anchor
          mutate(sportname3 = str_replace(sportname3, "Speed", "Skating (Speed)"))                %>% 
          mutate(sportname3 = str_replace(sportname3, "Synchronized", "Swimming (Synchronized)")) %>%
          mutate(sportname3 = str_replace(sportname3, "Table", "Tennis (Table Tennis)"))          %>%
          mutate(sportname3 = str_replace(sportname3, "Short", "Skating (Short Track Speed)"))    %>% 
          mutate(sportname3 = str_replace(sportname3, "Water", "Polo - Water"))                   %>%
          select(unique.olympics.event., sportname3)                                              %>%
          rename(simplifiedname = sportname3,
                 event = unique.olympics.event.)

# merge back with main dataset
olympics <- olympics %>%
            left_join(sports)

# Glimpse a sample of the cleaned event labels
olympics %>%
select(event, simplifiedname) %>%
head(n = 5) %>%
kable(caption = "Sample of cleaned event names")

# All the sports available to us
# unique(olympics$simplifiedname)

# remove clutter
rm(sports)
```

# Identify sports and time-periods in common for Olympian and Paralympian data

To compare the Olympians and Paralympians, we need to:

-   Subset the Olympians to include only the medal winners (the
    Paralympian data has only medal winners).
-   Subset the Olympians to include only years 1980-2016 (the
    Paralympian data only has years 1980-2016).
-   Subset the Olympians and Paralympians to include only the events
    that are common to both datasets (the number of sports in the
    Paralympics dataset is much lesser than the number of sports in the
    Olympics dataset)

``` r
# sports in paralympics data
paralympicsports <- unique(dat$type)


# change one event name in Olympic data to match paralympics data
olympicsports <- unique(olympics$simplifiedname)
olympicsports[which(olympicsports == "Tennis (Table Tennis)")] <- "Table Tennis"

# find sports common to olympic data and paralympic data
common <- paralympicsports[paralympicsports %in% olympicsports]
common
```

    ## [1] "Archery"      "Athletics"    "Basketball"   "Fencing"      "Rugby"       
    ## [6] "Swimming"     "Table Tennis" "Triathlon"    "Volleyball"

``` r
# what years are common to both datasets?
range(dat$year)
```

    ## [1] 1980 2016

``` r
range(olympics$year)
```

    ## [1] 1896 2016

-   Now, subset the Paralympics data to keep only the common sports.

``` r
# keep only common sports in dat. Remember, this 
# has only medal winners from 1980 onwards
para <- dat %>% filter(type %in% common)
```

-   Do the same to the Olympics data. But also, remember to keep only
    medal winners and years 1980 onwards.

``` r
# subset olympics to keep only years 1980 and onward
# keep only medal winners and keep only common sports
oly <- olympics %>% 
       filter(year >= 1980 & 
            !(is.na(medal)) &
              simplifiedname %in% common) 
       

# remove clutter
rm(common, olympicsports, paralympicsports)
```

-   What are the top medal winning countries since 1980s in Paralympics?

``` r
# paralympians
para %>% 
group_by(abb) %>%
count(sort = TRUE) %>%
head(n = 5) %>%
kable(caption = "Top 5 medal winners for Paralympics since 1980 in sports common to Olympians and Paralympians")
```

<table>
<caption>
Top 5 medal winners for Paralympics since 1980 in sports common to
Olympians and Paralympians
</caption>
<thead>
<tr>
<th style="text-align:left;">
abb
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
USA
</td>
<td style="text-align:right;">
1853
</td>
</tr>
<tr>
<td style="text-align:left;">
GBR
</td>
<td style="text-align:right;">
1356
</td>
</tr>
<tr>
<td style="text-align:left;">
CHN
</td>
<td style="text-align:right;">
1096
</td>
</tr>
<tr>
<td style="text-align:left;">
AUS
</td>
<td style="text-align:right;">
1092
</td>
</tr>
<tr>
<td style="text-align:left;">
CAN
</td>
<td style="text-align:right;">
1025
</td>
</tr>
</tbody>
</table>

-   I want to now see how the number of Paralympic medals from these
    Paralympic leaders compare to their Olympic medal numbers.

``` r
# common sports are
# "Archery"  
# "Athletics"  
# "Basketball" 
# "Fencing"     
# "Rugby"       
# "Swimming"   
# "Table Tennis"
# "Triathlon"  
# "Volleyball" 

# paralympic medal winners from USA, GBR, CHN, AUS, CAN from 1980 onwards in 
# common sports
temp1 <- para %>% filter(abb %in% c("USA", "GBR", "CHN", "AUS", "CAN"))
# olympic medal winners from USA, GBR, CHN, AUS, CAN from 1980 onwards in 
# common sports
temp2 <- oly %>% filter(noc %in% c("USA", "GBR", "CHN", "AUS", "CAN"))



# Count number of gold, silver and bronze medals won by each country paralympians in each sport
# since 1980
tally_para <- temp1 %>% 
group_by(abb, type, medal) %>%
summarize(medals_won = n()) %>%
arrange(-medals_won, type) %>%
mutate(participant = "Paralympian")

# Count number of gold, silver and bronze medals won by each country Olympians in each sport
# since 1980
tally_oly <- temp2 %>% 
group_by(noc, simplifiedname, medal) %>%
summarize(medals_won = n()) %>%
arrange(-medals_won, simplifiedname) %>%
rename(type = simplifiedname, abb = noc)  %>%
mutate(participant = "Olympian")


# rbind
medals <- rbind(tally_para, tally_oly) %>% 
          arrange(abb,medal, type, participant) 
```

# Figure

-   Our data frame is already nested, so we can go ahead and prepare the
    figure.
-   Used this
    [link](https://www.r-graph-gallery.com/338-interactive-circle-packing-with-circlepacker)
    from R Graph Gallery to help with the plot.

``` r
# use data.tree::as.Node() to prep data for figure
medals$pathString <- paste("root2",
                           medals$abb,
                           medals$type, 
                           medals$participant, 
                           medals$medal, 
                           medals$medals_won,
                           sep = "/")
medalspopulation <- as.Node(medals)
```

``` r
# Make the plot
p <- circlepackeR(medalspopulation, 
             size = "medals_won",
             color_min = "hsl(56,80%,80%)", 
             color_max = "hsl(341,30%,40%)")
p
```

![](1_paralympics-week32year2021_files/figure-gfm/interactive%20widget-1.png)<!-- -->

``` r
# save widget
saveWidget(p, file="2circles_paralympics_olympics.html")
```

## Chief take-aways

-   USA’s Olympians and Paralympians have won the most medals since 1980
    for the 9 sports that these two groups of athletes have in common.
    The US is followed closely by China, Australia and Great Britain.
-   Paralympians seem to bring home more medals than Olympians in
    swimming and athletics (especially in Australia, Canada and Great
    Britain)
-   In some sport-and-country combinations, like table tennis for the
    US, all medals won since 1980 have exclusively been by Paralympians.
-   In other sport-and-country combinations, like basketball for China,
    all medals won since 1980 have been exclusively by Olympians.
-   This chart could be improved by adding titles, and better labels but
    unsure of how to manipulate this html widget to do that.
