Scooby Doo
================
Lakshman Balaji
7/13/2021

# Background - the Scooby Doo Dataset

-   For this week’s *tidytuesday* challenge, we will be using a Kaggle
    dataset (manually aggregated by
    [plummye](https://www.kaggle.com/williamschooleman)).
-   This dataset has information from the last 50 years for episodes of
    [Scooby-Doo](https://scoobydoo.fandom.com/wiki/Scoobypedia)
-   Each row in this dataset represents an episode, and has a large
    number of variables — some variables are things you would normally
    expect to see in this kind of data, such as the original air date,
    the network, the IMDB rating, but also has some other interesting
    variables such as an indicator variable for which of the five main
    characters were captured during the show, the type of
    monster/creature that was caught by the gang etc.
-   For more information about the dataset, check out the official
    TidyTuesday page
    [here](https://github.com/rfordatascience/tidytuesday%5D) and
    navigate to the datasets section &gt; year 2021 &gt; week 29, here
    is the specific link to this week’s
    [dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md).

*For this week, we will visualize who eats [Scooby
Snacks](https://en.wikipedia.org/wiki/Scooby_Snacks) most consistently.*

-   My motivation for doing this is because five of the variables in the
    dataset (one each for Fred, Shaggy, Velma, Daphne and Scooby) were
    indicator variables that tell you if that character ate Scooby
    Snacks during the episode). This is a level of detail that I am
    impressed by and thought it would be interesting to visualize.
-   Another reason that I want to visualize this is because Scooby
    Snacks are commonly associated with the titular character (Scooby),
    or his owner (Shaggy). Sure, these two characters are known for
    their love of food, they are often offered these snacks as
    incentives to perform well, are sometimes shown eating truck-loads
    of Scooby Snacks, and are therefore top contenders for the sheer
    volume of Scooby Snacks eaten. But leaving quantity aside, do they
    also eat Scooby Snacks *consistently*? In how many episodes does the
    show depict Shaggy and Scooby eating these snacks? Are there any
    rivals? This is what I wanted to find out.

### Libraries and data import

Let’s get our libraries loaded first.

``` r
library(tidytuesdayR)
library(tidyverse)
library(lubridate)

library(kableExtra)
library(ggthemes)
library(scales)
```

Now let’s import the data. Data is from week 29, year 2021 (date:
2021-07-13)

Can either use the `tt_load()` function from the `tidytuesdayR` package
to do this - this is week 29 data from 2021, or can read in the already
downloaded .xlsx, `0scoobydoo.xlsx` (note: the “0” prefix in the
filename is mine).

``` r
# dat <- tidytuesdayR::tt_load(2021, week = 29)$scoobydoo # 603 observations
# (OR)
 library(openxlsx)
 dat <- read.xlsx("0scoobydoo.xlsx")
 dat$date_aired <- convertToDate(dat$date_aired, origin = "1900-01-01")
```

These are the variables I am interested in.

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
A sample of the indicator variables for Scooby Snacks
</caption>
<thead>
<tr>
<th style="text-align:right;">
index
</th>
<th style="text-align:left;">
snack\_fred
</th>
<th style="text-align:left;">
snack\_daphnie
</th>
<th style="text-align:left;">
snack\_velma
</th>
<th style="text-align:left;">
snack\_shaggy
</th>
<th style="text-align:left;">
snack\_scooby
</th>
<th style="text-align:left;">
number\_of\_snacks
</th>
<th style="text-align:left;">
date\_aired
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
1969-09-13
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1969-09-20
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
1969-09-27
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
1969-10-04
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
1969-10-11
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
1969-10-18
</td>
</tr>
</tbody>
</table>

The TRUE/FALSE in any of the `snack_` indicator variables, like
`snack_fred`, or `snack_daphnie`, tells you if that character was
featured eating Scooby Snacks in that particular episode. I initially
also wanted to look at the `number_of_snacks` variable and combine that
with the indicators to get the exact number of Scooby Snacks eaten in
each episode for each character, but decided against it for two reasons:
firstly, in some rows, two or more characters have the indicator set to
TRUE (in which case I wouldn’t be able to determine from
`number_of_snacks` how the snacks are distributed among the characters),
and secondly, some rows have the `number_of_snacks` set to character
descriptions like *“several”*, *“1 box”*, or *“wheel barrel full”*. So,
I will be focusing on the indicator variables instead.

I basically want to:

-   Convert the TRUE and FALSE in the five snack columns to 1s and 0s.
-   Extract year from date\_aired.
-   For each character, get a cumulative sum of the number of times they
    were featured eating Scooby Snacks for every year that this dataset
    represents (1969-2021)

# Data prep for figure

I do this all in one code chunk.

``` r
#------------------------------------------
# Cumulative number of Scooby Snacks eaten.
#-------------------------------------------
datafig <- dat %>%
  
  
            # Choose columns of interest, add year, convert TRUE/FALSE to 1/0
            select(names(.)[grepl("snack", names(.))], date_aired)        %>%
  
            mutate(year = year(date_aired))                               %>%
  
            mutate_at(c("snack_fred", "snack_daphnie", "snack_velma",
                        "snack_shaggy", "snack_scooby"), 
                      function(x) {ifelse(x == TRUE, 1, 0)})              %>%
  
            # Get the total number of episodes that show the character eating
            # Scooby Snacks for each year
            group_by(year)                                                %>%
  
  
            summarize(Shaggy = sum(snack_shaggy),
            Scooby = sum(snack_scooby),
            Fred = sum(snack_fred),
            Daphne = sum(snack_daphnie),
            Velma = sum(snack_velma))                                     %>%
  
  
           # Make data long: used pivot_longer() here as my old favorite, gather()
           # is no longer under active development and I want to push myself to learn
           # newer functions
            pivot_longer(cols = !(year), 
                         names_to = "snack_eater", 
                         values_to = "how_many")                          %>%
  
  
           # Make the snack_eater a factor, and calculate 
           # the total number of episodes that show the character eating snacks across all years
           # the cumulative number of episodes that show the character eating snacks, year by year
           mutate(snack_eater = factor(snack_eater))                      %>%
           group_by(snack_eater)                                          %>%
           mutate(total_eatings = sum(how_many))                          %>%
           group_by(snack_eater)                                          %>%
           mutate(cumu_episodes_eaten = cumsum(how_many))                 %>%
           ungroup()                                                      %>%
  
  
           # Rearrange factors so that the one who is shown eating snacks most frequently shows up
           # as the first item in the legend
           mutate(snack_eater = fct_reorder(snack_eater, -total_eatings)) %>%
  
           # Inspect the dataset at this point, and add indicators to help you
           # annotate the figure later.
           mutate(daph_annotationpts = ifelse(year %in% c(1984, 2021) & snack_eater == "Daphne", 1, 0),
                  shaggy_annotationpts = ifelse(year %in% c(2005, 2021) & snack_eater == "Shaggy", 1, 0))
```

Glimpse data at this point.

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Sample of data prepped for figure (note: two annotation variables not
shown
</caption>
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:left;">
snack\_eater
</th>
<th style="text-align:right;">
how\_many
</th>
<th style="text-align:right;">
total\_eatings
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1969
</td>
<td style="text-align:left;">
Shaggy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:right;">
1970
</td>
<td style="text-align:left;">
Shaggy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:right;">
1972
</td>
<td style="text-align:left;">
Shaggy
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:right;">
1973
</td>
<td style="text-align:left;">
Shaggy
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:right;">
1976
</td>
<td style="text-align:left;">
Shaggy
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
43
</td>
</tr>
</tbody>
</table>

-   `how_many` tells you how many episodes showed the character eating
    Scooby Snacks in the `year` of interest. For example, Shaggy was
    shown eating Scooby Snacks in just one episode in all of 1969.
-   `total_eatings` tells you the total number of episodes (between
    1969-2021) that show the character eating Scooby Snacks. For
    example, Shaggy was shown eating Scooby Snacks in 43 episodes in
    total.
-   `cumu_episodes_eaten` is just the cumulative count of `how_many` for
    each `year`.

# Prepare figure

-   We use the prepped data and our annotation variables to prepare a
    line chart of the cumulative number of episodes that show a
    character eating Scooby Snacks for each year from 1960 to 2021.
-   Some surprising numbers emerge.

``` r
#------------------------------------------------------------------------------------------------------
# Note: found two useful links that may help in future plots

# Wonderful one stop solution for color palettes:
# https://github.com/EmilHvitfeldt/r-color-palettes
# Another awesome resource to interactively learn to customize the myriad options available in ggplot2:
# https://ggplot2tor.com/theme
#-------------------------------------------------------------------------------------------------------

fig <- ggplot(data = datafig, 
               aes(x = year, 
                   y = cumu_episodes_eaten, 
                   group = snack_eater, 
                   col = snack_eater)) +
  
        geom_line(size = 1.0) +

        # Added these color values for lines using the Economist palette. 
        # library(scales)
        # show_col(economist_pal()(5))
        # Use red to identify the first line
        scale_color_manual(values = c("red", "#6794a7", "#014d64", "#7ad2f6", "#01a2d9")) +
  
        # name of legend
        labs(color ="Character") +
        # legend position
        theme_bw() +
        theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
        # axes labels
        labs(title = "Should we rename Scooby Snacks to Daphne Snacks?",
             subtitle = "Of the five characters in the show, Daphne is the one that is shown eating Scooby Snacks most consistently.",
         x = "Year",
         y = "Cumulative count of number of episodes \nthat show character eating Scooby Snacks",
         caption = "Data source: Kaggle, plummye, Sara Stoudt | TidyTuesdays | @LakshmanBalaji9") +
  
  
        #-------------
        # Annotations
        #-------------

        # Daphne start annotation and point
        annotate("text", 
              x=1976, 
              y=15*1.16, 
              label="Daphne takes the lead in 1984. \nAt this point, 15 episodes have \nfeatured her eating Scooby Snacks", 
              color="black", 
              size= 3, 
              angle=0, 
              fontface="plain", 
              hjust=0) +
  
        # Daphne red point
        geom_point(data = datafig[datafig$daph_annotationpts == 1, ],
                 size = 3,
                 color = "red") +
  
  
        # Shaggy start annotation and point  
        annotate("text", 
              x=2006.5, 
              y=14*1.16, 
              label="In 2005, Shaggy begins to break out.\nThe cumulative number of episodes \nshowing him eating Scooby Snacks \nat this point was just 13, but it climbs \nsharply in following years.", 
              color="black", 
              size= 3, 
              angle=0, 
              fontface="plain", 
              hjust=0) +

        # Shaggy red point  
         geom_point(data = datafig[datafig$shaggy_annotationpts == 1, ],
                size = 3,
                color = "#6794a7") +
  
        # End comments (top right)
         annotate("text", 
              x=c(2021, 2021),
              y=c(49, 43), 
              label=c("n = 49", "n = 43"), 
              color= c("red", "#6794a7"), 
              size= c(3, 3), 
              angle=0, 
              fontface=c("plain", "plain"), 
              hjust= -0.25) +
  
           annotate("text", 
              x=2019, 
              y=49, 
              label="As of 2021, Daphne still leads Shaggy by 6 episodes", 
              color="black", 
              size= 3, 
              angle=0, 
              fontface="bold", 
              hjust= 1) +

          # Irony
          annotate("text", 
              x=2000, 
              y=2.2, 
              label="Ironically, the character for whom the snack is named \nisn't shown eating it in a lot of episodes", 
              color="black", 
              size= 3, 
              angle=0, 
              fontface="plain", 
              hjust= 0.5)
```

View figure and save.

``` r
fig
```

![](1_scoobydoo-week29year2021_files/figure-gfm/view%20figure-1.png)<!-- -->

``` r
ggsave("3fig_scoobydoo_snacks.png", fig, width = 13.8, height = 7.47, units = "in")
```

### Conclusions

-   Daphne is the surprise winner of this contest. In the past 50 years,
    she has been shown eating Scooby Snacks in a total of 49 episodes.
    She first took the lead in 1984, and has never been beaten since
    then.
-   Shaggy’s Scooby Snacks eating took off sharply in 2005, but it still
    hasn’t been enough to catch up to Daphne.
-   Ironically, Scooby, for whom the snack is named, is shown eating it
    the least number of times.
-   However, bear in mind this does not adjust for quantity. Shaggy and
    Scooby could have well consumed a couple of truck loads of snacks in
    their episodes and surpassed Daphne.
