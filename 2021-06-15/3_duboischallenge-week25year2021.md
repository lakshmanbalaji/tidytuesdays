DuBoisChallenge Tweets
================
Lakshman Balaji
6/18/2021

# Background

-   We are going to use a *tidytuesday* dataset of tweets that contain
    the hashtag \#DuboisChallenge
-   This data is for the February 2021 — May 2021 time period.
-   Each row represents a tweet by a user with this hashtag. We have
    information such as the username of the person that tweeted, the
    number of likes, retweets and quote-tweets that the tweet received
    and the location of the user.
-   For more information about the dataset, check out the official
    TidyTuesday page
    [here](https://github.com/rfordatascience/tidytuesday%5D) and
    navigate to the datasets section &gt; year 2021 &gt; week 25, here
    is the specific link to this
    [dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-15/readme.md).

<center>
*We are going to identify the number of distinct people who participated
from each state in the US and examine how much each state contributed to
total engagement for the \#DuboisChallenge*
</center>

-   My motivation for doing this is because from some of the
    [other](https://twitter.com/Jamie_Bio/status/1405556890243940354/photo/1)
    great
    [visualizations](https://twitter.com/sdepickere/status/1405525845242900501/photo/1)
    that I
    [saw](https://twitter.com/efranke7282/status/1405338394713116675/photo/3)
    on this topic, I noticed that there was a high concentration of
    tweets coming from just a few users. I think that if this were a
    challenge/campaign that I was running, *a lot of people tweeting
    fewer times* is something that I would prefer to *fewer people
    tweeting lots of times*, because it could arguably mean that more
    individuals directly engaged with the campaign, as opposed to
    passively following/consuming content that a few superstars tweeted
    out.

### Libraries and data import

Let’s get our libraries loaded first.

``` r
library(tidytuesdayR)
library(tidyverse)

library(kableExtra)

library(sf)
library(spData)

library(hrbrthemes)
```

Now let’s import the data. Can either use the `tt_load()` function from
the `tidytuesdayR` package to do this - this is week 25 data from 2021,
or can read in the already downloaded .xlsx, `0tweets.xlsx` (note: the
“0” prefix in the filename is mine).

``` r
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = FALSE)

 tweets <- tidytuesdayR::tt_load(2021, week = 25)$tweets # 445 observations/tweets
# (OR)
# library(openxlsx)
# tweets <- read.xlsx("1tweets.xlsx")

# Note row has a missing date-time, let's exclude that row.
tweets <- tweets %>% filter(!(is.na(datetime)))       # 444 tweets
```

# Prepare the data

## States

-   There’s a good chunk of the data that has non-missing `lat` and
    `long` columns (about 355 tweets, to be precise.)
-   We could use these to overlay points over a map of US state
    boundaries, and figure out to which state each tweeter belonged.
-   I use Josh O’ Brien’s [excellent
    function](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r)
    to do this.

``` r
# Add this to keep track of rows in input dataset
tweets$sortorder <- 1:nrow(tweets)


# identify rows that do not have missingness in lat (or long)
tweetsub1 <- tweets %>% filter(!is.na(lat))  
# create a dataset that has only 2 columns: long, and lat (in that order)
latlongs  <- tweetsub1 %>% select(long, lat) 


#------------------------------------------#
# Josh O Brien's lonlat_to_state() function
#-----------------------------------------#

## pointsDF: A data.frame whose first column contains longitudes and  whose second column contains latitudes.
## states:   An sf MULTIPOLYGON object with 50 states plus DC. Feel free to change this to other geographies
##             if necessary
## name_col: Name of a column in `states` that supplies the states' names.
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)

  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)

  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(sf::st_intersects(pts, states))
  state_names[ii]
}

#-----------------------------------------#

# apply function:
# we input the latlongs dataframe as our pointsDF. 
latlongs <- latlongs %>% mutate(state = lonlat_to_state(latlongs)) 
# add back to dataset with non-missing latlongs
tweetsub1 <- tweetsub1 %>% cbind(., latlongs["state"])
# remove clutter
rm(lonlat_to_state, latlongs)
```

Take a quick look at the data now.

``` r
tweetsub1 %>% 
select(lat, long, state) %>%
head() %>%
kbl(caption = "States assigned to tweets based on lat-longs") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
States assigned to tweets based on lat-longs
</caption>
<thead>
<tr>
<th style="text-align:right;">
lat
</th>
<th style="text-align:right;">
long
</th>
<th style="text-align:left;">
state
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
36.22061
</td>
<td style="text-align:right;">
-86.69564
</td>
<td style="text-align:left;">
Tennessee
</td>
</tr>
</tbody>
</table>

Note, at this stage, any tweet that didn’t get a state assigned either
didn’t have a `lat` or `long`, or originates from outside the US. We
could stop at this stage and visualize the data, but I want to go ahead
and try to see if I can assign states to tweets using the `location`
column in the data.

``` r
# identify rows that have missingness in lat (or long)
tweetsub2 <- tweets %>% filter(is.na(lat))  # 89 observations

# For these locations
tweetsub2  %>%   
arrange(location) %>%
select(location, lat, long) %>%
count(location) %>%
arrange(-n) %>%
kbl(caption = "Distribution of location for tweets without lat-longs") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Distribution of location for tweets without lat-longs
</caption>
<thead>
<tr>
<th style="text-align:left;">
location
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
iPhone: 34.704040,-86.722909
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
<Kevin.Elder@GCSU.edu>
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville, TN
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Forde-Obama Hall
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
New York, NY
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
California, USA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Caracas, Venezuela
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Distrito Federal, México
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
down in dey wid em
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Hurst, TX
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Jaboatão, PE, Brasil
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lil’ Rudyshire
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey, USA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
OAK / NYC / ATL / The World
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Querétaro, México
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Seattle, WA
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

Remember, these are tweets that did not have a lat-long, and we are
looking at the locations provided by the tweeters. Assuming that the
tweeter’s `location` provided doesn’t change from tweet to tweet, we can
assign states to these tweets by manual inspection.

``` r
tweetsub2 <-   tweetsub2  %>%  
               mutate(state = 
                      # the lat long provided in the iPhone tweets are from Alabama
                      ifelse(str_detect(location, "iPhone"),'Alabama', 
                      # the email address is for a school in Georgia
                      ifelse(str_detect(location, "@"), 'Georgia',     
                      ifelse(str_detect(location, "TN"), 'Tennessee',
                      # Columbia University, NY
                      ifelse((str_detect(location, "Forde-Obama")|str_detect(location, "New York")), 'New York', 
                      ifelse(str_detect(location, "California"),'California',
                      ifelse(str_detect(location, "TX"), 'Texas',
                      ifelse(str_detect(location, "Jersey"), 'New Jersey',
                      ifelse(str_detect(location, "Seattle"),'Washington',
                      # everything else looks like it's outside the US
                      NA 
                      ))))))))
                      )


tweetsub2  %>%   
arrange(location) %>%
select(location, lat, long, state) %>%
count(location, state) %>%
arrange(-n) %>%
kbl(caption = "States assigned to tweets without lat-longs") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
States assigned to tweets without lat-longs
</caption>
<thead>
<tr>
<th style="text-align:left;">
location
</th>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
iPhone: 34.704040,-86.722909
</td>
<td style="text-align:left;">
Alabama
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
<Kevin.Elder@GCSU.edu>
</td>
<td style="text-align:left;">
Georgia
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville, TN
</td>
<td style="text-align:left;">
Tennessee
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Forde-Obama Hall
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
New York
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
New York, NY
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
California, USA
</td>
<td style="text-align:left;">
California
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Caracas, Venezuela
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Distrito Federal, México
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
down in dey wid em
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Hurst, TX
</td>
<td style="text-align:left;">
Texas
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Jaboatão, PE, Brasil
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lil’ Rudyshire
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey, USA
</td>
<td style="text-align:left;">
New Jersey
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
OAK / NYC / ATL / The World
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Querétaro, México
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Seattle, WA
</td>
<td style="text-align:left;">
Washington
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

Now, we can use `rbind()` to combine `tweetsub1` and `tweetsub2` to get
a dataset that has the `state` assigned to as many US tweets as
possible. We call this `tweets2` to avoid overwriting the input dataset.

``` r
tweets2 <- rbind(tweetsub1, tweetsub2)
tweets2 %>% 
arrange(sortorder) %>%
filter(row_number() %in% 50:55) %>%
select(lat, long, location, state) %>%
kbl(caption = "States assigned to as many US tweets as possibe") %>%
kable_classic(full_width = F, html_font = "Cambria") %>%
footnote(general = "Notice the US locations with lat longs have been assigned to their respective states. Notice the iPhone has been assigned to Alabama. Nairobi has been assigned NA.")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<caption>
States assigned to as many US tweets as possibe
</caption>
<thead>
<tr>
<th style="text-align:right;">
lat
</th>
<th style="text-align:right;">
long
</th>
<th style="text-align:left;">
location
</th>
<th style="text-align:left;">
state
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
40.712728
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
43.074761
</td>
<td style="text-align:right;">
-89.38376
</td>
<td style="text-align:left;">
Madison, WI
</td>
<td style="text-align:left;">
Wisconsin
</td>
</tr>
<tr>
<td style="text-align:right;">
40.712728
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:left;">
New York
</td>
</tr>
<tr>
<td style="text-align:right;">
36.220608
</td>
<td style="text-align:right;">
-86.69564
</td>
<td style="text-align:left;">
Nashville, TN
</td>
<td style="text-align:left;">
Tennessee
</td>
</tr>
<tr>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
iPhone: 34.704040,-86.722909
</td>
<td style="text-align:left;">
Alabama
</td>
</tr>
<tr>
<td style="text-align:right;">
-1.303169
</td>
<td style="text-align:right;">
36.82606
</td>
<td style="text-align:left;">
Nairobi, Kenya
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<span style="font-style: italic;">Note: </span>
</td>
</tr>
<tr>
<td style="padding: 0; " colspan="100%">
<sup></sup> Notice the US locations with lat longs have been assigned to
their respective states. Notice the iPhone has been assigned to Alabama.
Nairobi has been assigned NA.
</td>
</tr>
</tfoot>
</table>

## Distinct users and engagement percentages

-   We now count the number of distinct usernames who tweeted about the
    \#Duboischallenge from each state.
-   The person who tweeted is identified by the `username` variable.

``` r
# drop non US rows/locations with no geocodes
tweets2 <- tweets2 %>% filter(!(is.na(state))) # 323 rows left (about 72% of the input data)


# group by state and count distinct users
summary <- tweets2 %>%
           group_by(state) %>%
           summarize(nusers = length(unique(username)))

# glimpse data
summary %>% 
arrange(-nusers) %>%
head() %>%
kbl(caption = "Glimpse of the number of distinct tweeters in each state") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Glimpse of the number of distinct tweeters in each state
</caption>
<thead>
<tr>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
nusers
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
California
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Massachusetts
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Tennessee
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Texas
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota
</td>
<td style="text-align:right;">
5
</td>
</tr>
</tbody>
</table>

We define `engagement` as the sum of likes, retweets and quotes,
calculate this for each tweet, sum across the whole dataset, and then
identify the percent that each state contributed to `engagement`.

``` r
# engagements dataset
engagement <- tweets2 %>% 
              mutate(totalengage = sum(retweet_count, like_count, quote_count)) %>%
              rowwise() %>%
              mutate(contribengage = sum(retweet_count, like_count, quote_count)) %>%
              ungroup() %>% 
              group_by(state, totalengage) %>%
              summarize(contribengagestate = sum(contribengage)) %>%
              ungroup()
             
# sum
summary <- summary %>% 
       left_join(., engagement, by = "state", all.x = TRUE) %>%
       mutate(pctengage = round((contribengagestate/totalengage)*100, 2)) %>%
       select(state, nusers, contribengagestate, totalengage, pctengage) %>%
       arrange(-nusers)

# glimpse data
summary %>%
select(state, nusers, totalengage, pctengage) %>%
head() %>%
kbl(caption = "Glimpse of the number of distinct tweeters in each state + contribution of that state to total engagement") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Glimpse of the number of distinct tweeters in each state + contribution
of that state to total engagement
</caption>
<thead>
<tr>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
nusers
</th>
<th style="text-align:right;">
totalengage
</th>
<th style="text-align:right;">
pctengage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
25.71
</td>
</tr>
<tr>
<td style="text-align:left;">
California
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
30.36
</td>
</tr>
<tr>
<td style="text-align:left;">
Massachusetts
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
2.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Tennessee
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
9.85
</td>
</tr>
<tr>
<td style="text-align:left;">
Texas
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
2.06
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6129
</td>
<td style="text-align:right;">
3.13
</td>
</tr>
</tbody>
</table>

# Figure

-   Now, we are ready to make the figure of the number of distinct users
    in each state.
-   I will use a lollypop plot. I was inspired by the great code hosted
    at the [R Graph
    Gallery](https://www.r-graph-gallery.com/lollipop-plot.html)
-   I will use the dark-theme from the `hrbrthemes` package.
-   The size of the lollypop will represent the approximate percent
    contribution of that state to the total engagement across all
    states.

``` r
# make state a factor 
summary$state <- factor(summary$state, levels = summary$state)
```

``` r
p1 <- ggplot(data = summary, aes(x = state, y = nusers)) +
  
      # the sticks of the lollypops
      geom_segment(aes(x = state, xend = state,
                       y = 0, yend = nusers),
                       color = ft_cols$gray,
                       size = 1.1) +
                  
  
      # the head of the lollypops
      geom_point(aes(x = state, y = nusers),
                 col = ft_cols$peach) +
                    
 
      # labels
      labs(x = "States", 
      y = "Number of participants",
      title = "How many people joined in?",
      caption = "The number of people from each state who tweeted in the #DuboisChallenge") +
  
      # dark theme from hrbrthemes
       theme_ft_rc()+
  
      # orient lables in X axis at 90 degrees
       theme(axis.text.x = element_text(angle = 90)) 


p1
```

![](2duboischallenge-week25year2021_files/figure-gfm/figure-1.png)<!-- -->

Let’s try to include the engagement variable as well.

``` r
# prepare a coded version of the pctengage variable
summary$pctengagemodb <- ifelse(summary$pctengage < 1, 1, 
                          ifelse(summary$pctengage >= 1 & summary$pctengage <= 10, 2,
                          ifelse(summary$pctengage > 10 & summary$pctengage <= 20, 3,
                          ifelse(summary$pctengage >20 & summary$pctengage <= 30,  4,
                                                                                   5))))

p2 <- ggplot(data = summary, aes(x = state)) +
  
      # the sticks of the lollypops
      geom_segment(aes(x = state, xend = state,
                       y = 0, yend = nusers),
                       color =  ft_cols$gray, 
                       size = 1.1) +
                  
  
      # the head of the lollypops, set sizes manually
      geom_point(aes(x = state, y = nusers),
                 size = ifelse(summary$pctengagemodb == 1, 1.5,
                        ifelse(summary$pctengagemodb == 2, 4,
                        ifelse(summary$pctengagemodb == 4, 10, 11))),
                 color = ifelse(summary$pctengagemodb >= 4, ft_cols$light_blue,
                         ifelse(summary$pctengagemodb %in% c(2, 3), "goldenrod3", ft_cols$gray))) +
  
  
  
  
  
  
   #-------------------
   # manual annotations
   #-------------------
  
    # for New York
       annotate("text", 
              x=grep("New York", summary$state), 
              y=summary$nusers[which(summary$state=="New York")]*1.08, 
              label="NY's 21 tweeters contributed to ~26% of total engagement", 
              color=ft_cols$light_blue, 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
  
  
    # for California
     annotate("text", 
              x=grep("California", summary$state), 
              y=summary$nusers[which(summary$state=="California")]*1.3, 
              label="CA has less than half the number of tweeters that NY has", 
              color=ft_cols$light_blue, 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
       annotate("text", 
              x=grep("California", summary$state), 
              y=summary$nusers[which(summary$state=="California")]*1.2, 
              label="but contributes to  ~35% of total engagement", 
              color=ft_cols$light_blue, 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
 
  
       # for golden colors
       annotate("text", 
              x=grep("Penn", summary$state), 
              y=summary$nusers[which(summary$state=="Pennsylvania")]*1.38, 
              label="States in golden, like PA for instance, contributed to", 
              color="goldenrod3", 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
  
       annotate("text", 
              x=grep("Penn", summary$state), 
              y=summary$nusers[which(summary$state=="Pennsylvania")]*1.2, 
              label="between 1 and 10% of total engagement", 
              color="goldenrod3", 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
  
  
        # for grey colors
       annotate("text", 
              x=grep("Dela", summary$state), 
              y=summary$nusers[which(summary$state=="Delaware")]*2.55, 
              label="States with tiny grey bubbles, like DE", 
              color="gray", 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
  
       annotate("text", 
              x=grep("Dela", summary$state), 
              y=summary$nusers[which(summary$state=="Delaware")]*1.9, 
              label="contributed <1% to total engagement", 
              color="gray", 
              size=4 , 
              angle=0, 
              fontface="italic", 
              hjust=0) +
    
 
      # labels
      labs(x = "States", 
      y = "Number of distinct tweeters",
      title = "How many persons participated in the #DuBoisChallenge from each state?",
      subtitle = "The size of the bubble indicates the percent contribution of that state to overall engagement",
      caption = "Data source: #DuBois Challenge tweets, Anthony Starks & Allen Hillery Sekou Tyler|Tidy Tuesday Year 2021 Week 25") +
  
      # dark theme from hrbrthemes
       theme_ft_rc()+
  
      # orient labels in X axis at 90 degrees
       theme(axis.text.x = element_text(angle = 90)) 


p2
```

![](2duboischallenge-week25year2021_files/figure-gfm/print%20image-1.png)<!-- -->
