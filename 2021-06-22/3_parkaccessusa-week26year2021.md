Park Access USA 2020
================
Lakshman Balaji
6/24/2021

# Background

-   We are going to use a *tidytuesday* dataset sourced from [*The Trust
    for Public
    Land*](https://www.tpl.org/parks-and-an-equitable-recovery-parkscore-report).

-   This data contains information from the 100 largest cities in the
    US, on measures such as number of dog parks per 100,000 residents,
    spending on parks per resident etc.

-   Each row represents a city, and each city can have multiple rows,
    corresponding to different years in which the measures were
    collected (a longitudinal structure, years from 2012-2020).

-   For more information about the dataset, check out the official
    TidyTuesday page
    [here](https://github.com/rfordatascience/tidytuesday%5D) and
    navigate to the datasets section &gt; year 2021 &gt; week 26, here
    is the specific link to this
    [dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-22/readme.md).

-   Since I am writing this at the start of summer and the heat is
    making me obsess over water, *we are going to look at the top ten
    cities in the contiguous United States for the number of
    splashgrounds per 100,000 residents*.

-   My motivation for doing this is because some of the tweets that I
    saw (for example, [this
    one](https://twitter.com/MDeBoltC/status/1407171788652507202/photo/1)
    and [this other
    one](https://twitter.com/marcjaffee_/status/1407496896554340353/photo/1))
    used sigmoid curves to great effect which I liked. I tracked down
    the source at David Joberg’s
    [Github](https://github.com/davidsjoberg/ggbump) and saw a great
    [application](https://raw.githubusercontent.com/davidsjoberg/ggbump/master/man/figures/ranking_gdpr.png)
    of these curves to depict numerical data on a map from several
    countries. I wanted to try and learn this to add to my toolbox, and
    hence this week’s plot.

### Libraries and data import

Let’s get our libraries loaded first.

``` r
# access tidytuesday data
library(tidytuesdayR)

# tidyverse
library(tidyverse)

# format output tables
library(kableExtra)

# spatial data manipulation
library(tidygeocoder)
library(sf)
library(spData)


# load ggbump and other related packages
library(pacman)
pacman::p_load(BBmisc, hablar, ggbump, feather, janitor, lubridate)


# Add an image to ggplot
library(png)
library(grid)
```

Now let’s import the data. Either use the `tt_load()` function from the
`tidytuesdayR` package to do this - this is week 26 data from 2021, or
can read in the already downloaded .xlsx, `0parks.xlsx` (note: the “0”
prefix in the filename is mine). Note: in this document, I have used the
package. If you read in the .xlsx, you may have to do some additional
formatting before proceeding with the analysis.

``` r
# source data from tidytuesdayR package
parks <- tidytuesdayR::tt_load('2021-06-22')$parks 
# alternative
# library(openxlsx)
# parks <- read.xlsx("0parks.xlsx")


# I always like to add an input sort order
parks<- parks %>% mutate(order = 1:nrow(.))
```

View a sample of the data.

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Glimpse the data
</caption>
<thead>
<tr>
<th style="text-align:left;">
city
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
splashground\_data
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Minneapolis
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
4.0
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington, D.C.
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
4.1
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Paul
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
1.3
</td>
</tr>
<tr>
<td style="text-align:left;">
Arlington, Virginia
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
2.2
</td>
</tr>
<tr>
<td style="text-align:left;">
Cincinnati
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
4.5
</td>
</tr>
<tr>
<td style="text-align:left;">
Portland
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
2.1
</td>
</tr>
</tbody>
</table>

### Data preparation

-   In this section, I basically strive to geocode each city, and assign
    each city to a state.
-   Though I only need to look at the 10 cities that have the highest
    number of splashgrounds, I do this for the full data because I
    wanted all the 100 cities to be assigned to states, in case I decide
    to come back later and look at the data in a different way.
-   I use a combination of manual assignment + Josh O’ Brien’s
    [excellent
    function](https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r)
    to assign states.
-   See annotations in the chunk for details, or feel free to just run
    this section and skip the details.

``` r
#----------------
# Idiosyncracies
#----------------
# Charlotte/Meckenburg County is the same as Charlotte, fix this.
parks <- parks %>% mutate(city = ifelse(city == "Charlotte/Mecklenburg County", "Charlotte", city))
# Washington DC is spelled two different ways, fix tthis.
parks <- parks %>% mutate(city = ifelse(city == "Washington, D.C.", "Washington, DC", city))
# Also notice, Arlington is a city that has the State name appended to it.
# Store positions where Arlington VA and Arlington TX occur.
arlva <- which(parks$city == "Arlington, Virginia")
arltx <- which(parks$city == "Arlington, Texas")



#---------------------
# Geocoding
#---------------------
# get a df of unique cities out from parks: 100 such cities
cities <- data.frame(city = unique(parks$city)) %>% mutate(sortorder = 1:nrow(.))
# Take out the two Arlingtons and store in a separate dataset (for geocoding purposes)
cities.exc <- cities %>% filter(grepl("Arlington", city))
cities <- cities %>% filter(grepl("Arlington", city) == FALSE)
# for the two Arlingtons, separate city and state
cities.exc <- cities.exc %>% 
               separate(., 
                        "city", 
                        into = c("city", "state"), 
                        sep = ',')
# geocode the two datasets separately and merge them back together
# I did this because the geocode function expects either just the city
# argument, or both the city and state argument.
cities.geocoded <- tidygeocoder::geocode(cities, 
                                         city = city)        # provide just the city argument
cities.exc.geocoded <- tidygeocoder::geocode(cities.exc, 
                                             city = city, 
                                             state = state)  # provide city and state
# rbind
city <- rbind(cities.geocoded,
              cities.exc.geocoded[, c("city", "sortorder", "lat", "long")]) %>%
        arrange(sortorder) %>%
        select(-sortorder) %>%
        select(long, lat, city)
# remove clutter
rm(list = ls()[grepl("cities", ls())])


#-------------------------------------------
# Assign states
# Josh O Brien's lonlat_to_state() function
#------------------------------------------

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


# apply function:
states <- lonlat_to_state(city)
city <- city %>% mutate(state = states)
# notice odd values for Toledo. It's been assigned to 
# a Toledo in Spain.
city$long[city$city == "Toledo"] <- -83.537817
city$lat[city$city == "Toledo"] <- 41.652914
# notice missing values:
# city %>% filter(is.na(state))
# populate manually
city$state[which(city$city == "Anchorage")] <- "Alaska"
city$state[which(city$city == "Honolulu")] <- "Hawaii"
city$state[which(city$city == "Toledo")] <- "Ohio"
city$state[which(city$city == "Laredo")] <- "Texas"
# add state abbreviations to city
city$abb <- state.abb[match(city$state, state.name)]
city$abb[city$state == "District of Columbia"] <- "DC"
# remove clutter
rm(lonlat_to_state, states)
#-----------------------------------------#


#---------------------
# Add back in 
# and handle
# the Arlingtons
#---------------------


# Add back in to main dataset
# But make sure to exclude the 2 rows 
# from city that have city = "Arlington" and 
# state = "Virginia" or "Texas"
citysub <- city %>% filter(!(city == "Arlington"))
# merge citysub with parks
parks <- merge(parks, citysub,
                  by = "city",
                  all.x = TRUE) %>%
           arrange(order)
# Add the long, lat, city and state info
# back in for arlington, VA, and arlington, TX rows
parks$city[arlva]  <- "Arlington"
parks$state[arlva] <- "Virginia"
parks$long[arlva]  <- city$long[city$city == "Arlington" & city$state == "Virginia"]
parks$lat[arlva]  <- city$lat[city$city == "Arlington" & city$state == "Virginia"]
parks$city[arltx]  <- "Arlington"
parks$state[arltx] <- "Texas"
parks$long[arltx]  <- city$long[city$city == "Arlington" & city$state == "Texas"]
parks$lat[arltx]  <- city$lat[city$city == "Arlington" & city$state == "Texas"]


# remove clutter
rm(arltx, arlva, citysub)
```

-   At this point, your environment should have just two objects: the
    `parks` input dataset and another dataset called `city` that has the
    100 geocoded cities along with their states.

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Sample of our city dataset
</caption>
<thead>
<tr>
<th style="text-align:right;">
long
</th>
<th style="text-align:right;">
lat
</th>
<th style="text-align:left;">
city
</th>
<th style="text-align:left;">
state
</th>
<th style="text-align:left;">
abb
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
-106.65099
</td>
<td style="text-align:right;">
35.08410
</td>
<td style="text-align:left;">
Albuquerque
</td>
<td style="text-align:left;">
New Mexico
</td>
<td style="text-align:left;">
NM
</td>
</tr>
<tr>
<td style="text-align:right;">
-117.93349
</td>
<td style="text-align:right;">
33.83403
</td>
<td style="text-align:left;">
Anaheim
</td>
<td style="text-align:left;">
California
</td>
<td style="text-align:left;">
CA
</td>
</tr>
<tr>
<td style="text-align:right;">
-149.89485
</td>
<td style="text-align:right;">
61.21631
</td>
<td style="text-align:left;">
Anchorage
</td>
<td style="text-align:left;">
Alaska
</td>
<td style="text-align:left;">
AK
</td>
</tr>
<tr>
<td style="text-align:right;">
-77.08416
</td>
<td style="text-align:right;">
38.89040
</td>
<td style="text-align:left;">
Arlington
</td>
<td style="text-align:left;">
Virginia
</td>
<td style="text-align:left;">
VA
</td>
</tr>
<tr>
<td style="text-align:right;">
-97.10562
</td>
<td style="text-align:right;">
32.70194
</td>
<td style="text-align:left;">
Arlington
</td>
<td style="text-align:left;">
Texas
</td>
<td style="text-align:left;">
TX
</td>
</tr>
<tr>
<td style="text-align:right;">
-84.39026
</td>
<td style="text-align:right;">
33.74899
</td>
<td style="text-align:left;">
Atlanta
</td>
<td style="text-align:left;">
Georgia
</td>
<td style="text-align:left;">
GA
</td>
</tr>
</tbody>
</table>

-   Now, we write a function (`topncity()`) that allows us to group the
    data by any numeric metric that we want, for any year, and give us
    the top ten cities for that metric.

``` r
# function to map out whatever variable you want for whichever year you want.
# takes 6 arguments: 
#                   city : a dataframe of 100 biggest cities in the US, with cols long, lat, cityname, 
#                          statename, abb
#                  parks : the dataset for this week's tidytuesday with all the variables of interest
#                   year : by default set to 2020, but can set to other years in the data, like 2012, 2013, ..2020
#                    var : variable that you want to visualize, by default, is "splashground_data". Needs to be numeric.
#                      n :   by default is 10, returns the top 10 cities for the var for the year. For example
#                           if you run with defaults, will return the top 10 cities with highest per capita 
#                           splashgrounds in the year 2020. Can set to 1, 2, ...100.
#               exclude : TRUE by default. excludes states which are not part of contiguous US (Hawaii 
#                          and Alaska in this example). 
#                          can say FALSE to include those two states.
topncity <- function(city, 
                   parks,
                   yr = 2020,
                   var = "splashground_data",
                   n = 10,
                   exclude = TRUE) { 
  
  
  if (exclude == TRUE) {  # if exclude is TRUE, remove Hawaii and Alaska from consideration
    
    citym <- city %>% filter(!(state %in% c("Hawaii", "Alaska")))
    
  }
  
  if (exclude == FALSE) { # if exclude is FALSE, include Hawaii and Alaska in the list of considered cities
    
    citym <- city
    
  }
  
 
  # left dataset is the dataset of cities (has long, lat, city, state, and abb)
  left <- citym
  # right dataset is the dataset of tidytuesday with multiple rows for each year, and multiple variables
  # we filter to select year of interest, and choose only the city, state, year and variable of interest
  right <- parks %>% 
    filter(year == yr) %>% 
    select(city, state, year, !!(sym(var)))
  # merge both datasets
  output <- merge(left, right, by = c("city", "state"), all.x = TRUE) 
  # arrange by descending variable of interest and sort to return first n rows
  output <- output %>% 
    arrange(-(!!(sym(var)))) %>% 
    filter(row_number() <= n) %>%
    data.frame(.)
  
  
  # Add a column that says "United States", will be useful for a join later
  output$country <- "United States"
  return(output)
  
}
```

-   To get the top 10 cities for `splashground_data` in `year` = 2020,
    apply the `topncity()` function with defaults.

``` r
# apply function with defaults
splash <- topncity(city, 
                  parks)
# glimpse 
splash %>%
head(10) %>%
kbl(caption = "Top 10 cities for splashgrounds", colnames = c("city", "state", "long", "lat", "abb", "year", "splashground_data")) %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Top 10 cities for splashgrounds
</caption>
<thead>
<tr>
<th style="text-align:left;">
city
</th>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
long
</th>
<th style="text-align:right;">
lat
</th>
<th style="text-align:left;">
abb
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
splashground\_data
</th>
<th style="text-align:left;">
country
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Cleveland
</td>
<td style="text-align:left;">
Ohio
</td>
<td style="text-align:right;">
-81.69344
</td>
<td style="text-align:right;">
41.50516
</td>
<td style="text-align:left;">
OH
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
10.6
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston
</td>
<td style="text-align:left;">
Massachusetts
</td>
<td style="text-align:right;">
-71.05829
</td>
<td style="text-align:right;">
42.36025
</td>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
10.3
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh
</td>
<td style="text-align:left;">
Pennsylvania
</td>
<td style="text-align:right;">
-79.99009
</td>
<td style="text-align:right;">
40.44169
</td>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago
</td>
<td style="text-align:left;">
Illinois
</td>
<td style="text-align:right;">
-87.62442
</td>
<td style="text-align:right;">
41.87556
</td>
<td style="text-align:left;">
IL
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Tulsa
</td>
<td style="text-align:left;">
Oklahoma
</td>
<td style="text-align:right;">
-95.99291
</td>
<td style="text-align:right;">
36.15568
</td>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Des Moines
</td>
<td style="text-align:left;">
Iowa
</td>
<td style="text-align:right;">
-93.60467
</td>
<td style="text-align:right;">
41.59103
</td>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
7.4
</td>
<td style="text-align:left;">
United States
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
-74.00602
</td>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:left;">
NY
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Louisville
</td>
<td style="text-align:left;">
Kentucky
</td>
<td style="text-align:right;">
-85.75941
</td>
<td style="text-align:right;">
38.25424
</td>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia
</td>
<td style="text-align:left;">
Pennsylvania
</td>
<td style="text-align:right;">
-75.16353
</td>
<td style="text-align:right;">
39.95272
</td>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:left;">
United States
</td>
</tr>
<tr>
<td style="text-align:left;">
Henderson
</td>
<td style="text-align:left;">
Nevada
</td>
<td style="text-align:right;">
-114.98262
</td>
<td style="text-align:right;">
36.03011
</td>
<td style="text-align:left;">
NV
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
5.5
</td>
<td style="text-align:left;">
United States
</td>
</tr>
</tbody>
</table>

``` r
# applying function to another variable, changing defaults.
# srcenter <- topncity(city, parks, yr = 2017, var = "rec_sr_data", n = 5, exclude = TRUE)


# Note, function fails if input variable is not numeric
# nearpark <- topncity(city, parks, yr = 2020, var = "pct_near_park_data", n = 10, exclude = TRUE)
```

We now have the `splash` dataset that contains the top ten cities for
splashgrounds.

# Mapping

-   The goal for today is to try creating an image similar to this
    [one](https://raw.githubusercontent.com/davidsjoberg/ggbump/master/man/figures/ranking_gdpr.png),
    but using just one country (the US) instead of multiple European
    countries, and using cities within the US, rather than the center of
    the country, as the centroid.
-   The code for this figure is displayed
    [here](https://github.com/davidsjoberg/tidytuesday/blob/master/2020w17/2020w17_skript.R).
-   I will basically be adapting/changing this for my figure, and using
    plenty of annotations to explain the parts of the code to myself.
-   This plot will consist of five components
    1.  *A shape*: An outline of the US, plotted using `geom_sf()`
    2.  *Points*: Points representing the 10 cities with the highest
        number of splashgrouns, plotting using `geom_point()`
    3.  *Curves/sigmoids*: Sigmoid lines proceeding from the points to
        the line segments.
    4.  *Line segments/bars*: Normalized line segments, meant to mimic
        bar plots. They begin where the sigmoid curves end, stretching
        horizontally to the right, with length proportional to the
        number of splashgrounds per 100,000 persons.
    5.  *Filler segment*: A vertical line segment meant to create a gap
        between the sigmoid curves and the bar plot line segments.

### Step one: load contiguous US shape file and points: base data

-   Fire up a file of US states from `spData`, combine the states
    together to get an overall US shape, and join the 10 `splash` cities
    to it.
-   This helps us get the first two components, the shape and the
    points.

``` r
sdf_dat <- spData::us_states   %>%     # loads sf object of contiguous US with state boundaries
           sf::st_union()      %>%     # combine all the states together to get just the outline of the US. This is now a sfc object.
           sf::st_as_sf()      %>%               # convert again to sf.
           mutate(country = "United States") %>% # Add a column to help join the splash data to this object.
           left_join(splash, by = "country")

# Note: at this stage, you could plot the sdf_dat object, and you would only
# get an overall outline of the US.

# Glimpse the data
sdf_dat %>%
head(n = 10) %>%
kbl(caption = "Cities joined to US geography- column X indicates outline of US") %>%
kable_classic(full_width = F, html_font = "Cambria")
```

<table class=" lightable-classic" style="font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Cities joined to US geography- column X indicates outline of US
</caption>
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
city
</th>
<th style="text-align:left;">
state
</th>
<th style="text-align:right;">
long
</th>
<th style="text-align:right;">
lat
</th>
<th style="text-align:left;">
abb
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
splashground\_data
</th>
<th style="text-align:left;">
x
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Cleveland
</td>
<td style="text-align:left;">
Ohio
</td>
<td style="text-align:right;">
-81.69344
</td>
<td style="text-align:right;">
41.50516
</td>
<td style="text-align:left;">
OH
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
10.6
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Boston
</td>
<td style="text-align:left;">
Massachusetts
</td>
<td style="text-align:right;">
-71.05829
</td>
<td style="text-align:right;">
42.36025
</td>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
10.3
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Pittsburgh
</td>
<td style="text-align:left;">
Pennsylvania
</td>
<td style="text-align:right;">
-79.99009
</td>
<td style="text-align:right;">
40.44169
</td>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Chicago
</td>
<td style="text-align:left;">
Illinois
</td>
<td style="text-align:right;">
-87.62442
</td>
<td style="text-align:right;">
41.87556
</td>
<td style="text-align:left;">
IL
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Tulsa
</td>
<td style="text-align:left;">
Oklahoma
</td>
<td style="text-align:right;">
-95.99291
</td>
<td style="text-align:right;">
36.15568
</td>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Des Moines
</td>
<td style="text-align:left;">
Iowa
</td>
<td style="text-align:right;">
-93.60467
</td>
<td style="text-align:right;">
41.59103
</td>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
7.4
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:left;">
New York
</td>
<td style="text-align:right;">
-74.00602
</td>
<td style="text-align:right;">
40.71273
</td>
<td style="text-align:left;">
NY
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Louisville
</td>
<td style="text-align:left;">
Kentucky
</td>
<td style="text-align:right;">
-85.75941
</td>
<td style="text-align:right;">
38.25424
</td>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
6.0
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Philadelphia
</td>
<td style="text-align:left;">
Pennsylvania
</td>
<td style="text-align:right;">
-75.16353
</td>
<td style="text-align:right;">
39.95272
</td>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:left;">
Henderson
</td>
<td style="text-align:left;">
Nevada
</td>
<td style="text-align:right;">
-114.98262
</td>
<td style="text-align:right;">
36.03011
</td>
<td style="text-align:left;">
NV
</td>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
5.5
</td>
<td style="text-align:left;">
MULTIPOLYGON (((-81.68524 2…
</td>
</tr>
</tbody>
</table>

The base US map is now ready.

### Step two: develop rankings for the rest of the figure.

-   These rankings are used to orient the sigmoid curves, the line
    segments, and the filler segments relative to each other and the
    base US map. See annotations for details.

``` r
#----------
# ranking
#----------

ranking <- sdf_dat %>%
           # pull out the lat and long columns for the 10 cities of interest
           select(long, lat) %>%
  
           # add the values required to place the different components of the chart
           bind_cols(tibble(
             
           # 1. the vertical positions of the bars. This also determines the termination heights 
           # for the sigmoid lines which start from the cities. We do this by ranking the variable
           # of interest, and then assigning them a position between 25 and 50 on the Y axis such
           # their relative order is maintained. The normalize function helps us do this.
           bars_vert_pos = normalize(rank(sdf_dat$splash), range = c(25, 50), method = "range"),
           # 2. the name of the city and state from which each sigmoid line is supposed to originate
           city = sdf_dat$city,
           state = sdf_dat$abb,
           # 3. IMPORTANT: arbitrary is a number that we pick, it is just a longitude coordinate to the right of the US, 
           # several subsequent values are determined from this. For example, we calculate a value called 
           # x_axis_start, which is just arbitrary + 10. The bar charts begin at x_axis_start. 
           # Also, the rightmost extent of the 
           # sigmoid lines are determined by x_axis_start - 0.2, (because the sigmoid lines will terminate
           # just a little before x_axis_start). If you increase arbitrary, the bars and sigmoid lines 
           # will shift toward the right.
           arbitrary = -60,
           # 4. x_axis_start: determined by arbitrary, just tells you where the sigmoid
           # curves end and where the bars begin
           x_axis_start = arbitrary + 10,
           # 5. IMPORTANT: this next measure, bars_end_right gives us the position of the finishing x coordinate for the bar chart for each city
           #               (in other words, determines the length of the bar chart)
           #               this measure is normalized: it varies from (start of bar chart) to whatever upper limit you set. 
           #               Say the upper limit is set to 100.
           #               and assume the lower limit, x_axis_start (start of bar chart) is 70
           #               assume the highest value of variable of interest is 2.2 and the lowest is 0.0
           #               then this will assign a bars_end_right of 100 to the city with variable = 2.2 (length = 30)
           #                                 and a bars_end_right of 70 to the city with variable = 0.0  (length = 0)
           #               in the below lines, I set range = c(-50, -15), so the maximum possible length of a bar is 
           #               35 units to the right, and the minimum is 0 units. The first ctiy gets assigned the length of 35
           #               units and the last city gets assigned 0 units. All other cities fall between these two lengths.
           bars_end_right = normalize(sdf_dat$splash, range = c(first(x_axis_start), -15), method = "range"),
           
           
           
           # Add labels near the end of the sigmoid lines (the beginning of the bar charts)
           val_txt = paste0(format(sdf_dat$splash, digits = 0, nsmall = 2)),
           # For the first city alone, explain what the metric is in full (that it is the number of splashgrounds per 100,000 people)
           # For the rest of the cities, just show the number
           val_txt2 = if_else(city == "Cleveland", paste0(val_txt, "\nsplashgrounds per\n100,000 residents"), val_txt)))


# ranking is already an sf object 
# Assign coordinates/projection using the st_as_sf() function.
# set projection to WGS
ranking <- st_as_sf(ranking, 
                    coords = c("long", "lat"), 
                    crs = 4326)
# If you wanted to be totally sure, assign the same projection to sdf_dat also (sdf_dat is already an sf dataframe so no need to convert to sf).
# Here's how you would do it.
 sdf_dat <- st_transform(sdf_dat, crs = 4326)
# Add coordinates back to ranking and overwrite
# ranking <- cbind(ranking, st_coordinates(ranking))
#ranking <- ranking %>% rename(long = X.1, lat = Y.1) # consider renaming later



#------------------------
# add calculated values
# back to base data
#-----------------------

# Just add the vertical positions of the bars from the 
# ranking dataset to the sdf_dat dataset
# We need this info to tell the geom_sigmoids originating from the
# geom_points() which vertial
# level they are supposed to end at.
sdf_dat <- cbind(sdf_dat, ranking$bars_vert_pos) 
sdf_dat <- sdf_dat %>% rename(bars_vert_pos = ranking.bars_vert_pos)
```

### Step three: plot image

``` r
img <-  png::readPNG("1splatter.png")
g <- rasterGrob(img, interpolate=TRUE)

# Used https://htmlcolorcodes.com/color-picker/
# to pick colors for background/labels/continuous gradient etc.




p <-
  
  ggplot() + 
  
  
  # base plot: use just the base plot.
  geom_sf(data = sdf_dat, size = .3, fill = "transparent", color = "grey50") +
  
  
  # Sigmoid from city to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = long, y = lat ,
                   xend = x_axis_start - .2, yend = bars_vert_pos,
                   group = city, color = bars_vert_pos), 
               alpha = .6, smooth = 10, size = 1)   + 
  

  # Length of the bars
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = bars_vert_pos, 
                   xend = bars_end_right, yend = bars_vert_pos, 
                   color = bars_vert_pos), alpha = .6, size = 1, 
               lineend = "round") + 
  
  # Line interrupting sigmoid and barchart
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = 25, xend = x_axis_start, yend = 53), 
               alpha = .6, size = 1.3, color = "#32311e") +
  
  
  # Point to identify city of origin
  geom_point(data = ranking, 
             aes(x = long, y = lat,
                 color = bars_vert_pos), size = 2) +
  
  # city labels
  geom_text(data = ranking, 
            aes(x = x_axis_start-.5, y = bars_vert_pos, 
                label = paste0(city, ", ", state), color = bars_vert_pos), 
            hjust = 1, size = 4, nudge_y = .5) +
  
  # Numeric variable labels
  geom_text(data = ranking, aes(x = bars_end_right, 
                                y = bars_vert_pos, 
                                label = val_txt2, color = bars_vert_pos), 
            hjust = 0, size = 3.5, nudge_x = .25) +
  
  # Image insert (watercolor splash)
  annotation_custom(g, 
                    xmin=-107, 
                    xmax=-95,
                    ymin= 54, 
                    ymax= 59) +
  
  #  Themes
  coord_sf(clip = "off") +
  scale_fill_gradient(high = "#a3c5f6", low = "#445164") +
  scale_color_gradient(high = "#a3c5f6", low = "#445164") +
  theme_void() +
  labs(title = "Where are the splashgrounds?",
       subtitle = "Top ten cities in the US as of 2020 for number of \nsplashgrounds per 100,000 residents",
       caption = "Source: TidyTuesday Year 2021 Week 26 | plot inspired by davidsjoberg's vignette on ggbump: https://github.com/davidsjoberg/ggbump") + 
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill= "#32311e"),
        plot.caption = element_text(color = "gray50"),
        plot.title = element_text(color = "gray50", size = 20, face = "bold"),
        plot.subtitle = element_text(color = "gray50", size = 12.5))


p
```

![](2parkaccessusa-week26year2021_files/figure-gfm/step%20three:%20plot%20image-1.png)<!-- -->

``` r
# save size settings that I used
ggsave("trial.png", p, width = 19.1, height = 8.44, units = "in")
```
