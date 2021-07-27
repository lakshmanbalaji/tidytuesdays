The Olympics
================
Lakshman Balaji
7/27/2021

# Background - the Olympic Medals dataset

-   This week’s *tidytuesday* plot comes from
    [rgriffin](https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results?select=noc_regions.csv)
    on Kaggle. This data was compiled from the information available at
    [www.sports-reference.com](http://www.sports-reference.com)
-   The [TidyTuesday Github
    page](https://github.com/rfordatascience/tidytuesday) has more
    information about this dataset
    [here](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/readme.md).
-   For this week’s analysis, I will be looking at *the time lag between
    when a sport appeared first in the Olympics, and when the women’s
    version of the sport subsequently appeared*. Note: this data is only
    for the years 1896-2016, so some of these sports may have debuted
    much earlier than 1896 for men, thus underestimating the time lag
    for women.
-   Note: I do not include sports that have only had participants from
    either only men or women. For example, rhythmic gymnastics, which
    has only ever seen participation from women, or baseball, which has
    had only male participants, are not shown in the data.

# Load libraries

``` r
library(tidytuesdayR)
library(tidyverse)
library(ggforce)          # to draw ellipses
library(extrafont)        # load fonts
library(kableExtra)       # display tables
# loadfonts(device="win") # register fonts for Windows bitmap output
# fonts()                 # check available fonts
```

# Load data

This is *week 31, year 2021* data. (date: 2021-07-21)

``` r
olympics <- tidytuesdayR::tt_load(2021, week = 31)$olympics

# alternatively ,load excel worksheet
# library(openxlsx)
# olympics <- read.xlsx("0olympics.xlsx")
```

# Explore data and clean

What sports are represented in this dataset?

``` r
sports <- data.frame(unique(olympics$event))                         %>% 
          arrange(unique.olympics.event.)                            %>%
          mutate(sportname = str_split(unique.olympics.event., " ")) %>%
          mutate(sportname2 = word(unique.olympics.event., 1, sep = fixed(' ')))

# list of the unique events
unique(sports$sportname2) 
```

    ##  [1] "Aeronautics"   "Alpine"        "Alpinism"      "Archery"      
    ##  [5] "Art"           "Athletics"     "Badminton"     "Baseball"     
    ##  [9] "Basketball"    "Basque"        "Beach"         "Biathlon"     
    ## [13] "Bobsleigh"     "Boxing"        "Canoeing"      "Cricket"      
    ## [17] "Croquet"       "Cross"         "Curling"       "Cycling"      
    ## [21] "Diving"        "Equestrianism" "Fencing"       "Figure"       
    ## [25] "Football"      "Freestyle"     "Golf"          "Gymnastics"   
    ## [29] "Handball"      "Hockey"        "Ice"           "Jeu"          
    ## [33] "Judo"          "Lacrosse"      "Luge"          "Military"     
    ## [37] "Modern"        "Motorboating"  "Nordic"        "Polo"         
    ## [41] "Racquets"      "Rhythmic"      "Roque"         "Rowing"       
    ## [45] "Rugby"         "Sailing"       "Shooting"      "Short"        
    ## [49] "Skeleton"      "Ski"           "Snowboarding"  "Softball"     
    ## [53] "Speed"         "Swimming"      "Synchronized"  "Table"        
    ## [57] "Taekwondo"     "Tennis"        "Trampolining"  "Triathlon"    
    ## [61] "Tug-Of-War"    "Volleyball"    "Water"         "Weightlifting"
    ## [65] "Wrestling"

Need to simplify some of the labels.

For example:

    * need to fix Alpinism and combine with Alpine skiing.
    * change beach to beach volleyball
    * change cross to cross country skiing 
    * Figure to figure skating ...etc

``` r
# clean labels
sports <- sports %>% 
          mutate(sportname3 = sportname2) %>%
          mutate(sportname3 = str_replace(sportname3, "Alpinism", "Alpine"))             %>%
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

# list of the unique events: there are 64 unique events
# length(unique(sports$simplifiedname))

# merge back with main dataset
olympics <- olympics %>%
            left_join(sports)

# Glimpse a sample of the cleaned event labels
olympics %>%
select(event, simplifiedname) %>%
head(n = 5) %>%
kable(caption = "Sample of cleaned event names")
```

<table>
<caption>
Sample of cleaned event names
</caption>
<thead>
<tr>
<th style="text-align:left;">
event
</th>
<th style="text-align:left;">
simplifiedname
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Basketball Men’s Basketball
</td>
<td style="text-align:left;">
Basketball
</td>
</tr>
<tr>
<td style="text-align:left;">
Judo Men’s Extra-Lightweight
</td>
<td style="text-align:left;">
Judo
</td>
</tr>
<tr>
<td style="text-align:left;">
Football Men’s Football
</td>
<td style="text-align:left;">
Football
</td>
</tr>
<tr>
<td style="text-align:left;">
Tug-Of-War Men’s Tug-Of-War
</td>
<td style="text-align:left;">
Tug-Of-War
</td>
</tr>
<tr>
<td style="text-align:left;">
Speed Skating Women’s 500 metres
</td>
<td style="text-align:left;">
Skating (Speed)
</td>
</tr>
</tbody>
</table>

Now, for each of the 64 simplified events, group by gender, and find
earliest year that the event occurred in.

``` r
# find first year of event for men and women
discrepancy <- olympics %>%
               group_by(simplifiedname, sex) %>%
               summarize(firstyear = min(year))

# make wide and calculate difference in first yerar of event between men and women
discrepancy.w <- discrepancy %>%
                 pivot_wider(names_from = sex, values_from = firstyear) %>%
                 mutate(fmgap = M - F) %>%
                 arrange(fmgap)

# join fmgap variable to discrepancy
discrepancy.dat <- merge(discrepancy, 
                         discrepancy.w[, c("simplifiedname", "fmgap")], 
                         by = "simplifiedname", all.x = TRUE)
# create a new df of sports in which only women or only men participated
# both both have never participated
unique.sports <- discrepancy.dat %>% filter(is.na(fmgap))
# collapse discrepancy.dat into just one row (since each row represents a
# difference between men and women, with negative numbers indicating the 
# number of years women waited before first participating in the sport)
discrepancy.dat <- discrepancy.dat %>% 
                   filter(sex == "M" & !(is.na(fmgap))) %>%
                   select(simplifiedname, fmgap) %>%
                   arrange(fmgap)

# remove clutter
rm(discrepancy, discrepancy.w)

# of the 64 unique events, 49 have participation from both men and women, 
# 15 have participation from either only men or women

mean(discrepancy.dat$fmgap)
```

    ## [1] -36.36735

``` r
# from 49 sports, women waited an average of 36 years to join men in Olympic sports.


# Glimpse a sample of this dataset
discrepancy.dat %>%
head(n = 10) %>%
kable(col.names = c("eventname", "yearswaitedbywomen") )
```

<table>
<thead>
<tr>
<th style="text-align:left;">
eventname
</th>
<th style="text-align:right;">
yearswaitedbywomen
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rugby
</td>
<td style="text-align:right;">
-116
</td>
</tr>
<tr>
<td style="text-align:left;">
Boxing
</td>
<td style="text-align:right;">
-108
</td>
</tr>
<tr>
<td style="text-align:left;">
Wrestling
</td>
<td style="text-align:right;">
-108
</td>
</tr>
<tr>
<td style="text-align:left;">
Weightlifting
</td>
<td style="text-align:right;">
-104
</td>
</tr>
<tr>
<td style="text-align:left;">
Polo - Water
</td>
<td style="text-align:right;">
-100
</td>
</tr>
<tr>
<td style="text-align:left;">
Football
</td>
<td style="text-align:right;">
-96
</td>
</tr>
<tr>
<td style="text-align:left;">
Ski Jumping
</td>
<td style="text-align:right;">
-90
</td>
</tr>
<tr>
<td style="text-align:left;">
Cycling
</td>
<td style="text-align:right;">
-88
</td>
</tr>
<tr>
<td style="text-align:left;">
Modern Pentathlon
</td>
<td style="text-align:right;">
-88
</td>
</tr>
<tr>
<td style="text-align:left;">
Bobsleigh
</td>
<td style="text-align:right;">
-78
</td>
</tr>
</tbody>
</table>

Now create the figure.

``` r
# change data values to positive as it doesn't matter in this case, i.e,
# there are no events where women started participating before men
discrepancy.dat$fmgap <- discrepancy.dat$fmgap*(-1)

# order factors
discrepancy.dat$simplifiedname <- factor(discrepancy.dat$simplifiedname)
discrepancy.dat$simplifiedname <- fct_reorder(discrepancy.dat$simplifiedname,
                                              discrepancy.dat$fmgap)



# plot
p <- 
     #plot base
     ggplot(discrepancy.dat, 
            aes(x=simplifiedname, y=fmgap)) + 
  
     # points
     geom_point(col="#ffffff", size=3) +  
  
     # dotted lines
     geom_segment(aes(x=simplifiedname, xend=simplifiedname, 
                      y=min(fmgap), yend=120), 
                  linetype="dashed", size=0.1, colour="#ffffff", alpha = 0.4) +   
  
     # Y axes breaks (becomes x axis later with coord flip)
     scale_y_continuous(breaks= seq(0, 120, by = 20)) +
  
     # labels 
     labs(title="The Waiting Game for Women Olympians", 
          subtitle="Looking at data from 49 types of Olympic sporting events from 1896 to 2016, women have had to wait ~ 36 years \non average before joining men in the Olympics in each event.", 
          caption="Data source: Kaggle and www.sports-reference.com | TidyTuesdays | @LakshmanBalaji9", 
          x=" ", 
          y="Years elapsed before Olympic sport became available for women to participate in") +  
  

    # flip axes                 
     coord_flip() +
  
    # geom ellipse from ggforce (https://rdrr.io/cran/ggforce/man/geom_mark_ellipse.html)
  
    # ellipse for wrestling
     geom_ellipse(aes(x0 = 49, y0 = 116, # center of ellipse
                      a = 2, b = 3,      # major and minor axes
                      angle = 0.25*pi),  # tilt angle in radians
                  color = "#ffffff", alpha = 0.75) +
  
    # curved arrow for wrestling
    geom_curve(aes(x = 48, y = 114.2, xend = 40, yend = 105),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "#ffffff", alpha = 0.75) +
    # text annotation for wrestling
    annotate("text", 
              x= 38,  # pick up where the geom_curve left off
              y= 103.5, # pick up where the geom_curve left off
              label="    The first men's rugby event occured in 1900. \nThe first women's rugby event occured in 2016.", 
              color= "#ffffff", 
              alpha = 0.75,
              # size = 4 , 
              angle = 0, 
              fontface="italic", 
              family = "Candara",
              hjust=0.5) +
  
    # ellipse for equal sports
     geom_ellipse(aes(x0 = 9, y0 = 0, 
                      a = 10, b = 2.5, 
                      angle = 1*pi), 
                  color = "#ffffff", alpha = 0.75) +  
  
    # curved arrow for equal sports
    geom_curve(aes(x = 11.2, y = 2.55, xend = 9.5, yend = 20),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "#ffffff", alpha = 0.75) +
  
    # text annotation for equal sports
    annotate("text", 
              x= 10,  # pick up where the geom_curve left off
              y= 21, # pick up where the geom_curve left off
              label="In these Olympic sports, women started participating at the same time as men.", color= "#ffffff", 
              alpha = 0.75,
              # size = 4 , 
              angle = 0, 
              fontface="italic", 
              family = "Candara",
              hjust= 0) +

  
    # theming
     theme(plot.background =  element_rect(fill = "#9e6f00", colour="#9e6f00"),
           panel.background = element_rect(fill = "#9e6f00", colour="#9e6f00"),
           legend.position="none",
           axis.ticks.y = element_blank(),
           axis.ticks.x = element_line(color = "#ffffff"),
           axis.text = element_text(colour = "#ffffff", size=10, hjust = 0.5, family= "Candara"),
           axis.title = element_text(colour = "#ffffff", size=12, hjust = 0.5, family= "Candara"),
           plot.title = element_text(colour = "#ffffff", size=20, hjust = 0.5, face = "bold", family = "Candara"),
           plot.subtitle = element_text(colour = "#ffffff", size=14, hjust = 0.5, face = "italic", family = "Candara"),
           plot.caption = element_text(colour = "#ffffff", size=8, hjust = 1, family = "Candara"),
           plot.margin = unit(c(0.6, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())



# save image
ggsave("2olympicswomenswait.jpg", p, width = 14.5, height = 9)
```
