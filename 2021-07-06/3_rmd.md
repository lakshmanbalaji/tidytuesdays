International Independence Days
================
Lakshman Balaji
7/06/2021

# Background

-   We are going to use a *tidytuesday* dataset dealing with
    International Independence Days sourced from [*Wikipedia / Isabella
    Velasquez*](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-06/readme.md).
-   This data contains one row for each country, and tells you which
    year the country declared independence, and what the holiday is
    called and so on.
-   For this week, I will make a simple boxplot of the number of years
    elapsed since independence from the UK for each country. I picked
    the UK because this country was the most successful colonial power
    in terms of the territory it controlled (…the sun never sets on the
    British Empire?)

### Libraries and data import

Load libaries

``` r
library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(ggrepel)
library(ggdark)
```

# Process data

Load data. This is week 28, year 2021 data.

``` r
# week 28, year 2021 data
tuesdata <- tidytuesdayR::tt_load(2021, week = 28)$holidays
# or, alternatively, use these lines
# library(openxlsx)
# tuesdata <- read.xlsx("0_data.xlsx")

# grab everyone who go independence from the UK
ukleavers <- tuesdata %>%
filter(str_detect(independence_from, "(United Kingdom)|(Britain)")) %>%
filter(!is.na(year_of_event)) %>%
arrange(year_of_event) %>%
mutate(country = ifelse(country == "Gambia, The", "Gambia",
                 ifelse(country == "Bahamas, The", "Bahamas", country))) %>%
mutate(continent = countrycode(sourcevar = country, 
                               origin = "country.name",
                               destination = "continent"),
       age = round(as.numeric(difftime(Sys.Date(), date_parsed, units = "days"))/365.25, 2)) %>%
mutate(continent= factor(continent, levels = c("Europe", "Asia", "Africa", "Americas", "Oceania")))
```

# Plot

``` r
# exclude US and Brazil as they skew the plot. Mention in footnote.
ukleavers2 <- ukleavers %>% filter(!(country %in% c("United States", "Brazil")))


p<- ggplot(data = ukleavers2, aes(x = continent, y = age)) +
  
  
geom_boxplot(outlier.shape=NA, # avoid plotting outliers twice (geom_jitter plots outliers as well)
             width = 0.3,
             position = position_dodge(width = 15),
             aes(color = continent)) + 
  
  
geom_jitter(aes(x = continent, 
                y = age, 
                color = continent), 
            position = position_jitter(seed = 1234)) +
  

geom_text_repel(aes(label = country, color = continent), 
                max.overlaps = 50,
                position = position_jitter(seed = 1234),
                size = 3.5) +
  
scale_y_continuous(breaks = c(seq(40, 100, by = 10))) +
  
labs(y = "Years since independence",
     x = "Continent",
     title = "Gaining independence from the United Kingdom",
     subtitle = "Plot shows the number of years elapsed since independence from the UK for countries from each continent superimposed over a boxplot of median and interquartile ranges of years since independence. \nNote: US and Brazil omitted from chart (gained independence 245 and 198 years ago respectively).") +

  dark_theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
```

Plot image.

``` r
p
```

![](1_script_files/figure-gfm/plot%20image-1.png)<!-- -->

Save image.

``` r
ggsave("2plot.png", p, height = 8.11, width = 15.8 )
```
