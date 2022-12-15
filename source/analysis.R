library(tidyverse)
library(dplyr)
library(ggplot2)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary 
county_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# What state has the highest total population of Black prisoners from 1970 to 2018?
highest_black_population <- function() {
  h <- county_data %>%
    select(state, black_pop_15to64) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(black_pop_15to64 = sum(black_pop_15to64)) %>%
    filter(black_pop_15to64 == max(black_pop_15to64)) %>%
    pull(state)
  
  return(h)
}

highest_black_population()

# What state has the highest total population of Latinx prisoners from 1970 to 2018?
highest_latinx_population <- function() {
  h <- county_data %>%
    select(state, latinx_pop_15to64) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(latinx_pop_15to64 = sum(latinx_pop_15to64)) %>%
    filter(latinx_pop_15to64 == max(latinx_pop_15to64)) %>%
    pull(state)
  
  return(h)
}

highest_latinx_population()

# What state has the highest total population of Native prisoners from 1970 to 2018?
highest_native_population <- function() {
  h <- county_data %>%
    select(state, native_pop_15to64) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(native_pop_15to64 = sum(native_pop_15to64)) %>%
    filter(native_pop_15to64 == max(native_pop_15to64)) %>%
    pull(state)
  
  return(h)
}

highest_native_population()

# What state has the highest total population of AAPI prisoners from 1970 to 2018?
highest_aapi_population <- function() {
  h <- county_data %>%
    select(state, aapi_pop_15to64) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(aapi_pop_15to64 = sum(aapi_pop_15to64)) %>%
    filter(aapi_pop_15to64 == max(aapi_pop_15to64)) %>%
    pull(state)
  
  return(h)
}

highest_aapi_population()

# What state has the highest total population of White prisoners from 1970 to 2018?
highest_white_population <- function() {
  h <- county_data %>%
    select(state, white_pop_15to64) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(white_pop_15to64 = sum(white_pop_15to64)) %>%
    filter(white_pop_15to64 == max(white_pop_15to64)) %>%
    pull(state)
  
  return(h)
}

highest_white_population()


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

federal_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

get_year_jail_pop <- function() {
  s <- federal_data %>%
    select(year, total_jail_pop) %>%
    na.omit()
  return(s)   
}

#class(get_year_jail_pop())
get_year_jail_pop()


plot_jail_pop_for_us <- function() {
  p <- ggplot(data = get_year_jail_pop()) + 
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Growth of the U.S. Jail Population from 1970 to 2018", 
         x = "year",
         y = "total jail population")
  
  return(p)   
} 

plot_jail_pop_for_us()
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Jail Population by State 

county_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

get_jail_pop_by_states <- function(states) {
  c <- county_data %>% 
    filter(state %in% states) %>%
    group_by(year, state) %>%
    select(year, state, total_pop) %>%
    na.omit() %>%
    summarize(total_pop = sum(total_pop))
  return(c)   
}

get_jail_pop_by_states(c("WA", "OR", "CA", "FL"))

plot_jail_pop_by_states <- function(states) {
  p <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_pop, color = state)) + 
    labs(title = "Growth of the Jail Populations by State from 1970 to 2018", 
         x = "year",
         y = "total jail population")
  
  return(p)
}

plot_jail_pop_by_states(c("WA", "OR", "CA", "FL"))
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Race Comparison of Jail Populations

select_race_year <- function () {
  r <- county_data %>%
    select(year, black_jail_pop, white_jail_pop, latinx_jail_pop,
           native_jail_pop, aapi_jail_pop, other_race_jail_pop) %>%
    na.omit()
  
  return(r)
}

race_compare <- function() {
  g <- ggplot(as.data.frame(select_race_year())) +
    geom_point(mapping = aes(x = year, y = black_jail_pop, color= "black")) +
    geom_point(mapping = aes(x = year, y = white_jail_pop, color = "white")) +
    geom_point(mapping = aes(x = year, y = latinx_jail_pop, color = "latinx")) +
    geom_point(mapping = aes(x = year, y = native_jail_pop, color = "native")) +
    geom_point(mapping = aes(x = year, y = aapi_jail_pop, color = "aapi")) +
    geom_point(mapping = aes(x = year, y = other_race_jail_pop, color = "other")) +
    labs(title = "Comparison of Jail Populations by Race (1970-2018)", 
         x = "year",
         y = "jail population",
         color = "race")
  
  return(g)
}

race_compare()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map of Total Native Incarceration Rates by State

library(usdata)
county_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# wrangle data sets for join 

native_jail_pop <- county_data %>%
  select(state, native_pop_15to64) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(native_prison_pop = sum(native_pop_15to64))

View(native_jail_pop)

state_shape <- map_data("state")
state_shape$region = state2abbr(state_shape$region)
colnames(state_shape)[5] = "state"

View(state_shape)
##state2abbr function source: https://www.rdocumentation.org/packages/usdata/versions/0.2.0/topics/state2abbr

# function to join both data frames

joined_map_data <- function() {
  join <- left_join(state_shape, native_jail_pop, by = "state")
  
  return(join)
}

joined_map_data()

# define blank theme 
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank() 
  )


# function to plot map of native incarceration 

plot_map <- function() {
  m <- ggplot(as.data.frame(joined_map_data())) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = native_prison_pop),
      color = "white", size = .5) +
    coord_map() + 
    scale_fill_continuous(low = "Pink", high = "Purple") +
    labs(fill = "Native Incarceration Rates by U.S. States (1970-2018)") +
    blank_theme
  
  return(m)
}

plot_map()
#----------------------------------------------------------------------------#