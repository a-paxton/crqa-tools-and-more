######## Tutorial: Detrended fluctation analysis ########
#
# These brief exercises will walk you through performing detrended fluctuation
# analysis (DFA). To do so, we'll use temperature data for Storrs (CT, USA)
# from 1888 until 2023, courtesy of the US National Oceanic and Atmospheric 
# Association. (I tried to get it from 1881, when UConn was founded in Storrs, 
# but---alas!---the earliest data for Storrs available from NOAA was 1888.)
#
# Code written by: A. Paxton (University of Connecticut)
# Date last modified: 13 April 2023

#### 1. Preliminaries ####

# prep the workspace
rm(list=ls())

# load in libraries as needed -- be sure to install these if you don't have them!
library(crqa)
library(ggplot2)
library(tidyr)
library(nonlinearTseries)

# set working directory to appropriate location and then read in our data
setwd('./')
storrs_climate_df = read.csv("./data/noaa-storrs_climate_data.csv")

# let's look at what we have, shall we?
head(storrs_climate_df)

# for this tutorial, let's just look at the temperature data
storrs_temp_df = storrs_climate_df %>% ungroup() %>%
  dplyr::select(STATION,
                DATE,
                TMAX,
                TMIN) %>%
  
  # convert DATE into something that's actually reading as a date
  mutate(DATE = as.Date(DATE))

# let's take a look at 10 years of minimum and maximum daily temp data
ggplot(slice(storrs_temp_df, 1:(365*10)),
       aes(x = DATE,
           y = TMAX)) +
  ggtitle("Maximum and minimum daily temperature in Storrs, CT, USA") +
  ylab('Temperature (F)') +
  xlab('Date (Year)') +
  geom_point(color = "red",
             alpha = .5) +
  geom_point(aes(y = TMIN), color = "blue",
             alpha = .5)

# now that we've inspected at least a bit of our data, let's look at our missing data
sum(is.na(storrs_temp_df$TMAX))
sum(is.na(storrs_temp_df$TMIN))
sum(is.na(storrs_temp_df$TMIN))/dim(storrs_temp_df)[1]

# we have a pretty small amount of data missing---about 2% from the dataset we're using,
# but recent work by López and colleagues (2021, https://doi.org/10.1016/j.cageo.2021.104794)
# suggests that DFA is robust to missing data at even higher rates that we found, so we'll
# use casewise deletion for this

# clear out missing from our dataset
storrs_temp_df = storrs_temp_df %>% ungroup() %>%
  drop_na(TMAX, TMIN)

# and we can move on to running DFA! first, let's choose max
storrs_max_dfa = nonlinearTseries::dfa(storrs_temp_df$TMAX,
                                       window.size.range = c(4,
                                                             floor(dim(storrs_temp_df)[1]/4)))
storrs_max_dfa

# and let's look at min
storrs_min_dfa = nonlinearTseries::dfa(storrs_temp_df$TMIN,
                                       window.size.range = c(4,
                                                             floor(dim(storrs_temp_df)[1]/4)))
storrs_min_dfa

# in both cases, we see two distinct scaling regimes---in other words,
# two regions that have very different slopes---divided at around 500

# let's see what the slope of the line is for the 4- to ~500-day windows.
# note that we limit our `fluctuation.function` and `window.sizes` to the same
# number of items in each vector, corresponding to the window sizes we're including
# in the first regime. to figure out what you want to include, you can look at the
# `storrs_max_dfa$fluctuation.function` and `storrs_max_dfa$window.sizes` vectors
# or even count the number of points on your plot. as with most things in programming,
# there are a bunch of ways that you could implement it---this is just one way!
observed_H_temp_max_faster = unname(lm(log(storrs_max_dfa$fluctuation.function[1:11]) ~ 
                                         log(storrs_max_dfa$window.sizes[1:11]))$coefficients[2])
observed_H_temp_max_faster
# at a (relatively) faster scale, max temperatures from 1888-2023 show very high persistence

# and now let's see what the slope of the line is for the ~500- to 11679-day windows (about 1.5y to 32y)
observed_H_temp_max_slower = unname(lm(log(storrs_max_dfa$fluctuation.function[11:20]) ~ 
                                         log(storrs_max_dfa$window.sizes[11:20]))$coefficients[2])
observed_H_temp_max_slower
# at a much slower scale, max temperatures from 1888-2023 are highly antipersistent

# now, you try! try the same process with the minimum temperature data. 
