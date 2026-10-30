#### Tutorial: Continuous recurrence ####
#
# These brief exercises will walk you through performing continuous recurrence
# quantification analysis using manual parameter searches.
#
# For continuous cross-recurrence quantification analysis, see the 
# `tutorial-continuous_cross_recurrence.R` file in this repository.
#
# Code written by: A. Paxton (University of Connecticut)
# Date last modified: 30 October 2026

#### 1. Preliminaries ####

# prep the workspace  
rm(list=ls())

# load in libraries as needed
library(nonlinearTseries)
library(tseriesChaos)
library(dplyr)
library(crqa)
library(ggplot2)

# set working directory to appropriate location
setwd('./')

# read in the raw data
circle_x = read.table('./data/circlexML.txt') %>%
  .$V1
circle_y = read.table('./data/circleyML.txt') %>%
  .$V1

# rescaled data
rescale_circle_x = as.numeric(scale(circle_x))
rescale_circle_y = as.numeric(scale(circle_y))

#### 2. Plotting your data ####

# raw data
plot(circle_x)
plot(circle_y)

# rescaled data
plot(rescale_circle_x)
plot(rescale_circle_y)

#### 3. Recurrence quantification analysis ####

######## 3a. Recurrence parameter setting ########

# decide Theiler window parameter
rec_tw_analysis = 1

######## 3b. Determine delay ########

# determine delay
rec_ami = mutual(circle_x,
                 lag.max = 800)

# visualize your AMI results
plot(rec_ami)

# select your delay from the AMI data
rec_chosen_delay = 164
rec_remaining_mutual_info = rec_ami[rec_chosen_delay + 1] # confirm with this
# remember: the named vector starts with 0! -- so add 1

######## 3c. Determine embedding parameter ########

# determine embedding
rec_max_embedding = 10
rec_fnn = false.nearest(circle_x,
                        m=rec_max_embedding,
                        d=rec_chosen_delay,
                        t=rec_tw_analysis)

# visualize your FNN results
plot(rec_fnn)

# select your embedding dimension from the FNN data
rec_chosen_embedding = 4
rec_remaining_fnn = rec_fnn[,rec_chosen_embedding]

######## 3d. Select radius and run CRQA ########

# set additional varables
rec_normalize = 0 # rescaling the input -- no need to do this if we've z-scaled already
rec_rescale = 1 # distance matrix rescaling option -- let's choose mean-rescaling
rec_radius = .25 # radius -- try different ones! what happens?

# run RQA for analysis
rec_analysis = crqa::crqa(ts1 = rescale_circle_x, 
                          ts2 = rescale_circle_x,
                          delay = rec_chosen_delay, 
                          embed = rec_chosen_embedding, 
                          radius = rec_radius, 
                          normalize = rec_normalize, 
                          rescale = rec_rescale,
                          tw = rec_tw_analysis, 
                          whiteline = FALSE,
                          datatype = "continuous",
                          method = "rqa")

######## 3e. Create the recurrence plot ########

# set plotting-related theiler window
rec_tw_plot = 0

# run RQA for plotting
rec_rp = crqa::crqa(ts1 = rescale_circle_x, 
                    ts2 = rescale_circle_x,
                    delay = rec_chosen_delay, 
                    embed = rec_chosen_embedding, 
                    radius = rec_radius, 
                    normalize = rec_normalize, 
                    rescale = rec_rescale,
                    tw = rec_tw_analysis, 
                    whiteline = FALSE,
                    datatype = "continuous",
                    method = "rqa")

# use the standard plotting functions
plot_rp(rec_rp$RP)

######## 3f. Inspect the CRQA metrics ########

# take a look at the quantification metrics for CRQA across x- and y-axis movement
rec_analysis$RR # rate of recurrence
rec_analysis$DET # % determinism
rec_analysis$NRLINE # total number of lines on the plot
rec_analysis$maxL # maximum line length on plot
rec_analysis$L # average line length on plot
rec_analysis$ENTR # entropy
rec_analysis$rENTR # normalized entropy
rec_analysis$LAM # laminarity
rec_analysis$TT # trapping time