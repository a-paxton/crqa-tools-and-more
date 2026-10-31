#### Tutorial: Continuous cross-recurrence ####
#
# These brief exercises will walk you through performing continuous cross-recurrence
# quantification analysis using manual parameter searches.
#
# For continuous recurrence quantification analysis, see the 
# `tutorial-continuous_recurrence.R` file in this repository.
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

#### 3. Cross-recurrence quantification analysis ####

######## 3a. Cross-recurrence parameter setting ########

# decide Theiler window parameter
cross_rec_tw = 0

######## 3b. Determine delay ########

# determine delay for x
cross_ami_x = mutual(circle_x,
                     lag.max = 800)

# determine delay for y
cross_ami_y = mutual(circle_y,
                     lag.max = 800)

# visualize your AMI results
plot(cross_ami_x)
plot(cross_ami_y)

# figure out the delays for each
chosen_delay_x = 164
chosen_delay_y = 124

# select your delay from the AMI data -- remember
cross_chosen_delay = max(c(chosen_delay_x, chosen_delay_y))
cross_remaining_mutual_info_x = cross_ami_x[cross_chosen_delay + 1]
cross_remaining_mutual_info_y = cross_ami_y[cross_chosen_delay + 1]

######## 3c. Determine embedding parameter ########

# set maximum max for both
cross_max_embedding = 10

# determine embedding
cross_fnn_x = false.nearest(circle_x,
                            m = cross_max_embedding,
                            d = cross_chosen_delay,
                            t = cross_rec_tw)

# determine embedding
cross_fnn_y = false.nearest(circle_y,
                            m = cross_max_embedding,
                            d = cross_chosen_delay,
                            t = cross_rec_tw)

# visualize your FNN results
plot(cross_fnn_x)
plot(cross_fnn_y)

# figure out the embeddings for each
chosen_embedding_x = 4
chosen_embedding_y = 4

# select your embedding dimension from the FNN results
cross_chosen_embedding = max(c(chosen_embedding_x, 
                               chosen_embedding_y))

######## 3d. Select radius and run CRQA ########

# set additional varables
cross_normalize = 0 # rescaling the input -- no need to do this if we've z-scaled already
cross_rescale = 1 # distance matrix rescaling option -- let's choose mean-rescaling
cross_radius = .25 # radius -- try different ones! what happens?

# run CRQA 
cross_rec_analysis = crqa(ts1 = rescale_circle_x, 
                          ts2 = rescale_circle_y,
                          delay = cross_chosen_delay, 
                          embed = cross_chosen_embedding, 
                          radius = cross_radius,
                          normalize = cross_normalize, 
                          rescale = cross_rescale,
                          mindiagline = 2,
                          minvertline = 2, 
                          tw = cross_rec_tw, 
                          whiteline = FALSE,
                          recpt=FALSE,
                          datatype = "continuous",
                          method = "crqa")

######## 3e. Create the cross-recurrence plot ########

# use the standard plotting functions
plot_rp(cross_rec_analysis$RP, par)

######## 3f. Inspect the CRQA metrics ########

# take a look at the quantification metrics for CRQA across x- and y-axis movement
cross_rec_analysis$RR # rate of recurrence
cross_rec_analysis$DET # % determinism
cross_rec_analysis$NRLINE # total number of lines on the plot
cross_rec_analysis$maxL # maximum line length on plot
cross_rec_analysis$L # average line length on plot
cross_rec_analysis$ENTR # entropy
cross_rec_analysis$rENTR # normalized entropy
cross_rec_analysis$LAM # laminarity
cross_rec_analysis$TT # trapping time

