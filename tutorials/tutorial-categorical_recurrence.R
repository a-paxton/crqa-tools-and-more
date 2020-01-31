######## Tutorial: Categorical recurrence ########
#
# These brief exercises will walk you through performing categorical recurrence
# quantification analysis, including for auto- and cross-recurrence.
#
# Note: There is a known problem with the ggplot-based plotting in which the
#       LOI/LOS appears broken or disjointed, rather than a perfectly straight 
#       diagonal. I'm hoping to have a fix for it at some time in the future.
#
# Code written by: A. Paxton (University of Connecticut)
# Date last modified: 30 January 2020

#### 1. Preliminaries ####

# prep the workspace
rm(list=ls())

# load in libraries as needed -- be sure to install these if you don't have them!
library(dplyr)
library(crqa)
library(ggplot2)

# set working directory to appropriate location
setwd('./')

# read in the data
poetic = read.table("./data/chickens-poetry-converted.txt", sep="\t") %>%
  .$V1
informative = read.table("./data/chickens-informative-converted.txt", sep="\t") %>%
  .$V1

#### 2. Plotting your data ####

# plot the informative sequences
informative_seq = qplot(informative,
                        x=seq_along(informative), geom="point") +
  geom_path(color="purple") +
  geom_point(color="purple") +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in informative text")
informative_seq

# plot the poetic sequences
poetic_seq = qplot(poetic,
                   x=seq_along(poetic), geom="point") +
  geom_path(color="orange") +
  geom_point(color="orange") +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in poetic text")
poetic_seq

# plot the informative histogram
informative_histogram = qplot(informative,
                              geom="histogram") +
  geom_histogram(fill="purple") +
  theme(legend.position="none") +
  xlab("Numeric-converted letter") + ylab("Frequency") +
  ggtitle("Character frequencies in informative text")
informative_histogram

# plot the poetic histogram
poetic_histogram = qplot(poetic,
                         geom="histogram") +
  geom_histogram(fill="orange") +
  theme(legend.position="none") +
  xlab("Numeric-converted letter") + ylab("Frequency") +
  ggtitle("Character frequencies in poetic text")
poetic_histogram

#### 3. Recurrence quantification analysis ####

######## 3a. Recurrence parameter setting ########

# set the Theiler window parameter for RQA (should be 1 to ignore LOI in RQA)
rec_theiler_window = 1

# set radius to be very small for categorical matches
rec_categorical_radius = .0001

######## 3b. Run recurrence quantification analysis ########

# run rqa over informative text
recurrence_analysis_informative = crqa(ts1=informative,
                                       ts2=informative,
                                       delay=0,
                                       embed=1,
                                       rescale=0,
                                       radius=rec_categorical_radius,
                                       normalize=0,
                                       mindiagline=2,
                                       minvertline=2,
                                       tw=rec_theiler_window)

# run rqa over poetic text
recurrence_analysis_poetic = crqa(ts1=poetic,
                                  ts2=poetic,
                                  delay=0,
                                  embed=1,
                                  rescale=0,
                                  radius=rec_categorical_radius,
                                  normalize=0,
                                  mindiagline=2,
                                  minvertline=2,
                                  tw=rec_theiler_window)

######## 3c. Create the recurrence plot ########

# Note: In order to get the line of identity to appear in these plots,
#       you must run another `crqa()` function call with a Thieler window
#       (`tw`) of 0 in order to preserve the line of identity in the plot.

# set the Theiler window parameter for RP (should be 0 to keep LOI in RP)
rec_tw_plotting = 0

# run rqa over informative with a Theiler window of 0 for plotting
recurrence_analysis_plot_informative = crqa(ts1=informative,
                                            ts2=informative,
                                            delay=0,
                                            embed=1,
                                            rescale=0,
                                            radius=rec_categorical_radius,
                                            normalize=0,
                                            mindiagline=2,
                                            minvertline=2,
                                            tw=rec_tw_plotting)

# run rqa over poetic with a Theiler window of 0 for plotting
recurrence_analysis_plot_poetic = crqa(ts1=poetic,
                                       ts2=poetic,
                                       delay=0,
                                       embed=1,
                                       rescale=0,
                                       radius=rec_categorical_radius,
                                       normalize=0,
                                       mindiagline=2,
                                       minvertline=2,
                                       tw=rec_tw_plotting)

# use crqa package's native plotting function
par = list(unit = 2, 
           labelx = "Letter", 
           labely = "Letter", 
           cols = "purple", 
           pcex = 1)
plotRP(recurrence_analysis_plot_informative$RP, par)

# use crqa package's native plotting function
par = list(unit = 2, 
           labelx = "Letter", 
           labely = "Letter", 
           cols = "orange", 
           pcex = 1)
plotRP(recurrence_analysis_plot_poetic$RP, par)

######## 3d. Inspect the RQA metrics ########

# take a look at the quantification metrics for informative text
recurrence_analysis_informative$RR # rate of recurrence
recurrence_analysis_informative$DET # % determinism
recurrence_analysis_informative$NRLINE # total number of lines on the plot
recurrence_analysis_informative$maxL # maximum line length on plot
recurrence_analysis_informative$L # average line length on plot
recurrence_analysis_informative$ENTR # entropy
recurrence_analysis_informative$rENTR # normalized entropy
recurrence_analysis_informative$LAM # laminarity
recurrence_analysis_informative$TT # trapping time

# take a look at the quantification metrics for poetic text
recurrence_analysis_poetic$RR # rate of recurrence
recurrence_analysis_poetic$DET # % determinism
recurrence_analysis_poetic$NRLINE # total number of lines on the plot
recurrence_analysis_poetic$maxL # maximum line length on plot
recurrence_analysis_poetic$L # average line length on plot
recurrence_analysis_poetic$ENTR # entropy
recurrence_analysis_poetic$rENTR # normalized entropy
recurrence_analysis_poetic$LAM # laminarity
recurrence_analysis_poetic$TT # trapping time

######## 3e. Use ggplot2 for plotting ########

# convert informative and poetic into dataframes for easier plotting
informative_df = data.frame(points = recurrence_analysis_plot_informative$RP@i,
                            loc = seq_along(recurrence_analysis_plot_informative$RP@i))
poetic_df = data.frame(points = recurrence_analysis_plot_poetic$RP@i,
                       loc = seq_along(recurrence_analysis_plot_poetic$RP@i))

# use ggplot2 to generate the informative RP
ggplot(informative_df,aes(x=points,
                          y=loc)) +
  geom_point(color="purple",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters)") + xlab("Time (in letters)") +
  ggtitle("Categorical recurrence quantification analysis of informative text")

# use ggplot2 to generate the poetic RP
ggplot(poetic_df,aes(x=points,
                     y=loc)) +
  geom_point(color="orange",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters)") + xlab("Time (in letters)") +
  ggtitle("Categorical recurrence quantification analysis of poetic text")

######## 4. Cross-recurrence ########

######## 4a. Cross-recurrence parameter setting ########

# decide Theiler window parameter
cross_theiler_window = 0

# set radius to be very small for categorical matches
cross_categorical_radius = .0001

######## 4b. Run cross-recurrence ########

# truncate informative to length of poetic
truncated_informative = informative[1:length(poetic)] 

# run cross recurrence over each
cross_recurrence_analysis = crqa(ts1=truncated_informative,
                                 ts2=poetic,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=cross_categorical_radius,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=cross_theiler_window)

######## 4c. Create the recurrence plot with standard plotting ########

# use the standard plotting functions
par = list(unit = 2, 
           labelx = "Letter for informative text", 
           labely = "Letter for poetic text", 
           cols = "red", 
           pcex = 1)
plotRP(cross_recurrence_analysis$RP, par)

######## 4d. Create the recurrence plot with ggplot2 ########

# convert cross-recurrence output into a dataframe for easier plotting
cross_rec_df = data.frame(points = cross_recurrence_analysis$RP@i,
                          loc = seq_along(cross_recurrence_analysis$RP@i))

# build the CRP
ggplot(cross_rec_df,aes(x=points,
                        y=loc)) +
  geom_point(color="red",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters) of poetic text") + xlab("Time (in letters) of informative text") +
  ggtitle("Categorical cross-recurrence quantification analysis\nof poetic and informative texts about chickens")

######## 4e. Create the diagonal recurrence profile ########

# specify the window size (from negative win_size to 0 to positive win_size)
win_size = 15

# create the DRP
chicken_drp = drpdfromts(ts1 = truncated_informative,
                         ts2 = poetic,
                         datatype="categorical",
                         ws = win_size,
                         delay=0,
                         embed=1,
                         rescale=0,
                         radius=cross_categorical_radius,
                         normalize=0,
                         mindiagline=2,
                         minvertline=2,
                         tw=cross_theiler_window)

# plot the DRP
qplot(y = chicken_drp$profile, 
      x = -win_size:win_size, 
      geom="line") +
  geom_line(color="red",size=1) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("% Recurrence") + xlab("Lag (in letters)") +
  ggtitle("Diagonal recurrence profile\nof poetic and informative texts about chickens")