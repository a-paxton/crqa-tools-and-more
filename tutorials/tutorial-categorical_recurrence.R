######## Tutorial: Categorical recurrence and cross-recurrence ########
#
# These brief exercises will walk you through performing categorical recurrence
# quantification analysis, including for auto- and cross-recurrence.
#
# Code written by: A. Paxton (University of Connecticut)
# Date last modified: 27 January 2025

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
  rename(word = V1) %>%
  tibble::rowid_to_column("seq_loc")
informative = read.table("./data/chickens-informative-converted.txt", sep="\t") %>%
  rename(word = V1) %>%
  tibble::rowid_to_column("seq_loc")

#### 2. Plotting your data ####

# plot the informative sequences
informative_seq = ggplot(data = informative,
                         aes(x = seq_loc,
                             y = word)) +
  geom_path(color="purple") +
  geom_point(color="purple") +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in informative text")
informative_seq

# plot the poetic sequences
poetic_seq = ggplot(data = poetic,
                    aes(x = seq_loc,
                        y = word)) +
  geom_path(color="orange") +
  geom_point(color="orange") +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in poetic text")
poetic_seq

# plot the informative histogram
informative_histogram = ggplot(data = informative,
                               aes(x = word)) +
  geom_histogram(fill="purple", bins = 30) +
  theme(legend.position="none") +
  xlab("Numeric-converted letter") + ylab("Frequency") +
  ggtitle("Character frequencies in informative text")
informative_histogram

# plot the poetic histogram
poetic_histogram = ggplot(data = poetic,
                          aes(x = word)) +
  geom_histogram(fill="orange", bins = 30) +
  theme(legend.position="none") +
  xlab("Numeric-converted letter") + ylab("Frequency") +
  ggtitle("Character frequencies in poetic text")
poetic_histogram

#### 3. Recurrence quantification analysis ####

######## 3a. Set parameters for recurrence ########

# set the Theiler window parameter for RQA (should be 1 to ignore LOI in RQA)
rec_tw_quantification = 1

# set radius to be very small for categorical matches
rec_categorical_radius = .0001

######## 3b. Run recurrence quantification analysis ########

# run rqa over informative text
recurrence_analysis_informative = crqa(ts1=informative$word,
                                       ts2=informative$word,
                                       delay=0,
                                       embed=1,
                                       rescale=0,
                                       radius=rec_categorical_radius,
                                       normalize=0,
                                       mindiagline=2,
                                       minvertline=2,
                                       tw=rec_tw_quantification)

# run rqa over poetic text
recurrence_analysis_poetic = crqa(ts1=poetic$word,
                                  ts2=poetic$word,
                                  delay=0,
                                  embed=1,
                                  rescale=0,
                                  radius=rec_categorical_radius,
                                  normalize=0,
                                  mindiagline=2,
                                  minvertline=2,
                                  tw=rec_tw_quantification)

######## 3c. Create the recurrence plot ########

# Note: In order to get the line of identity to appear in these plots,
#       you must run another `crqa()` function call with a Theiler window
#       (`tw`) of 0 in order to preserve the line of identity in the plot.

# set the Theiler window parameter for RP (should be 0 to keep LOI in RP)
rec_tw_plot = 0

# run rqa over informative with a Theiler window of 0 for plotting
recurrence_analysis_plot_informative = crqa(ts1=informative$word,
                                            ts2=informative$word,
                                            delay=0,
                                            embed=1,
                                            rescale=0,
                                            radius=rec_categorical_radius,
                                            normalize=0,
                                            mindiagline=2,
                                            minvertline=2,
                                            tw=rec_tw_plot)

# run rqa over poetic with a Theiler window of 0 for plotting
recurrence_analysis_plot_poetic = crqa(ts1=poetic$word,
                                       ts2=poetic$word,
                                       delay=0,
                                       embed=1,
                                       rescale=0,
                                       radius=rec_categorical_radius,
                                       normalize=0,
                                       mindiagline=2,
                                       minvertline=2,
                                       tw=rec_tw_plot)

# build our recurrence plots
plot_rp(recurrence_analysis_plot_informative$RP, 
        title = "Recurrence Plot of\nInformative Chicken Text",
        pcolour = "purple",
        xlabel = "Letter",
        ylabel = "Letter",
        geom = "point")
plot_rp(recurrence_analysis_plot_poetic$RP, 
        title = "Recurrence Plot of\nPoetic Chicken Text",
        pcolour = "orange",
        xlabel = "Letter",
        ylabel = "Letter",
        geom = "point")

######## 3d. Inspect the RQA metrics ########

# Note: You must use the recurrence analysis variables created in 
#       Section 3b (NOT the plotting ones created in Section 3c)
#       to get the correct RQA values.

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

######## 3e. Visualize RP with ggplot ########

# Note: Because of the known issue for ggplot RP generation, this 
#       section is commented out by default.
#
# # convert informative and poetic into dataframes for easier plotting
# informative_df = data.frame(points = recurrence_analysis_plot_informative$RP@i,
#                             loc = seq_along(recurrence_analysis_plot_informative$RP@i))
# poetic_df = data.frame(points = recurrence_analysis_plot_poetic$RP@i,
#                        loc = seq_along(recurrence_analysis_plot_poetic$RP@i))
# 
# # use ggplot2 to generate the informative RP
# ggplot(informative_df,aes(x=points,
#                           y=loc)) +
#   geom_point(color="purple",size=1) +
#   theme_classic() +
#   theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
#   ylab("Time (in letters)") + xlab("Time (in letters)") +
#   ggtitle("Categorical recurrence quantification analysis of informative text")
# 
# # use ggplot2 to generate the poetic RP
# ggplot(poetic_df,aes(x=points,
#                      y=loc)) +
#   geom_point(color="orange",size=1) +
#   theme_classic() +
#   theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
#   ylab("Time (in letters)") + xlab("Time (in letters)") +
#   ggtitle("Categorical recurrence quantification analysis of poetic text")

######## 4. Cross-recurrence ########

######## 4a. Set parameters for cross-recurrence ########

# decide Theiler window parameter
cross_theiler_window = 0

# set radius to be very small for categorical matches
cross_categorical_radius = .0001

######## 4b. Run cross-recurrence ########

# truncate informative to length of poetic
truncated_informative = informative %>%
  slice(1:dim(poetic)[1])

# run cross recurrence over each
cross_recurrence_analysis = crqa(ts1=truncated_informative$word,
                                 ts2=poetic$word,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=cross_categorical_radius,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=cross_theiler_window)

######## 4c. Create the recurrence plot ########

# use the standard plotting functions
plot_rp(cross_recurrence_analysis$RP, 
        title = "Cross-Recurrence Plot of\nTwo Chicken Texts",
        pcolour = "red",
        xlabel = "Letter for informative",
        ylabel = "Letter for poetic",
        geom = "point")

######## 4d. Inspect the CRQA metrics ########

# take a look at the quantification metrics for CRQA across texts
cross_recurrence_analysis$RR # rate of recurrence
cross_recurrence_analysis$DET # % determinism
cross_recurrence_analysis$NRLINE # total number of lines on the plot
cross_recurrence_analysis$maxL # maximum line length on plot
cross_recurrence_analysis$L # average line length on plot
cross_recurrence_analysis$ENTR # entropy
cross_recurrence_analysis$rENTR # normalized entropy
cross_recurrence_analysis$LAM # laminarity
cross_recurrence_analysis$TT # trapping time

######## 4e. Create the diagonal recurrence profile ########

# specify the window size (from negative win_size to 0 to positive win_size)
win_size = 15

# create the DRP
chicken_drp = drpfromts(ts1 = truncated_informative$word,
                        ts2 = poetic$word,
                        datatype="categorical",
                        windowsize = win_size,
                        delay=0,
                        embed=1,
                        rescale=0,
                        radius=cross_categorical_radius,
                        normalize=0,
                        mindiagline=2,
                        minvertline=2,
                        tw=cross_theiler_window)

# make a dataframe for easier plotting
chicken_drp_data = data.frame(profile = chicken_drp$profile,
                              lag = -win_size:win_size)

# plot the DRP
qplot(y = chicken_drp$profile, 
      x = -win_size:win_size, 
      geom="line") +
  geom_line(color="red",size=1) +
  theme_classic() +
  theme(legend.position="none") +
  ylab("% Recurrence") + xlab("Lag (in letters)") +
  ggtitle("Diagonal recurrence profile\nof poetic and informative texts about chickens")