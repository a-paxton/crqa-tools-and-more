######## Tutorial: Categorical recurrence ########
#
# These brief exercises will walk you through performing categorical recurrence
# and cross-recurrence quantification analysis using manual parameter searches.
#
# Code written by: A. Paxton (University of Connecticut)
# Date last modified: 03 May 2019

#### 1. Preliminaries ####

# prep the workspace
rm(list=ls())

# load in libraries as needed
library(dplyr)
library(crqa)
library(ggplot2)

# set working directory to appropriate location
setwd('./data')

# read in the data
bland = read.table("bland.txt", sep="\t") %>%
  .$V1
hype = read.table("hype.txt", sep="\t") %>%
  .$V1

#### 2. Plotting your data ####

# plot the hype sequences
qplot(hype,
      x=seq_along(hype), geom="point") +
  geom_path() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in hype announcement")

# plot the bland sequences
qplot(bland,
      x=seq_along(bland), geom="point") +
  geom_path() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  xlab("Time (in letters)") + ylab("Letter number") +
  ggtitle("Character sequences in bland announcement")

#### 3. Recurrence quantification analysis ####

######## 3a. Recurrence parameter setting ########

# decide Theiler window parameter (generally, 1 if RQA and 0 if CRQA)
rec_theiler_window = 1

# set radius to be very small for categorical matches
rec_categorical_radius = .0001

######## 3b. Run recurrence quantification analysis ########

# run rqa over hype
recurrence_analysis_hype = crqa(ts1=hype,
                                ts2=hype,
                                delay=0,
                                embed=1,
                                rescale=0,
                                radius=rec_categorical_radius,
                                normalize=0,
                                mindiagline=2,
                                minvertline=2,
                                tw=rec_theiler_window)

# run rqa over bland
recurrence_analysis_bland = crqa(ts1=bland,
                                 ts2=bland,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=rec_categorical_radius,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=rec_theiler_window)

######## 3c. Create the recurrence plot ########

##  this is where you will create NEW crqa analysis variables that
##  have the theiler window = 0

# convert bland and hype into dataframes for easier plotting
bland_df = data.frame(points = recurrence_analysis_bland$RP@i,
                      loc = seq_along(recurrence_analysis_bland$RP@i))
hype_df = data.frame(points = recurrence_analysis_hype$RP@i,
                     loc = seq_along(recurrence_analysis_hype$RP@i))

# use ggplot2's qplot function to generate the bland recurrence plot
ggplot(bland_df,aes(x=points,
                    y=loc)) +
  geom_point(color="purple",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters)") + xlab("Time (in letters)") +
  ggtitle("Categorical recurrence quantification analysis of bland announcement")

# use ggplot2's qplot function to generate the hype recurrence plot
ggplot(hype_df,aes(x=points,
                   y=loc)) +
  geom_point(color="orange",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters)") + xlab("Time (in letters)") +
  ggtitle("Categorical recurrence quantification analysis of hype announcement")

######## 4. Cross-recurrence ########

######## 4a. Cross-recurrence parameter setting ########

# decide Theiler window parameter (generally, 1 if RQA and 0 if CRQA)
cross_theiler_window = 0

# set radius to be very small for categorical matches
cross_categorical_radius = .0001

######## 4b. Run cross-recurrence ########

# truncate bland to length of hype
truncated_bland = bland[1:length(hype)] 

# run cross recurrence over each
cross_recurrence_analysis = crqa(ts1=truncated_bland,
                                 ts2=hype,
                                 delay=0,
                                 embed=1,
                                 rescale=0,
                                 radius=cross_categorical_radius,
                                 normalize=0,
                                 mindiagline=2,
                                 minvertline=2,
                                 tw=cross_theiler_window)

######## 4c. Create the recurrence plot ########

# convert bland and hype into dataframes for easier plotting
cross_rec_df = data.frame(points = cross_recurrence_analysis$RP@i,
                          loc = seq_along(cross_recurrence_analysis$RP@i))

# use ggplot2's qplot function to generate the bland recurrence plot
ggplot(cross_rec_df,aes(x=points,
                              y=loc)) +
  geom_point(color="red",size=1) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ylab("Time (in letters) of hype text") + xlab("Time (in letters) of bland text") +
  ggtitle("Categorical cross-recurrence quantification analysis\nof hype and bland announcements")