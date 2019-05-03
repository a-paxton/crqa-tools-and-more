# preliminaries
library(crqa)

# create two categorical time series
ts1 = c(0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,1,0,0,
	0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
ts2 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,
	0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)

# set parameters for all CRQA analyses
delay = 1
embed = 1
rescale = 1
radius = 1
normalize = 0
mindiagline = 2
minvertline = 2
tw = 0
whiteline = FALSE
recpt = FALSE
side = "both"
checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)

# calculate CRQA
rec_analysis = crqa(ts1, ts2, delay, embed, rescale, radius, normalize, mindiagline, 
	minvertline, tw, whiteline, recpt, side, checkl)

# use standard plot function to generate the recurrence plot
plot(rec_analysis$RP@i,type='p',cex=.5, xlab = "Time Series 1", ylab="Time Series 2", labels=FALSE)
  