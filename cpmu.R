# Calculates parameter µ
#
# Filename: cpmu.R
# Author: Darren Kavanagh
# Created: 25.07.2009
# Last Modified:
# ======================================================
# Method outlined in Dennis et al. (1991)
#
# Code assumes that there are no missing census years
#
# parameter µ: determines how quickly the mean increases
#
# tscount: time series count data

cpmu <- function (tscount) {
   	tslen <- length(tscount)
   	# calculate µ	 
	pmu <- mean(log(tscount[2:tslen]/tscount[1:(tslen-1)]))
	return(pmu)
}