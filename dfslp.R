# Degrees of freedom for slope sigma squared estimate
#
# Filename: dfslp.R
# Author: Darren Kavanagh
# Created: 05.07.2009
# Last Modified:
# ======================================================
# From Holmes & Fagan 2002
#
# n - time series length
# L - the number of counts summed together to form  Rt

dfslp <- function (n, L) {
	
	dfslp <- 0.333 + 0.212 * n -0.387 * L 
	return(dfslp)
}