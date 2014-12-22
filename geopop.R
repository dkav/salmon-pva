# Simple function to calculate geometric growth
#
# Filename: geopop.R
# Author: Darren Kavanagh
# Created: 15.07.2009
# Last Modified:
# ======================================================
# 
# pop : vector of population numbers (N)
# gr  : population growth rate (λ)
# tl  : Time in the future to end calc (Tn)

# Function to calc new population
gmgm <- function (pop, gr, tl) {
	for (ctime in 1:tl) {
		pop[ctime+1] <- gr * pop[ctime]          
	}
	return(pop)
}