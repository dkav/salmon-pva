# Calculate population parameters
#
# Filename: calcp.R
# Author: Darren Kavanagh
# Created: 30.07.2009
# Last Modified:
# ======================================================
# 
# Assumes consecutive counts no missing time periods
#
# pcount: time series of populations counts
# L: number of N's to add together to make running sum

# load libraries and functions
source('runsum.R')
source('cpmu.R')
source('hdifapx.R')
source('ltpgr.R')

calcp <- function (pcount, L) {	
    # get running sum
	rscount <- runsum(pcount, L)	
    # calc µ using Dennis method
    mu <- cpmu(rscount)
    # calc σ2 using Holmes slope method
	sigmasq <- hdifapx(rscount,L)$hsigmasqslp
	# long-term rate of population change 
	nts <- length(pcount)		   
	lambdap <- ltpgr(mu, sigmasq, nts, L)
	return(data.frame(mu, sigmasq, lambdap))
}