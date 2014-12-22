# Plot probability cumulative distribution function (CDF)
#
# Filename: gcdf.R
# Author: Darren Kavanagh
# Date: 18.08.2009
# Last Modified: 28.01.2010
# ======================================================
#
# cdata: dataset with probability results
# xdata: extra dataset with probability results
# yl: y label (extinction or decline)
# fn: name for file
# lt: legend title
# ky1: legend key 1 (only used xdata has data)
# ky2: legend key 2 (only used xdata has data)

# load libraries and functions
library(ggplot2)

gcdf <- function (cdata, xdata=NA, yl, fn, lt=NA, ky1=NA, ky2=NA) {
	# ----------------------------- #
	# Generate Output               #
	# ----------------------------- #
	filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

	xl <- "Time (years)"
	scol <- scale_colour_manual(lt, breaks=c("colr1"="black","colr2"="red"), labels=c(ky2, ky1))

	# -------------------------------
	# Country/Region Datasets
	selds <- c("South Europe", "North Europe", "NE Atlantic")
	pdata1 <- subset(cdata, ! dataset %in% selds)
	
	p1 <- ggplot(data=pdata1, aes(x=fyear,y=risk)) + facet_wrap(~dataset, ncol=2)
	if (is.logical(xdata)) {
		p1 <- p1 + geom_line() }
	else {
		p1xdata <- subset(xdata, ! dataset %in% selds)
		p1 <- p1 + scol + opts(legend.position=c(0.65, 0.1))
		p1 <- p1 + geom_line(aes(colour="colr2"))
		p1 <- p1 + geom_line(aes(x=fyear,y=risk, colour="colr1"), data=p1xdata)		
	}	
	p1 <- p1 +labs(x=xl, y=yl) + ylim(0, 1) 
	#	p1 <- p1 + geom_smooth(aes(ymin=riskl95, ymax=riskh95), stat="identity") 

	imgnm <- paste("./output/","CDF ", fn," Country ", filedt, ".png", sep = "")
	ggsave(imgnm, width = 7.5, height = 12.5)
	
	# -------------------------------
	# Total datasets
	selds <- c("South Europe", "North Europe")
	pdata2 <- subset(cdata, dataset %in% selds)
	
	p2 <- ggplot(data=pdata2, aes(x=fyear,y=risk)) + facet_wrap(~dataset, ncol=2)
	if (is.logical(xdata)) {
		p2 <- p2 + geom_line() }
	else {
		p2xdata <- subset(xdata, dataset %in% selds)
		p2 <- p2 + scol
		p2 <- p2 + geom_line(aes(x=fyear,y=risk, colour="colr1"), data=p2xdata)
		p2 <- p2 + geom_line(aes(colour="colr2"))

	}	
	p2 <- p2 +labs(x=xl, y=yl) + ylim(0, 1) 
#	p2 <- p2 + geom_smooth(aes(ymin=riskl95, ymax=riskh95), stat="identity")

	imgnm <- paste("./output/","CDF ", fn," Total ", filedt, ".png", sep = "")
	ggsave(imgnm, width = 8.3, height = 3.5)
}