# Plot λ for different input datasets
# 
# Filename: saplot.R
# Author: Darren Kavanagh
# Created: 12.10.2009
# Last Modified: 21.11.2009 
# ======================================================
# 
# ds : plot dataset
# xc : x coordinate
# xl : x lable
# fn : output file name

saplot <- function (ds, xc, xl, fn) {
	
	# ----------------------------- #
	# Generate Output               #
	# ----------------------------- #
	filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files
	
	fw <- facet_wrap(~dataset, ncol=2)

	# create scale
	miny <- 0.7; maxy <- 1.3
	syco <- scale_y_continuous(breaks=c(0.8,1,1.2))
	gylm <- coord_cartesian(ylim = c(miny, maxy))
	
	# labels and legend key
	yl <- expression(paste("Long-term population growth rate ", lambda))
	xyl <- labs(x=xl, y=yl)

	lg1 <- expression(paste(lambda," >= 1"))
	ll1 <- expression(paste(lambda," <  1 "))
	pntcol <- c(g1 = "blue", l1 = "red")
	pntcs <- scale_colour_manual("Legend", pntcol, labels=c(lg1,ll1))
	
	selgrp <- c("South", "North")
	
	# Country/Region Datasets
	pdata <- subset(ds, dsgroup %in% selgrp)
	
	p <- ggplot(data=pdata, aes(y=lambda))
	p <- p + xc + fw + syco + gylm 
	p1 <- p + geom_line() 
	p1 <- p1 + geom_smooth(aes(ymin=lambdal95, ymax=lambdah95), stat="identity", , colour="grey50") 	
	p1 <- p1 + geom_point(aes(colour = ifelse(round(lambda,2) >= 1, "g1", "l1")))
	p1 <- p1 + xyl + pntcs + opts(legend.position=c(0.65, 0.1)) 

	imgnm <- paste("./output/", fn, " C ", filedt, ".png", sep = "")
	ggsave(imgnm, width = 7.5, height = 12.5)
	
	# Total datasets
	pdata <- subset(ds, !dsgroup %in% selgrp)
	
	p <- ggplot(data=pdata, aes(y=lambda))
	p <- p + xc + fw + syco + gylm 
	p2 <- p + geom_line() 
	p2 <- p2 + geom_smooth(aes(ymin=lambdal95, ymax=lambdah95), stat="identity", , colour="grey50") 
	p2 <- p2 + geom_point(aes(colour = ifelse(round(lambda,2) >= 1, "g1", "l1")))
	p2 <- p2 + xyl + pntcs + opts(legend.position=c(0.65, 0.175)) 
	
	imgnm <- paste("./output/", fn, " T ", filedt, ".png", sep = "")
	ggsave(imgnm, width = 7.5, height = 6.25)
	
	return("Plots successfully produced")
}