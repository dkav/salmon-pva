# Detecting trends using Spearman's Rank Correlation 
# Coefficient (see Gauthier 2001)
#
# Filename: ctrend.R
# Author: Darren Kavanagh
# Created: 02.11.2009
# Last Modified: 
# ======================================================
#
# cds = count dataset (dataset, year, count)
# fn = output file name

# load libraries and functions
library(pastecs)

ctrend <- function (cds, fn) {
	
	dsn <- unique(cds$dataset)
	nds <- length(dsn)
	ctnd <- data.frame(dataset=dsn, rho=numeric(nds), pvalue=numeric(nds))
	
	for (j in 1:nds) {
		
		curds <- as.character(dsn[j])    
	    cdssub <- subset(cds, dataset == curds)
	
		tt <- trend.test(cdssub$count)
		ctnd[j,2-3] <- data.frame(tt$estimate, tt$p.value)	
	}	
	
	# ----------------------------- #
	# Generate Output               #
	# ----------------------------- # 	
	filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files                  
	
	fileoutnm <- paste("./output/", fn, " ", filedt,".csv", sep = "")
	write.table(ctnd, file = fileoutnm, sep = ",", col.names = NA,
	            qmethod = "double") 	
}