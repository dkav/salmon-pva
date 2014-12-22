# Calculate stats 
#
# Filename: cstats.R
# Author: Darren Kavanagh
# Created: 31.10.2009
# Last Modified: 02.11.2009
# ======================================================
#
# cn = count results
# ds = count datasets
# fn = output file name

# load libraries and functions
library(psych)

cstats <- function (cn, ds, fn) {

	# ----------------------------- #
	# Perform analysis              #
	# ----------------------------- #
	cstat <- describe.by(cn, ds, mat=TRUE)
	tgmean <- aggregate(list("gmean"=cn), list("dataset"=ds), geometric.mean)
	cstat=cbind(cstat, gmean=tgmean$gmean)
	
	# ----------------------------- #
	# Generate Output               #
	# ----------------------------- # 	
	filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files                  
	
	fileoutnm <- paste("./output/", fn, " ", filedt,".csv", sep = "")
	write.table(cstat, file = fileoutnm, sep = ",", col.names = NA,
	            qmethod = "double") 
   
    return("Stats generated")	            	            
}            