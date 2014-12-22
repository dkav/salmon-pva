# Calculate population parameters for a range of start 
# times with varying forward years. Saves output for
# Total, 1SW and MSW. Separate file for max forward 
# year.
#
# Filename: pgrcalc.R
# Author: Darren Kavanagh
# Created: 30.07.2009
# Last Modified: 06.08.2008
# ======================================================
# 
# load libraries and functions
source('calcp.R')

L <- 5 # set L value

mints = 15          		# minimum number of time series counts
stryr <- 1971   	     	# start year
endyr <- 2008    		    # end year
maxstyr <- (endyr - mints)  # max start year
strtrng <- c(stryr:maxstyr) # start year range
notss <- length(strtrng)    # no of years in range

# ----------------------------- #
# Load & Run Analysis           #
# ----------------------------- #
# spawner data
cdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cdata$dataset<-factor(cdata$dataset, levels=unique(cdata$dataset))

# cdata <- subset(cdata, dataset == "NE Atlantic") # limit for testing

# PFA data
cpfa <- read.csv("./data/nea_pfa.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
# Returns data
crtn <- read.csv("./data/nea_returns.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

ordds <- unique(cdata$dataset)
orgrp <- unique(cdata$dsgroup)
nods <- length(ordds) # number of datasets

# find number of records to allocate
nrec <- 0 # rec counter
for (j in 1:nods) {
   for (k in 1:notss) {
   	    nofy <- (maxstyr+1) - strtrng[k] 
    	for (i in 1:nofy) {
			nrec <- nrec + 1
		  }     
	}
}

nomx <- notss*nods 
# output data frames
# total
totgr <- data.frame(dataset=factor(character(nrec), levels=(ordds)),dsgroup=factor(character(nrec),levels=orgrp), syear=numeric(nrec), fyear=numeric(nrec), ctoratio=numeric(nrec), ctmratio=numeric(nrec), apsize=numeric(nrec), tmrate=numeric(nrec), dmrate=numeric(nrec), lmrate=numeric(nrec), L=numeric(nrec), mu=numeric(nrec), sigmasq=numeric(nrec), lambda=numeric(nrec), lambdal95=numeric(nrec), lambdah95=numeric(nrec))

totmxgr <- totgr[1:nomx,]

# 1SW
oswgr <- totgr
oswmxgr <- totmxgr

# MSW
mswgr <- totgr
mswmxgr <- totmxgr

cnt <- 0; mxcnt <- 0 # counters

print(paste("No of datasets:", nods))

# run analysis for each dataset
for (j in 1:nods) {
	curds <- as.character(ordds[j])      
    cdssub <- subset(cdata, dataset == curds) # spawners
    cpfasub <- subset(cpfa, dataset == curds) # PFA
    crtnsub <- subset(crtn, dataset == curds) # returns
        
    print(paste("Dataset",j,curds,"processing"))
    # vary start year
    for (k in 1:notss) {
    	mxcnt <- mxcnt + 1
    	csyear <- strtrng[k]
    	stsds <- subset(cdssub, year >= csyear)  # spawners
    	stpfa <- subset(cpfasub, year >= csyear) # PFA
    	strtn <- subset(crtnsub, year >= csyear) # returns
    	    	    	
    	# vary no years into the future
        nofy <- (maxstyr+1) - csyear 
    	for (i in 1:nofy) {	
    		cnt <- cnt + 1	
    		cfyear <- mints+i-1	
			ftsds <- subset(stsds, year <= (csyear+cfyear)) # spawners
			ftspfa <- subset(stpfa, year <= (csyear+cfyear)) # PFA
			ftsrtn <- subset(strtn, year <= (csyear+cfyear)) # returns
			
			ftslnt <- length(ftsds$dataset)
			cds <- as.character(ftsds$dataset[1])
			cdsgrp <- as.character(ftsds$dsgroup[1])

			# 1SW/MSW ratios
			ctoratio <- mean(ftsds$osw/ftsds$total)
			ctmratio <- mean(ftsds$msw/ftsds$total)
			
			curinp <- data.frame(cds, cdsgrp, csyear, cfyear, ctoratio, ctmratio)
		# total
			ctotap <- mean(ftsds$total) # total average pop size
			# total mortality rate
			tctmrate <- mean((ftspfa$totpfa-ftsds$total)/ftspfa$totpfa)
			# distant mortality rate
			tcdmrate <- mean((ftspfa$totpfa-ftsrtn$totret)/ftspfa$totpfa)
			# local mortality rate
			tclmrate <- mean((ftsrtn$totret-ftsds$total)/ftspfa$totpfa)
			# calc growth rate parameters
			totp <-  calcp(ftsds$total, L)
  		    # populate total data frame
			totgr[cnt,] <- data.frame(curinp, ctotap, tctmrate, tcdmrate, tclmrate, L, totp)	
				
		# 1SW
			coswap <- mean(ftsds$osw) # osw average pop size
			# total mortality rate
			octmrate <- mean((ftspfa$oswpfa-ftsds$osw)/ftspfa$oswpfa)
			# distant mortality rate
			ocdmrate <- mean((ftspfa$oswpfa-ftsrtn$oswret)/ftspfa$oswpfa)
			# local mortality rate
			oclmrate <- mean((ftsrtn$oswret-ftsds$osw)/ftspfa$oswpfa)
			# calc growth rate parameters
		    oswp <-  calcp(ftsds$osw, L)
  		    # populate total data frame
			oswgr[cnt,] <- data.frame(curinp, coswap, octmrate, ocdmrate, oclmrate, L, oswp)	
			 
		# MSW			
		    cmswap <- mean(ftsds$msw) # msw average pop size		    
		    # total mortality rate
			mctmrate <- mean((ftspfa$mswpfa-ftsds$msw)/ftspfa$mswpfa)
			# distant mortality rate
			mcdmrate <- mean((ftspfa$mswpfa-ftsrtn$mswret)/ftspfa$mswpfa)
			# local mortality rate
			mclmrate <- mean((ftsrtn$mswret-ftsds$msw)/ftspfa$mswpfa)
			# calc growth rate parameters
			mswp <-  calcp(ftsds$msw, L)
  		    # populate total data frame
			mswgr[cnt,] <- data.frame(curinp, cmswap, mctmrate, mcdmrate, mclmrate, L, mswp)							
		  } # end of future years loop	
		  # allocate max forward years	 
		  totmxgr[mxcnt,] <- totgr[cnt,] # total
		  oswmxgr[mxcnt,] <- oswgr[cnt,] # 1SW
		  mswmxgr[mxcnt,] <- mswgr[cnt,] # MSW		    		  
   	 } # end of start year loop   
}  # end of dataset loop
 
# ----------------------------- #
# Generate Output               #
# ----------------------------- #
# total 
fileoutnm <- paste("./data/", "totgr.csv", sep = "")
write.table(totgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")
fileoutnm <- paste("./data/", "totmxgr.csv", sep = "")
write.table(totmxgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")           
# 1SW 
fileoutnm <- paste("./data/", "oswgr.csv", sep = "")
write.table(oswgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")
fileoutnm <- paste("./data/", "oswmxgr.csv", sep = "")
write.table(oswmxgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")
# MSW           
fileoutnm <- paste("./data/", "mswgr.csv", sep = "")
write.table(mswgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")
fileoutnm <- paste("./data/", "mswmxgr.csv", sep = "")
write.table(mswmxgr, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")