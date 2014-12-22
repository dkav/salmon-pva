# Calculate risk estimates for different datasets
#
# Filename: calcrisk.R
# Author: Darren Kavanagh
# Created: 06.08.2009
# Last Modified: 29.01.2010
# ======================================================
# 

# load libraries and functions
source('probexti.R')

# ----------------------------- #
# Define Parameters             #
# ----------------------------- #
ds <- "tot" 
#ds <- "osw" 
#ds <- "msw"

#styr = 1971 # start year
styr = 1988 # start year
fts <- 1000 # how far to project into the future
ifts <- 1   # increment to assess in the the future
exth <- 1   # extinction threashold

# define regions
south <- c("Ireland", "France", "UK (EW)", "UK (NI)", "UK (Scot)","Iceland (S&W)")
north <- c("Finland", "Norway", "Russia", "Sweden", "Iceland (N&E)")

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
# growth parameters dataset
totds <- read.csv(paste("./data/", ds, "mxgr.csv", sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
totds$dataset<-factor(totds$dataset, levels=unique(totds$dataset))
# population size dataset (including total groups)
popds <- read.csv("./data/popsizewtg.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

ordds <- unique(totds$dataset) # list of datasets
nods <- length(ordds)          # no of datasets
norec <- nods*(fts/ifts)       # no of records to store
noreds <- fts/ifts             # no of records for each dataset

extinct <- data.frame(dataset=character(0), region=character(0), fyear=numeric(0), risk=numeric(0), riskl95=numeric(0), riskh95=numeric(0))
decl90 <- extinct

# ----------------------------- #
# Perform Analysis              #
# ----------------------------- #
print(paste("No of datasets:", nods))

for (j in 1:nods) {
	curds <- as.character(ordds[j])   
	print(paste("Dataset",j, "processing:", curds))
	if (curds %in% south) regn = "South"
	else if (curds %in% north) regn = "North"
	else regn =""
	
    cdssub <- subset(totds, dataset == curds & syear== styr)
	popsub <- subset(popds, dataset == curds) # population size
	
	# requires only 1 row
	if (length(cdssub$syear) != 1) {
		print("More than 1 set of parameters selected")
		break
	}
	clnt <- cdssub$fyear + 1
	
	psz <- popsub[[paste(ds, "pops", sep="")]] # population size	
	
	# extinction
	textinct <- probexti(psz, clnt, cdssub$mu, cdssub$sigmasq, cdssub$L, exth, fts, ifts)
	extinct <- rbind(extinct, data.frame(dataset=curds, region=regn, textinct))
		
	# 90% decline
    deth <- psz * 0.1 # decline threashold
	tdecl90 <- probexti(psz, clnt, cdssub$mu, cdssub$sigmasq, cdssub$L, deth, fts, ifts)
	decl90 <- rbind(decl90, data.frame(dataset=curds, region=regn, tdecl90))
}

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
# extinction
fileoutnm <- paste("./data/", styr, ds, "extinct.csv", sep = "")
write.table(extinct, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")  

# 90% decline
fileoutnm <- paste("./data/", styr, ds,"decl90.csv", sep = "")
write.table(decl90, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double") 