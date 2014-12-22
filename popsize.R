# Estimate population size 
#
# Filename: popsize.R
# Author: Darren Kavanagh
# Created: 06.08.2009
# Last Modified: 29.01.2010
# ======================================================
# 
#

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
# East Atlantic datasets #
eacds <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
selgrp <- c("South", "North")
eacds <- subset(eacds, dsgroup %in% selgrp)

# 1SW spanwer age
oswards <- read.csv("./data/osw_ar.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# set levels (unique datasets) to order in file
eacds$dataset<-factor(eacds$dataset,levels=unique(eacds$data))
oswards$dataset<-factor(oswards$dataset,levels=unique(oswards$data))

# ----------------------------- #
# Perform analysis              #
# ----------------------------- #
ordds <- unique(eacds$dataset)
nods <- length(ordds) # number of datasets

syear <- 2004 # need to start a few years before

pops <- data.frame(dataset=ordds, totpops=numeric(nods), oswpops=numeric(nods), mswpops=numeric(nods))

for (j in 1:nods) {
   
	curds <- as.character(ordds[j]) # get dataset name      
	
	# get 1SW age at spawning
	oswsub <- subset(oswards, dataset == curds)
	oswar <- oswsub$oswar
	
	# get dataset counts
	eacsub <- subset(eacds, dataset == curds & year >=syear) 
	
	# calc and store population size 
	pops[j,2] <- sum(eacsub$total[1:(oswar-1)]) + eacsub$msw[oswar]
	pops[j,3] <- sum(eacsub$osw[1:(oswar-1)]) 	
	pops[j,4] <- sum(eacsub$msw[1:(oswar)]) 	

}

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
fileoutnm <- paste("./data/popsize.csv", sep = "")
write.table(pops, file = fileoutnm, sep = ",", col.names = NA,
            qmethod = "double") 