# Derive L value from sea age classes
#
# Filename: derivel.R
# Author: Darren Kavanagh
# Created: 31.07.2009
# Last Modified:
# ======================================================
#

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
cdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cdata$dataset <- factor(cdata$dataset, levels=unique(cdata$dataset))

# 1SW return age
oswards <- read.csv("./data/osw_ar.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

selgrp <- c("South", "North")
cdata <- subset(cdata, dsgroup %in% selgrp)

ordds <- unique(cdata$dataset)
orgrp <- unique(cdata$dsgroup)
nods <- length(ordds) # number of datasets

# ----------------------------- #
# Perform Analaysis             #
# ----------------------------- #

# vector average age at return time in order of country
oswa <- oswards$oswar 
mswa <- oswa + 1

statsfL <- data.frame(dataset=ordds, oswa=oswa, mswa=mswa, tosw=numeric(nods), tmsw=numeric(nods), dsavg=numeric(nods)) 

totsum <- NULL

# calculate average age at return for each country
for (j in 1:nods) {
   curds <- as.character(ordds[j])      
   cdssub <- subset(cdata, dataset == curds)
   
   tosw <- sum(cdssub$osw)
   tmsw <- sum(cdssub$msw)
   oswp <- rep(oswa[j],tosw)
   mswp <- rep(mswa[j],tmsw)
 
   cdsavg <- mean(c(oswp,mswp))
   totsum <- c(totsum,oswp,mswp)
   statsfL[j,4:6] <- data.frame(tosw, tmsw, cdsavg)
}

tdsavg <- mean(statsfL$dsavg)
print(paste("Average return age (mean of countries):", format(tdsavg,digits=2)))
tavg <- mean(totsum)
print(paste("Average return age (mean of total):", format(tavg,digits=2)))

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
fileoutnm <- paste("./output/", "derivel.csv", sep = "")
write.table(statsfL, file = fileoutnm, sep = ",", col.names = NA,
           qmethod = "double")