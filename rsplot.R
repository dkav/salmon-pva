# Plot example of running sum output
#
# Filename: rsplot.R
# Author: Darren Kavanagh
# Date: 09.07.2009
# Last Modified: 18.10.2009
# ======================================================
# 

# load libraries and functions 
library(ggplot2)
source('runsum.R')

L = 5 # number of N's to add together to make running sum

# ----------------------------- #
# Load & Run Analysis           #
# ----------------------------- #
# Get example dataset
cdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

cdata <- subset(cdata, dataset == "Ireland")
cdata <- data.frame(year=cdata$year, total=cdata$total)

# calculate running sum
rcount <- runsum(cdata$total, L)
blnks <- rep(NA, (L-1))
rcount <- c(rcount, blnks) # account for blanks

cdata <- data.frame(cdata,rcount) # 

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	
          
Ntlab <- expression(N[t])
Rtlab <- expression(R[t])

# scale and legend keysxco <- scale_x_continuous(limits=c(1970,2010), breaks=seq(1975,2005,by=10))scol <- scale_colour_manual("Count", c("Nt"="red","Rt"="blue"), labels=c(Ntlab,Rtlab))

# plot
rsplot <- ggplot(cdata, aes(x=year)) + scol + sxco
rsplot <- rsplot + geom_line(aes(y=total/1000, colour="Nt")) # Nt line
rsplot <- rsplot + geom_line(aes(y=rcount/1000, colour="Rt")) # Rt line
rsplot <- rsplot + labs(x="Time (years)", y="No Spawners (thousands)")

# create png
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files
imgnm <- paste("./output/","Running Sum Ex ", filedt,".png", sep = "")
ggsave(imgnm, width = 5.5, height = 3.25)