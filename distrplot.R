# Plot distribution plots
#
# Filename: distrplot.R
# Author: Darren Kavanagh
# Created: 23.11.2009
# Last Modified:
# ======================================================
#

# load libraries and functions 
library(ggplot2)
source('fsorder.R')

# ----------------------------- #
# Load datasets                 #
# ----------------------------- #

# North-East Atlantic returns 
neacrtn <- read.csv("./data/nea_returns.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
# North-East Atlantic spawners 
neacspw <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# set levels (unique datasets) to order in file
neacrtn$dataset<-factor(neacrtn$dataset,levels=fsorder())
neacspw$dataset<-factor(neacspw$dataset,levels=fsorder())

selgrp <- c("South", "North")
rdata <- subset(neacrtn, dsgroup %in% selgrp)
sdata <- subset(neacspw, dsgroup %in% selgrp)

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	           
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# returns distribution plot
dlabs <- labs(x="No Returns (thousands)", y="Density") 
dplot <- ggplot(cdata, aes(totret/1000)) +  facet_wrap(~dataset, ncol=2, scale="free")
dplot <- dplot + geom_histogram(aes(y = ..density..), colour="grey20", fill="white") 
dplot <- dplot + geom_density(colour="red")
dplot <- dplot + dlabs
imgnm <- paste("./output/","Ret Tot Dist Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

# spawner distribution plot
dlabs <- labs(x="No Spawners (thousands)", y="Density") 
dplot <- ggplot(sdata, aes(total/1000)) +  facet_wrap(~dataset, ncol=2, scale="free")
dplot <- dplot + geom_histogram(aes(y = ..density..), colour="grey20", fill="white") 
dplot <- dplot + geom_density(colour="red")
dplot <- dplot + dlabs
imgnm <- paste("./output/","Spw Tot Dist Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)