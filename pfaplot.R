# Plot pre-fishery abundance (PFA) and spawner time 
# series
#
# Filename: pfaplot.R
# Author: Darren Kavanagh
# Created: 05.11.2009
# Last Modified: 13.11.2009
# ======================================================
# 

# load libraries and functions 
library(ggplot2)
source('fsorder.R')

# ----------------------------- #
# Load datasets                 #
# ----------------------------- #
# North-East Atlantic datasets 
neacdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

neapfa <- read.csv("./data/nea_pfa.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# set levels (unique datasets) to order in file
neacdata$dataset<-factor(neacdata$dataset,levels=fsorder())
neapfa$dataset<-factor(neapfa$dataset,levels=fsorder())

selgrp <- c("South", "North")

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	           
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# define scales and legends
sxco <- scale_x_continuous(limits=c(1970,2010), breaks=seq(1975,2005,by=10))
sylogtr <- scale_y_continuous(trans = "log10", limits=c(10^3.5,10^6.5), breaks=10^seq(4,6,by=1))

regclr <- c("PFA" ="red", "Spawn" = "black")
grpscol <- scale_colour_manual("Count type", breaks = c("PFA", "Spawn"), value = regclr, labels = c("Pre-fishery", "Spawners"))
 
plegt <- opts(legend.position=c(0.75, 0.11)) 

cl10labs <- labs(x="Time (years)", y="No Salmon (log scale)") 
clabs <- labs(x="Time (years)", y="No Salmon") 

#---------------------------
# Country/Region Plots
cdata <- subset(neacdata, dsgroup %in% selgrp)
pfadata <- subset(neapfa, dsgroup %in% selgrp)

cplot <- ggplot(cdata, aes(x=year)) + sxco + grpscol + plegt

# log total plot
clplt <- cplot + facet_wrap(~dataset, ncol=2)
clplt <- clplt + geom_line(aes(y=total, colour="Spawn"))
clplt <- clplt + geom_line(aes(y=totpfa, colour="PFA"), data=pfadata)
clplt <- clplt + sylogtr + cl10labs 
imgnm <- paste("./output/","Reg Tot LPFA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

# total plot
cplt <- cplot + facet_wrap(~dataset, ncol=2, scale="free")
cplt <- cplt + geom_line(aes(y=total, colour="Spawn"))
cplt <- cplt + geom_line(aes(y=totpfa, colour="PFA"), data=pfadata)
cplt <- cplt + clabs
imgnm <- paste("./output/","Reg Tot PFA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)