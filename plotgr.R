# Plot growth rates
#
# Filename: plotgr.R
# Author: Darren Kavanagh
# Created: 09.10.2009
# Last Modified: 18.11.2009
# ======================================================
# 

# load libraries and functions
library(ggplot2)

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
# growth parameters dataset
totds <- read.csv("./data/totmxgr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# start year
p1styr <- 1971 
p2styr <- 1988 
styr <- c(1971,1988)
selgrp <- c("South", "North")

totds <- subset(totds,dsgroup %in% selgrp & syear %in% styr)
totds$dataset<-factor(totds$dataset, levels=unique(totds$dataset))
totds <- with(totds, totds[order(rev(dataset)), ])
totds$dataset<-factor(totds$dataset, levels=unique(totds$dataset))

p1data <- subset(totds, syear == p1styr)
p2data <- subset(totds, syear == p2styr)

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# define labels and axis
xlabel = expression(lambda); ylabel="Country"
xyl <- labs(x=xlabel, y=ylabel)
xlm <- xlim(0.8, 1.2)

# define geometry parameters
gvline <- geom_vline(xintercept = 1, colour="blue", linetype=2, width = 0.04)
gpoint <- geom_point(size=3, colour="black") 

# plot 1
p1 <- ggplot(data=p1data, aes(x=lambda, y=dataset)) 
p1 <- p1 + theme_bw() + xyl + xlm 
p1 <- p1 + geom_errorbarh(aes(xmax = lambdah95, xmin = lambdal95), linetype=1, width = 0.01, colour="grey25")
p1 <- p1 + gvline + gpoint
imgnm <- paste("./output/","GR Plot ",  p1styr, " ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5.2)

# plot 2
p2 <- ggplot(data=p2data, aes(x=lambda, y=dataset)) 
p2 <- p2 + theme_bw() + xyl + xlm 
p2 <- p2 + geom_errorbarh(aes(xmax = lambdah95, xmin = lambdal95), linetype=1, width = 0.01, colour="grey25")
p2 <- p2 + gvline + gpoint + geom_point(data=p1data, datasize=2, colour="red", shape=4)
imgnm <- paste("./output/","GR Plot ", p2styr, " ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5.2)
