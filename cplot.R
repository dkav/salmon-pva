# Plot spawner time series
#
# Filename: cplot.R
# Author: Darren Kavanagh
# Created: 27.07.2009
# Last Modified: 23.11.2009
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

# set levels (unique datasets) to order in file
neacdata$dataset<-factor(neacdata$dataset,levels=fsorder())

selgrp <- c("South", "North")

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	           
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# define scales and legends
scol <- scale_colour_manual("Class", c("Total"="grey25","1SW"="blue","MSW"="red"))
sxco <- scale_x_continuous(limits=c(1970,2010), breaks=seq(1975,2005,by=10))
sylogtr <- scale_y_continuous(trans = "log10", limits=c(10^1.5,10^6.5), breaks=10^seq(2,6,by=1))

regclr <- c("South Europe" ="red", "North Europe" = "blue")
grpscol <- scale_colour_manual("Europe", breaks = c("South Europe", "North Europe"), value = regclr, labels = c("South", "North"))
 
plegf <- opts(legend.position ="none") 
plegt <- opts(legend.position=c(0.65, 0.1)) 
pleggrp <- opts(legend.position=c(0.8, 0.8), legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))

clabs <- labs(x="Time (years)", y="No Spawners (thousands)") 
cl10labs <- labs(x="Time (years)", y="No Spawners (log scale)") 

# trendline
ltrend <- geom_smooth(linetype="dotted", method = "lm", se=F)

#---------------------------
# Country/Region Plots
cdata <- subset(neacdata, dsgroup %in% selgrp)
cplot <- ggplot(cdata, aes(x=year)) + scol + sxco 
cplot <- cplot + facet_wrap(~dataset, ncol=2)

# standard plot #
# sea age plot
cps <- cplot + geom_line(aes(y=osw/1000, colour="1SW")) # 1SW
cps <- cps + geom_line(aes(y=msw/1000, colour="MSW")) # MSW
cps <- cps + clabs + plegt
imgnm <- paste("./output/","Reg SW Cnt Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

# total plot
cpt <- cplot + aes(y=total/1000) + geom_line() 
cpt <- cpt + clabs + plegf + facet_wrap(~dataset, ncol=2, scale="free")
imgnm <- paste("./output/","Reg Tot Cnt Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

# log plot #
# sea age plot
sylogtrx <- scale_y_continuous(trans = "log10", limits=c(10^1.9,10^6.1), breaks=10^seq(2,6,by=2), minor = 10^seq(3,6,by=2))
cpls <- cplot + geom_line(aes(y=osw, colour="1SW")) # 1SW 
cpls <- cpls + geom_line(aes(y=msw, colour="MSW")) # MSW
cplt <- cplot + geom_line(aes(y=total)) 
cpls <- cpls + cl10labs + plegt + sylogtrx
imgnm <- paste("./output/","Reg SW LCnt Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

# total plot
sylogtrx <- scale_y_continuous(trans = "log10", limits=c(10^3.5,10^6.1), breaks=10^seq(4,6,by=1))
cplt <- cplot + geom_line(aes(y=total)) 
cplt <- cplt + sylogtrx + cl10labs + plegf 
imgnm <- paste("./output/","Reg Tot LCnt Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 10)

#--------------------------------------
# Group Plots
seltgrp <- c("NTotal", "STotal")
cdata <- subset(neacdata, dsgroup %in% seltgrp)
cplot <- ggplot(cdata, aes(x=year)) + sxco

# standard plot #
# sea age plot
cps <- cplot + facet_wrap(~dataset, ncol=2) + scol
cps <- cps + geom_line(aes(y=osw/1000, colour="1SW")) # 1SW line
cps <- cps + geom_line(aes(y=msw/1000, colour="MSW")) # MSW line
cps <- cps + clabs
imgnm <- paste("./output/","Reg SW Grp Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7.5, height = 3.125)

# total plot
cpt <- cplot + aes(y=total/1000, colour = dataset) + geom_line() 
cpt <- cpt + clabs + grpscol + pleggrp
#cpt <- cpt + ltrend
imgnm <- paste("./output/","Reg Tot Grp Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 3)

# log plot #
# add layers
cpls <- cplot + facet_wrap(~dataset, ncol=2) + scol
cpls <- cpls + geom_line(aes(y=osw, colour="1SW")) # 1SW line
cpls <- cpls + geom_line(aes(y=msw, colour="MSW")) # MSW line
cpls <- cpls + cl10labs + sylogtr
imgnm <- paste("./output/","Reg SW LGrp Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7.5, height = 3.125)

# total plot
cplt <- cplot + geom_line(aes(y=total, colour = dataset)) 
cplt <- cplt + cl10labs + sylogtr + grpscol
imgnm <- paste("./output/","Reg Tot LGrp Plots ", filedt, ".png", sep = "") 
ggsave(imgnm, width = 6, height = 3)

#--------------------------------------
# NEA Plots
cdata <- subset(neacdata, dsgroup == "NEA")
cplot <- ggplot(cdata, aes(x=year)) + scol + sxco

# standard plot #
# sea age plot
cps <- cplot + geom_line(aes(y=osw/1000, colour="1SW")) # 1SW line
cps <- cps + geom_line(aes(y=msw/1000, colour="MSW")) # MSW line
cps <- cps + clabs 
imgnm <- paste("./output/","Reg SW NEA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 3)

# total plot
cpt <- cplot + aes(y=total/1000) + geom_line() 
cpt <- cpt + clabs + plegf 
cpt <- cpt + ltrend
imgnm <- paste("./output/","Reg Tot NEA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 3)

# log plot #
# add layers
cpls <- cplot + geom_line(aes(y=osw, colour="1SW")) # 1SW line
cpls <- cpls + geom_line(aes(y=msw, colour="MSW")) # MSW line
cpls <- cpls + cl10labs + sylogtr
imgnm <- paste("./output/","Reg SW LNEA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 3)

# total plot
cplt <- cplot + geom_line(aes(y=total)) 
cplt <- cplt + cl10labs + plegf + sylogtr
imgnm <- paste("./output/","Reg Tot LNEA Plots ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 3)