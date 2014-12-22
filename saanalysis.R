# Sea age sensitivity analysis
#
# Filename: saanalysis.R
# Author: Darren Kavanagh
# Created: 30.07.2009
# Last Modified: 02.11.2009
# ======================================================
#

# load libraries and functions
library(ggplot2)
source('fsorder.R')

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
totds <- read.csv("./data/totmxgr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
totds$dataset<-factor(totds$dataset, levels=fsorder())

oswds <- read.csv("./data/oswmxgr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
oswds$dataset<-factor(oswds$dataset, levels=fsorder())

mswds <- read.csv("./data/mswmxgr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
mswds$dataset<-factor(mswds$dataset, levels=fsorder())

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

fw <- facet_wrap(~dataset, ncol=2)

# create scale
syco <- scale_y_continuous(limits=(c(0.9,1.1))) 
scol <- scale_colour_manual("Class", c("1SW"="blue","MSW"="red", "All"="grey25"))

xl <- "Start Year"
yl <- expression(paste("Long-term population growth rate ", lambda))
xyl <- labs(x=xl, y=yl)

# -------------------------------
# Country/Region Datasets
selgrp <- c("South", "North")
tdata <- subset(totds, dsgroup %in% selgrp)
odata <- subset(oswds, dsgroup %in% selgrp)
mdata <- subset(mswds, dsgroup %in% selgrp)

p <- ggplot()
p1 <- p + geom_line(data=odata, aes(x=syear,y=lambda, colour="1SW")) 
p1 <- p1 + geom_line(data=mdata, aes(x=syear,y=lambda, colour="MSW"))
p1 <- p1 + geom_line(data=tdata, aes(x=syear,y=lambda, colour="All")) 
p1 <- p1 + fw + scol + syco
p1 <- p1 + xyl + opts(legend.position=c(0.65, 0.1)) 

imgnm <- paste("./output/","Sea Age SA C ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7.5, height = 12.5)

# -------------------------------
# Total datasets
tdata <- subset(totds, !dsgroup %in% selgrp)
odata <- subset(oswds, !dsgroup %in% selgrp)
mdata <- subset(mswds, !dsgroup %in% selgrp)

p <- ggplot()
p2 <- p + geom_line(data=odata, aes(x=syear,y=lambda, colour="1SW")) 
p2 <- p2 + geom_line(data=mdata, aes(x=syear,y=lambda, colour="MSW"))
p2 <- p2 + geom_line(data=tdata, aes(x=syear,y=lambda, colour="All")) 
p2 <- p2 + fw + scol + syco 
p2 <- p2 + xyl + opts(legend.position=c(0.65, 0.175)) 

imgnm <- paste("./output/","Sea Age SA T ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7.5, height = 6.25)