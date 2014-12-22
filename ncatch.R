# Plot total nominal catch
#
# Filename: ncatch.R
# Author: Darren Kavanagh
# Created: 25.11.2009
# Last Modified: 25.01.2010
# ======================================================
# 

# load libraries and functions 
library(ggplot2)

# ----------------------------- #
# Load datasets                 #
# ----------------------------- #
# total nominal catch 1960-2008
ncdata <- read.csv("./data/nea_tcatch.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	           
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

#nclabs <- labs(x="Time (years)", y="Nominal Catch\n(tonnes fresh weight)") 
#ncscal<- scale_y_continuous(format="comma")
#
#ncplot <- ggplot(ncdata, aes(x=year, y=total))
#ncplot <- ncplot + geom_line() + nclabs + ncscal
#
#imgnm <- paste("./output/","Nominal Catch ", filedt, ".png", sep = "")
#ggsave(imgnm, width = 8, height = 4)


nclabs <- labs(x="Time (years)", y="Nominal Catch\n(tonnes fresh weight)") 
ncscal<- scale_y_continuous(format="comma")

ncplot <- ggplot(ncdata)
ncplot <- ncplot + geom_bar(aes(x=year, y=total), stat='identity', fill='grey50') + nclabs + ncscal

imgnm <- paste("./output/","Nominal Catch Bar", filedt, ".png", sep = "")
ggsave(imgnm, width = 8, height = 4)