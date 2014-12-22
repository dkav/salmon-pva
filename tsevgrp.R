# Plot λ for each start year with max no future years
# and plot λ for 1971 with a range of search years
#
# Filename: tsevgrp.R
# Author: Darren Kavanagh
# Created: 28.07.2009
# Last Modified: 21.11.2009
# ======================================================
# 

# load libraries and functions
library(ggplot2)
source('fsorder.R')
source('saplot.R')

# ----------------------------- #
# Load Data & Generate Output   #
# ----------------------------- #

#  Start years (1971-1993)      #
# ----------------------------- #
cdata <- read.csv("./data/totmxgr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
# set levels (unique datasets) to order in file
cdata$dataset<-factor(cdata$dataset,levels=fsorder())

# plot parameters
xcoord <- aes(x=syear)
xlabel <- "Start Year"
fname  <- "TSeries SA SY"

# call plot function
saplot(cdata, xcoord, xlabel, fname)