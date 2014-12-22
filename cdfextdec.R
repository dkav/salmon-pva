# Plot extinction and decline CDF's
#
# Filename: cdfextdec.R
# Author: Darren Kavanagh
# Date: 18.08.2009
# Last Modified: 29.01.2010
# ======================================================
#

# load libraries and functions
source('fsorder.R')
source('gcdf.R')

# ----------------------------- #
# Load Data & Generate Output   #
# ----------------------------- #

# legend keys
lgt1 <- "Time period"
lgt2 <- "Probabilty type"
key1 <- "1971-2008"
key2 <- "1988-2008"
key3 <- "Extinction"
key4 <- "90% decline"

# 1971
cext1 <- read.csv("./data/1971totextinct.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cext1$dataset<-factor(cext1$dataset, levels=fsorder())
gcdf(cext1, , "Probability of extinction", "Ext_1971",,,)

c1decl90 <- read.csv("./data/1971totdecl90.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
c1decl90$dataset<-factor(c1decl90$dataset, levels=fsorder())
gcdf(c1decl90, , "Probability of 90% decline", "Del90_1971",,,)

# 1971 extinction & decline
gcdf(cext1, c1decl90, "Probability of extinction / 90% decline", "ExtDel90_1971", lgt2, key3, key4)

# 1971 & 1988
cext2 <- read.csv("./data/1988totextinct.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cext2$dataset<-factor(cext2$dataset, levels=fsorder())
gcdf(cext2, cext1, "Probability of extinction", "Ext_1988", lgt1, key2, key1)

c2decl90 <- read.csv("./data/1988totdecl90.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
c2decl90$dataset<-factor(c2decl90$dataset, levels=fsorder())
gcdf(c2decl90, c1decl90, "Probability of 90% decline", "Del90_1988", lgt1, key2, key1)

# 1988 extinction & decline
gcdf(cext2, c2decl90, "Probability of extinction / 90% decline", "ExtDel90_1988", lgt2, key3, key4)