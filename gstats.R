# Generate basic stats for count data
#
# Filename: gstats.R
# Author: Darren Kavanagh
# Created: 10.07.2009
# Last Modified: 02.11.2009
# ======================================================
#
#

# load libraries and functions
source('cstats.R')
source('ctrend.R')

# ----------------------------- #
# Load datasets                 #
# ----------------------------- #
# North-East Atlantic datasets #
cdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

# set levels to order in file
cdata$dataset<-factor(cdata$dataset, levels=unique(cdata$dataset))

# ----------------------------- #
# Perform analysis              #
# ----------------------------- #
# 1971-2008 stats
 # total
cstats(cdata$total, cdata$dataset, "Stats 1971 Total")
trendds <- data.frame(dataset=cdata$dataset, year=cdata$year, count=cdata$total)
ctrend(trendds, "Trend 1971 Total")
 # 1SW
cstats(cdata$osw, cdata$dataset, "Stats 1971 1SW")
 # MSW
cstats(cdata$msw, cdata$dataset, "Stats 1971 MSW")

# 10 yr stats
tds <- subset(cdata, year > 1998)
 # total
cstats(tds$total, tds$dataset, "Stats 10 yr Total")
 # 1SW
cstats(tds$osw, tds$dataset, "Stats 10 yr 1SW")
 # MSW
cstats(tds$msw, tds$dataset, "Stats 10 yr MSW")

# 5 yr stats
fds <- subset(cdata, year > 2003)
 # total
cstats(fds$total, fds$dataset, "Stats 5 yr Total")
 # 1SW
cstats(fds$osw, fds$dataset, "Stats 5 yr 1SW")
 # MSW
cstats(fds$msw, fds$dataset, "Stats 5 yr MSW")

# 1988-2008 stats
sds <- subset(cdata, year >= 1988)
 # total
cstats(sds$total, sds$dataset, "Stats 1988 Total")
trendds <- data.frame(dataset=sds$dataset, year=sds$year, count=sds$total)
ctrend(trendds, "Trend 1988 Total")
 # 1SW
cstats(sds$osw, sds$dataset, "Stats 1988 1SW")
 # MSW
cstats(sds$msw, sds$dataset, "Stats 1988 MSW")