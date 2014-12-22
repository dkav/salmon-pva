# CDFs for presentation  #
# Filename: prescdf.R
# Author: Darren Kavanagh
# Date: 08.12.2009
# Last Modified: 28.01.2010
# ======================================================

# load libraries and functions
library(ggplot2)

# ----------------------------- #
# Load Data                     #
# ----------------------------- #
# define dataset to use
#ds <- "tot"
#ds <- "osw"
ds <- "msw"

#dy <- "1971"
dy <- "1988"

regn <- c("South", "North")

# load extinction data
cext <- read.csv(paste("./data/", dy, ds,"extinct.csv", sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cext <- subset(cext, cext$region %in% regn & cext$fyear <= 100)
cext$dataset <- factor(cext$dataset, levels=unique(cext$dataset))

# load 90% decline data
cdecl90 <- read.csv(paste("./data/", dy, ds, "decl90.csv", sep = ""), header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 
cdecl90 <- subset(cdecl90, cdecl90$region %in% regn & cdecl90$fyear <= 100)
cdecl90$dataset <-factor(cdecl90$dataset, levels=unique(cdecl90$dataset))
# south
cdecl90s <- subset(cdecl90, cdecl90$region == "South" & cdecl90$fyear <= 100)
cdecl90s$dataset <-factor(cdecl90s$dataset, levels=unique(cdecl90s$dataset))
# north
cdecl90n <- subset(cdecl90, cdecl90$region == "North" & cdecl90$fyear <= 100)
cdecl90n$dataset <-factor(cdecl90n$dataset, levels=unique(cdecl90n$dataset))

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	           
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# define plot variables
sxco <- scale_x_continuous(limits=c(0,100))
syco <- scale_y_continuous(limits=c(0,1))
lget <- scale_colour_hue("Stock")
xlab <-  "Time (years)"
clabsext <- labs(x=xlab, y="Probability of Extinction") 
clabsdel90 <- labs(x=xlab, y="Probability of 90% Decline") 

# extinction #
p1 <- ggplot(data=cext, aes(x=fyear,y=risk, colour=dataset), colour="Stock") 
p1 <- p1 + geom_line() + sxco + syco + clabsext + lget
p1 <- p1 + opts(legend.position=c(0.265,0.67), legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))

imgnm <- paste("./output/","Pres ", dy," ", toupper(ds)," CDF Ext ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5)


# 90% decline #
p2 <- ggplot(data=cdecl90, aes(x=fyear,y=risk, colour=dataset))
p2 <- p2 + geom_line() + sxco + syco + clabsdel90 + lget
p2 <- p2 + opts(legend.position=c(0.265,0.67), legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))

imgnm <- paste("./output/","Pres ", dy," ", toupper(ds)," CDF Del90 ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5)

# south
p2s <- ggplot(data=cdecl90s, aes(x=fyear,y=risk, colour=dataset))
p2s <- p2s + geom_line() + sxco + syco + clabsdel90 + lget
p2s <- p2s + opts(legend.position=c(0.265,0.79), legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))

imgnm <- paste("./output/","Pres ", dy," ", toupper(ds)," CDF Del90 S ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5)

# north
p2n <- ggplot(data=cdecl90n, aes(x=fyear,y=risk, colour=dataset))
p2n <- p2n + geom_line() + sxco + syco + clabsdel90 + lget
p2n <- p2n + opts(legend.position=c(0.2618,0.814), legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))

imgnm <- paste("./output/","Pres ", dy," ", toupper(ds)," CDF Del90 N ", filedt, ".png", sep = "")
ggsave(imgnm, width = 6, height = 5)