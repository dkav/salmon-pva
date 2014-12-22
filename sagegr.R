# Plot growth rates for different sea age classes
#
# Filename: sagegr.R
# Author: Darren Kavanagh
# Created: 02.11.2009
# Last Modified: 20.11.2009
# ======================================================
# 

# load libraries and functions
library(ggplot2)

# ----------------------------- #
# Load Data                     #
# ----------------------------- #

p1data <- read.csv("./data/seaagegr.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") 

p1data$dataset<-factor(p1data$dataset, levels=unique(p1data$dataset))
p1data <- with(p1data, p1data[order(rev(dataset)), ])
p1data$dataset<-factor(p1data$dataset, levels=unique(p1data$dataset))

# ----------------------------- #
# Generate Output               #
# ----------------------------- #
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# define scales
scol <- scale_colour_manual("Class", c("colr1"="blue","colr2"="red", "colr3"="grey25"), labels=c("1SW", "MSW", "All"))
sshp <- scale_shape_manual("Class", c("shp1"=24,"shp2"=23,"shp3"=3), labels=c("1SW", "MSW", "All"))
ssze <- scale_size_manual("Class", c("siz1"=3,"siz2"=3,"siz3"=1.75), labels=c("1SW", "MSW", "All"))

# define labels and axis
xlabel = expression(lambda); ylabel="Country/Region"
xyl <- labs(x=xlabel, y=ylabel)
xlm <- xlim(0.9, 1.1)

# define legend
lgnd <- opts(legend.background=theme_rect(colour=NA), legend.key=theme_rect(colour=NA))
lgpos <- opts(legend.position=c(0.92, 0.2 ))

# define geometry parameters
gvline <- geom_vline(xintercept = 1, colour="grey50", linetype=2, width = 0.02)

# plot 1
p1 <- ggplot(data=p1data, aes(y=dataset)) 
p1 <- p1 + facet_wrap(~year, ncol=2)
p1 <- p1 + theme_bw() + xyl + xlm
p1 <- p1 + scol + sshp + ssze
p1 <- p1 + gvline
p1 <- p1 + geom_point(aes(x=osw, colour="colr1", shape="shp1", size="siz1"))
p1 <- p1 + geom_point(aes(x=msw, colour="colr2", shape="shp2", size="siz2"))
p1 <- p1 + geom_point(aes(x=total, colour="colr3", shape="shp3", size="siz3"))
p1 <- p1 + lgnd + lgpos
imgnm <- paste("./output/","SA GR Plot ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7.2, height = 5.25)