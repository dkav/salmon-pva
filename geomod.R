# Illustrate population growth in a constant # and stochastic environment
#
# Filename: geomod.R
# Author: Darren Kavanagh
# Created: 14.07.2009
# Last Modified:
# ======================================================
# 

# load libraries and functions
library(ggplot2)
source('geopop.R')

# set variables
stpop <- 10             # starting population
tinter <- 21            # time intervals
maxtime <- tinter-1     # final time
timespan <- 0:maxtime   # timespan vector
avgr <- 1.05            # average growth rate and λ < 1
grs = 1                 # λ = 1 
grd = 0.95              # λ < 1 
varr <- 0.015           # variance of average growth rate
nosim <- 1000          # no of simulations

# ----------------------------- #
# Generate populations          #
# ----------------------------- #
#  λ = 1 
N <- numeric(tinter)
N[1]<- stpop  # set starting population
styN = data.frame(tspan=timespan,value=gmgm(N,grs, maxtime))

#  λ < 1 
N <- numeric(tinter)
N[1]<- stpop 
delN = data.frame(tspan=timespan,value=gmgm(N,grd, maxtime))

# growing population
N <- numeric(tinter)
N[1]<- stpop 

avgN <- data.frame(tspan=timespan,value=gmgm(N,avgr, maxtime))

# reset variables
tinter <- 30            
maxtime <- tinter-1     
timespan <- 0:maxtime 

# simulation loop
allN <- array(,dim=c(tinter, nosim))
for (i in 1:nosim) {
    N <- numeric(tinter)
    N[1]<- stpop            
    # time interval loop
    for (ctime in 1:maxtime) {
        r <- avgr + sqrt(varr) * rnorm(1)  # get a random growth rate
        N[ctime+1] <- r * N[ctime]           # calc new population
    }
    allN[,i] <- N
}

# transform wide array into a long data frame
allN <- melt(allN, varnames=c("gpno","grp"))
allN <- data.frame(allN, tspan=timespan)

# dataset for probability density plot
sallN <- subset(allN, allN$tspan %in% c(seq(5,25,5)))

t20allN <- subset(allN, allN$tspan < 21)
lastNs <- subset(t20allN, t20allN$tspan == 20)
# time series with highest pop at tn
maxgp <- lastNs[which.max(lastNs$value),"grp"]
maxts = subset(t20allN, grp==maxgp)
# time series with lowest pop at tn
mingp <- lastNs[which.min(lastNs$value),"grp"]
mints = subset(t20allN, grp==mingp)

lnt20allN <- sallN
lnt20allN$value <- log(lnt20allN$value)

# ----------------------------- #
# Generate Output               #
# ----------------------------- # 	
filedt <- format(Sys.time(), "%m%d%H%M") # Get timestamp for files

# Plot with NO stochasticity
# ---------------------
p1=ggplot(avgN, aes(x=tspan, y=value)) 
# λ > 1
p1 <- p1 + geom_line(colour="blue") + geom_point(colour="blue", shape=15) 
#  λ = 1 
p1 <- p1 + geom_line(data=styN, colour="green") + geom_point(data=styN, colour="green", shape=18)
#  λ < 1
p1 <- p1 + geom_line(data=delN, colour="red") + geom_point(data=delN, colour="red", shape=16) 
p1 <- p1 + labs(x="Time, t", y="Abundance, N(t)")

imgnm <- paste("./output/","Geom Pop ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7, height = 5)

# Plot with stochasticity
# -----------------------
p2 <- ggplot(t20allN, aes(x=tspan, y=value)) 
p2 <- p2 + geom_line(aes(group=grp), colour="grey50") 
# max line
p2 <- p2 + geom_line(data=maxts, colour="blue") + geom_point(data=maxts, colour="blue", shape=18)
# min line
p2 <- p2 + geom_line(data=mints, colour="red") + geom_point(data=mints, colour="red", shape=16) 
# avg line
p2 <- p2 + geom_line(data=avgN, colour="green") + geom_point(data=avgN, colour="green", shape=15) 
p2 <- p2 + labs(x="Time, t", y="Abundance, N(t)")

imgnm <- paste("./output/","Stoch Pop ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7, height = 5)

# Plot of probability density
# ---------------------------
p3 <- ggplot(sallN, aes(x=value, group=tspan))
p3 <- p3 + geom_density(adjust=5, aes(colour=tspan))
p3 <- p3 + coord_cartesian(xlim = c(0, 100)) + labs(x="Abundance, N(t)") + scale_y_continuous("Probability Density") + opts(legend.position = "none")

imgnm <- paste("./output/","Stoch Dens ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7, height = 5)

# Plot of Normal Distribution
# ---------------------------
p4 <- ggplot(lnt20allN, aes(x=tspan, y=value)) 
p4 <- p4 + geom_line(aes(group=grp), colour="grey75") 
p4 <- p4 + labs(x="Time, t", y="Log of Abundance, Log N(t)")
stat_sum_df <- function(fun, geom="crossbar", ...){ stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)}
p4 <- p4 + stat_sum_df("mean_sdl", mult=1) + stat_summary(fun.y = "mean", geom="line", colour="red")

imgnm <- paste("./output/","Norm Dist ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7, height = 5)
