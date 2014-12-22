# Illustrate population growth stochastic environment
#
# Filename: geomod.R
# Author: Darren Kavanagh
# Created: 26.01.2010
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
nosim <- 50            # no of simulations

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

# Plot with stochasticity
# -----------------------
p2 <- ggplot(t20allN, aes(x=tspan, y=value)) 
p2 <- p2 + geom_line(aes(group=grp), colour="grey50") 
# max line
#p2 <- p2 + geom_line(data=maxts, colour="blue") + geom_point(data=maxts, colour="blue", shape=18)
# min line
#p2 <- p2 + geom_line(data=mints, colour="red") + geom_point(data=mints, colour="red", shape=16) 
# avg line
#p2 <- p2 + geom_line(data=avgN, colour="green") + geom_point(data=avgN, colour="green", shape=15) 
ylab <- expression(paste("Abundance, N"[(t)]))
p2 <- p2 + labs(x="Time, t", y=ylab)

imgnm <- paste("./output/","Stoch Pop Pres ", filedt, ".png", sep = "")
ggsave(imgnm, width = 7, height = 5)