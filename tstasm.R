# Test count and running sum transformed counts for # their fit to the assumptions of the stochastic model## Filename: tstasm.R# Author: Darren Kavanagh# Created: 31.07.2009# Last Modified: 04.08.2009# ======================================================## Based on  Splus code from Holmes 2004 and Holmes & 
# Fagan 2002 (see ESA Ecological Archives A014-023-S1 
# and Ecological Archives E083-047-S1 respectively)## load libraries and functions source('runsum.R')# ----------------------------- ## Load & Run Analysis           ## ----------------------------- #cdata <- read.csv("./data/nea_spawners.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, comment.char="") cdata$dataset <- factor(cdata$dataset, levels=unique(cdata$dataset))selgrp <- c("South", "North")cdata <- subset(cdata, dsgroup %in% selgrp)L <- 5ordds <- unique(cdata$dataset) # list of datasetsnods <- length(ordds) # number of datasetsNmodtst <- data.frame(dataset=ordds, Nnfpval=numeric(nods), Ntrendp=numeric(nods), Nautocp=numeric(nods), Nddpval=numeric(nods), Noutliners=numeric(nods))

Rmodtst <- data.frame(dataset=ordds, Rnfpval=numeric(nods), Rtrendp=numeric(nods), Rautocp=numeric(nods), Rddpval=numeric(nods), Routliners=numeric(nods), Rclrsq=numeric(nods), Rclpval=numeric(nods))

# Run Tests# ----------# for each datasetfor (j in 1:nods) {	curds <- as.character(ordds[j])          cdssub <- subset(cdata, dataset == curds)    N <- cdssub$total      	# get running sum	rscount <- runsum(N, L)			# get logratios at tau=1
	lenN<-length(N)
	Nlogratios<-log(N[2:lenN]/N[1:(lenN-1)])
    # indices where Nlogratios is not zero; need for trend test
    Nlogratiosyrs<-(1:length(Nlogratios))[is.finite(Nlogratios)]
    # chuck cases where N is zero 
    Nlogratios<-Nlogratios[is.finite(Nlogratios)] 
    
    lenR <- length(rscount)	Rlogratios <- log(rscount[2:lenR])-log(rscount[1:(lenR-1)])	Rlogratiosyrs <- (1:length(Rlogratios))[is.finite(Rlogratios)] 

# Test 1: for normality using a Kolmogorov-Smirnov test	Nnormfit<-ks.test(Nlogratios,y="pnorm")
	Nnfpval <- Nnormfit$p.value
	
	Rnormfit<-ks.test(Rlogratios,y="pnorm")
	Rnfpval <- Rnormfit$p.value
		
# Test 2: check for trends in data	Ntrendstat <- ls.print(lsfit(Nlogratiosyrs,Nlogratios),print.it=F)	Ntrendpval <- Ntrendstat$coef.table[[1]][2,4]  
	
	Rtrendstat <- ls.print(lsfit(Rlogratiosyrs,Rlogratios),print.it=F)	Rtrendpval <- Rtrendstat$coef.table[[1]][2,4]        		# Test 3: check for 1st order autocorrelation # 		  use detrended, mean zero data	lenNlogs <- length(Nlogratios)	Nlogratiodetrend <- Nlogratios-(Ntrendstat$coef.table[[1]][1,1]+Ntrendstat$coef.table[[1]][2,1]*(1:lenNlogs))
	 # ratios in t with ratio at t+1			Nx <- Nlogratiodetrend[1:(lenNlogs-1)]
	# corresponding ratios in t+1	
	Ny <- Nlogratiodetrend[2:lenNlogs] 	Ntmpacf <- cor.test(Nx, Ny, method="spearman")
	Nacpval <- Ntmpacf$p.value
	
	lenRlogs <- length(Rlogratios)	Rlogratiodetrend <- Rlogratios-(Rtrendstat$coef.table[[1]][1,1]+Rtrendstat$coef.table[[1]][2,1]*(1:lenRlogs))
	 # ratios in t with ratio at t+1			Rx <- Rlogratiodetrend[1:(lenRlogs-1)]
	# corresponding ratios in t+1	
	Ry <- Rlogratiodetrend[2:lenRlogs] 	Rtmpacf <- cor.test(Rx, Ry, method="spearman")
	Racpval <- Rtmpacf$p.value
			    
# Test 4: check for denisity-dependance	Nlcntfit <- lm(Nlogratios~N[1:(lenN-1)])		Nlcntdiag <- summary(Nlcntfit)
	Nddpval <- Nlcntdiag$coef[2,4]
	
	Rlcntfit <- lm(Rlogratios~rscount[1:(lenR-1)])		Rlcntdiag <- summary(Rlcntfit)
	Rddpval <- Rlcntdiag$coef[2,4]
	
# Test 5: check for outliers in the logratios
	# Size adjusted cut-off = 2*sqrt(num parameters/num observations) 
	# re Belsley, Kuh, and Welsch (1980) 
    
	outliercutoff <- 2 # values larger are influential
    
	Nmulsfit <- lsfit(rep(1,(lenN-1)),Nlogratios,intercept=F)	Nmulsdiag <- ls.diag(Nmulsfit)  	Nmudfits <- abs(Nmulsdiag$dfits) < outliercutoff #	Rlogratios <- Rlogratios[mudfits] # option to remove outliers
	Noutliers <- prod(Nmudfits)	if(Noutliers==0) {		print(paste("Outliers in", curds, "count logratios"))	    	}	Noutliers <- prod(Nmudfits)
	Rmulsfit <- lsfit(rep(1,(lenR-1)),Rlogratios,intercept=F)	Rmulsdiag <- ls.diag(Rmulsfit)	Rmudfits <- abs(Rmulsdiag$dfits) < outliercutoff #	Rlogratios <- Rlogratios[mudfits] # option to remove outliers
	Routliers <- prod(Rmudfits)	if(Routliers==0) {		print(paste("Outliers in", curds, "running sum logratios"))	    	}	Routliers <- prod(Rmudfits)
	
# Test 6: check if the sigma regression is bad    # get sigma2p using slope of variance versus tau		taumax <- 4	varrunsums <- c()	for (i in 1:taumax)		{		tmp <- log(rscount[(i+1):lenR])-log(rscount[1:(lenR-i)]);		varrunsums <- c(varrunsums,var(tmp));		}	# get r-squared for linearity of variance slope	lmsigma2 <- lm(varrunsums~c(1:length(varrunsums)))	lmsigma2sum <- summary(lmsigma2)
	rclrsq <- lmsigma2sum$r.squared
	rclpval <- lmsigma2sum$coef[2,4]	
	# store results		Nmodtst[j,2:6] <- data.frame(Nnfpval, Ntrendpval, Nacpval, Nddpval, Noutliers)
	
	Rmodtst[j,2:8] <- data.frame(Rnfpval, Rtrendpval, Racpval, Rddpval, Routliers, rclrsq, rclpval)} # end of dataset loop# ----------------------------- ## Generate Output               ## ----------------------------- #fileoutnm <- paste("./output/", "Nmodtst.csv", sep = "")write.table(Nmodtst, file = fileoutnm, sep = ",", col.names = NA,           qmethod = "double")
           
fileoutnm <- paste("./output/", "Rmodtst.csv", sep = "")write.table(Rmodtst, file = fileoutnm, sep = ",", col.names = NA,           qmethod = "double")           