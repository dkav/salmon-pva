# Calculates basic probability of extinction using # the Dennis-Holmes method (Holmes and Fagan 2002)## Filename: probexti.R# Author: Darren Kavanagh# Created: 04.08.2009# Last Modified: 17.11.2009# ======================================================# # Based on Splus code from Holmes 2004 # (see ESA Ecological Archives A014-023-S1.) ## pops: population size# nc: number of counts in time series# mu: µ determines how quickly the mean increases# sigmasq: σ2 determines how quickly the variance increases# L: number of N's to add together to make runsum# exth: extinction threashold# fts: how far to project into the future# ifts: increment to assess in the the future## load libraries and functionssource('riskmcp.R')probexti <- function (pops=NA, nc=NA, mu=NA, sigmasq=NA, L=5, exth=1, fts=1000, ifts=5) {	nfts <- fts/ifts 	# number of future time points	rsl <- nc-L			# running sum count length		riskext=data.frame(fyear=numeric(nfts), risk=numeric(nfts), riskl95=numeric(nfts), riskh95=numeric(nfts))	
	# Probability calcs
	 # set probability of reaching a lower extinction threshold	if (mu < 0) #declining population    			probext <- 1 # applies to all realisations	else { # accounts for all possible realisations	   		probext <- (exth/pops)^(2*mu/sigmasq)	
		probext[probext>1] <- 1 # cannot be more probable than 1
	}	

	# Confidence intervals calcs -		
	 # set distributions of µ and σ2 for 
	rmcp <- riskmcp(nc,mu,sigmasq,L)	murnds <- rmcp$murnds	s2slprnds <- rmcp$s2slprnds		
		
	# set probability of reaching a lower extinction threshold	
    probextcl <- (exth/pops)^(2*murnds/s2slprnds)
	probextcl[murnds<0] <- 1  # negative growth = eventual extinction
	probextcl[probextcl>1] <- 1 # cannot be more probable than 1

	# set parameters
	cfyr <- 1 # count of current future year	 	for (k in (1:nfts)) {  		cfyr <-  ifts*k # calc future year
 		
		# Calculate risk of ultimate extinction	
 		 # calculate conditional extinction time CDF		G <- pnorm( (-log(pops/exth) + abs(mu)*cfyr)/sqrt(sigmasq*cfyr) ) + ( exp(2*log(pops/exth)*abs(mu)/sigmasq) * pnorm( (-log(pops/exth) - abs(mu)*cfyr)/sqrt(sigmasq*cfyr) ) )							risk <- G*probext		
		risk[risk>1] <- 1 # cannot become more extinct
				# Get confidence intervals on prob of decline 		# via monte carlo simulation following Dennis 1991			
		 # calculate conditional extinction time CDF
				Gcl <- pnorm( (-log(pops/exth) + abs(murnds)*cfyr)/sqrt(s2slprnds*cfyr) ) + ( exp(2*log(pops/exth)*abs(murnds)/s2slprnds)*pnorm( (-log(pops/exth) - abs(murnds)*cfyr)/sqrt(s2slprnds*cfyr) ) )
				
		Gcl[is.na(Gcl)] <- 0 # when NA's present set to zero
		
		# set probability of ultimate extinction		riskcl <- Gcl*probextcl	
				# calculate 95% CLs			riskl95 <- quantile(riskcl,.025)		riskh95 <- quantile(riskcl,.975)		riskh95[riskh95>1] <- 1 # cannot become more extinct				riskext[k,] <- data.frame(cfyr, risk, riskl95, riskh95)	} 	return(riskext)	}