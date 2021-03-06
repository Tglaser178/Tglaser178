#-----------------------------------------------------------------------------
#  Write BBS model to file. Code provided by J. Sauer. (via Jim Sarocco)
#-----------------------------------------------------------------------------
sink(file="bbs_model_13.txt")
cat("
    model{  #### counts and overdispersion effects  ######
    for( k in 1 : ncounts ) {
    log(lambda[k]) <- beta[strat[k]] * (year[k] - fixedyear) + obs[obser[k]] 
    + eta*firstyr[k] + strata[strat[k]] + noise[k] + yeareffect[year[k],strat[k]]
    noise[k] ~ dnorm(0.0, taunoise)
    count[k] ~ dpois(lambda[k])
    zfcount[k] ~ dpois(lambda[k])
    err[k] <- pow(count[k]-lambda[k],2)/lambda[k]
    ferr[k] <- pow(zfcount[k]-lambda[k],2)/lambda[k]
    }
    gof <- sum(err[1:ncounts])
    fgof <- sum(ferr[1:ncounts])
    diffgof <- gof-fgof
    posdiff <- step(diffgof)
    taunoise ~ dgamma(0.001,0.001)
    sdnoise <- 1 / pow(taunoise, 0.5)
    #---------------------------------------------------------#
    #### observer effects  ######
    for( i in 1 : nobservers ) {
    obs[i] ~ dnorm( 0.0,tauobs)
    }
    eta ~ dnorm( 0.0,1.0E-6)
    tauobs ~ dgamma(0.001,0.001)
    sdobs <- 1 / pow(tauobs, 0.5)
    #----------------------------------#
    #### stratum effects  ######
    for( s in 1 : nstrata ) {
    beta[s] ~ dnorm( 0.0,1.0E-6)
    strata[s] ~ dnorm( 0.0,1.0E-6)
    tauyear[s] ~ dgamma(0.001,0.001)
    #### stratum specific year effects  ######
    for( y in 1 : nyears ) {
    yeareffect[y,s] ~ dnorm( 0.0, tauyear[s])
    }
    sdyear[s] <- 1 / pow(tauyear[s],0.5)
    expstrata[s] <- exp(strata[s])
    expbeta[s] <- exp(beta[s])
    overdisp[s] <- 1 + 1/(expstrata[s]*taunoise)
    #-------------------------------------------------#
    }
    
    totareaweight <- sum(areaweight[1:nstrata]) 
    #### summary statistics  ######
    for( i in 1 : nstrata ) {
    for( t in 1 : nyears ) {
    n[i,t] <- nonzeroweight[i]*exp(strata[i] + beta[i]*(t-fixedyear)
    +yeareffect[t,i] + 0.5*sdnoise*sdnoise + 
    0.5*sdobs*sdobs)
    N[i,t] <- areaweight[i]*n[i,t]/totareaweight
    }
    }
    for( i in 1 : nstrata ) {
    B[i] <- pow(n[i,nyears]/n[i,1],1/(nyears-1))
    }
    for( t in 1 : nyears ) {
    CompIndex[t] <- sum(N[1:nstrata,t])
    }
    ### SOTB summary is from 1968 on ###
    for( t in 3 : nyears ) {
    CompSob[t] <- CompIndex[t]/CompIndex[3]
    }
    Bbar <- pow(CompIndex[nyears]/CompIndex[1],1/(nyears-1))
    #-------------------------------------------------#
    }
    ", fill=TRUE)
sink()