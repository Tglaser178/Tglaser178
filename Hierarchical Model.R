##############################################################################
# Analysis of  bbs data to get trends in Northeastern states/ "landscapes"
# Created by C. Taylor May 13 2016
# Based on code sent by Jim Sarroco for WIWA BBS analysis (wiwa_network_bbs_model1.R)
# which  was in turn based on code from J. Sauer
##############################################################################
rm(list=ls())

library("ff") # to read in large BBS data set
library("rgeos")
library("rgdal")
library("maptools")
library("sp")
library("jagsUI")
library('tidyverse')

SPECIES="Wilson's Warbler"
YEAR=c(1967:2018)
OUTFILE="DATA/outBBS/BBS_WIWA_BCR.RData"



out<-list()
#-----------------------------------------------------------------------------
# 1 - Read in and process BBS data. Data downloaded 3.08.16 from 
#     ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/
#-----------------------------------------------------------------------------

# read species list to identify species numbers

df_2 <- read.csv("BBS_WIWA_Filtered.csv")
BCR=unique(df_2$BCR)

# Create unique route field 
bbs.dat$Route <- factor(do.call(paste, list(bbs.dat$BCR, bbs.dat$Route, sep=".")))  
# select years of interest
### bbs.dat <- bbs.dat[bbs.dat$year[2008:2018]]  ###Already filtered


### out$nRoutes<-length(unique(bbs.dat$Route))

# read observer data
obs.dat <- read.csv("WIWA_BBS_metadata.csv") 
# select only regular bbs data; i.e., no double observers, replication

### obs.dat <- obs.dat[obs.dat$RPID%in%"101",]
### obs.dat <- obs.dat[,c(1:6,9)] # get rid of extra data
obs.dat$Rte <- factor(do.call(paste, list(obs.dat$state, obs.dat$route, sep=".")))
# observer ID based on route x observer
obs.dat$obs <- factor(do.call(paste, list(obs.dat$Obs, obs.dat$Rte, sep="."))) 
### obs.dat <- obs.dat[obs.dat$Rte %in% unique(bbs.dat$Rte),] # select routes of interest
### obs.dat <- obs.dat[obs.dat$Year>(YEAR[1]-1) & obs.dat$Year<(YEAR[2]+1),]# select years of interest

# merge observers and years surveyed to count data
bbs.dat <- merge(bbs.dat, obs.dat, all.y=T) # merge observer and count data
# fill in zeros for years where route counted with no obs.
bbs.dat$WIWA_count[is.na(bbs.dat$WIWA_count)] <- 0 
# create first year indicator variable
bbs.dat$fy <- ifelse(duplicated(bbs.dat$ObsN), 0, 1) 


# read in route data, which contains location info
routes = read.csv("WIWA_BBS_metadata.csv")
routes$Rte <- interaction(routes$state, routes$route) 
bbs.dat <- merge(bbs.dat, routes) #merge route and count/obs data
bbs.locs <- bbs.dat[,c("Longitude","Latitude")] # extract coordinates

#-----------------------------------------------------------------------------
# 2 - Read in and process spatial data, match to BBS data
#-----------------------------------------------------------------------------


# Read in regions = state shapefile
REGIONS <- readOGR(dsn="bcr_terrestrial_shape",layer="BCR_Terrestrial_master_International")
#regions.proj=proj4string(REGIONS)
#project to Albers equal area so areas calcaulted will be in sq meters
REGIONS<-spTransform(REGIONS,CRS("+proj=aea +ellps=WGS84 +lat_1=29.5 +lat_2=45.5"))

bbs.BCR=c(2,4,5,6,7,8,9,10,12,14,15,16,32)
BCR=data.frame(BCR=bbs.BCR, AreaBCR=sapply(1:length(BCR),function(i){gArea(subset(REGIONS,BCR==BCR[i]),byid=T)}))


BCR$BCR=as.factor(bbs.BCR)
AreaBCR.df=as.data.frame(tapply(BCR$AreaBCR,BCR$BCR,sum,simplify=T))
names(AreaBCR.df)<-c("AreaBCR")
AreaBCR.df$AreaBCR<-row.names(AreaBCR.df)
BCR=merge(BCR,AreaBCR.df,by.x="BCR")
bbs.dat=merge(bbs.dat,BCR,by.x="BCR")
bbs.dat$BCR<-factor(bbs.dat$BCR,levels=as.character(unique(bbs.dat$BCR)))  # remove strata from which ther are no observations
#-----------------------------------------------------------------------------
#  Write BBS model to file. Code provided by J. Sauer. (via Jim Sarocco)
#-----------------------------------------------------------------------------


source("LinkSauerModel.R")

#-----------------------------------------------------------------------------
# 4 - prep/bundle data for bbs trend model, set inits, run model
#-----------------------------------------------------------------------------

count <- df_2$SpeciesTotal
ncounts <- length(df_2$SpeciesTotal)
obser <- df_2$ObsN
obser <- as.numeric(factor(df_2$ObsN))
nobservers <- length(unique(obser))   
firstyr <- bbs.dat$fy
year <- as.numeric(factor(df_2$Year))
nyears <- length(unique(year))
strat <- as.numeric(df_2$BCR)     
nstrata <- length(unique(strat))


aw <- unique(subset(bbs.dat, select = c(BCR, AreaBCR)))
aw <- aw[order(aw$BCR),]
areaweight <- aw$AreaBCR

# calculate z weights 
rte.all <- read.csv("routes.csv") 
rte.all$Rte <- factor(do.call(paste, list(rte.all$BCR, rte.all$Route, sep=".")))
### rte.all <- rte.all[rte.all$Year>(YEAR[1]-1) & rte.all$Year<(YEAR[2]+1),]

rte.all=rte.all[sapply(rte.all$BCR,function(x){any(x==BCR$BCR)}),]
BCR.df=data.frame(BCR=as.factor(bbs.BCR))
rte.all=merge(rte.all,BCR.df,by="BCR")   ### need to fix

Rte <- read.csv("routes.csv") 
rte.sum <- aggregate(Rte~BCR, rte.all, length) # use BCR scale here
names(rte.sum)[2] <- "tot.rtes"
spec.rte.sum <- aggregate(Rte~BCR, unique(subset(bbs.dat, select=c(Rte, BCR))), length)
names(spec.rte.sum)[2] <- "detec.rtes"
wts <- merge(spec.rte.sum, rte.sum)
wts$nonzeroweight <- wts$detec.rtes/wts$tot.rtes
wts<-wts[order(wts$BCR),]
nonzeroweight <- wts$nonzeroweight




### bundle data:
nYears<-YEAR[2018]-YEAR[2008]
jags.data <- list(count=count, year = year, obser=obser,  nyears=nyears, firstyr=firstyr, ncounts=ncounts, strat=strat,  
                  nobservers=nobservers, nstrata=nstrata, areaweight=areaweight, nonzeroweight=nonzeroweight, fixedyear=round(nYears/2))

# Initial values
inits <- function(){
  list(tauyear=rep(1,nstrata),taunoise=1,tauobs=1,beta=rep(0,nstrata),strata=rep(0,nstrata),eta=0)
}  

# Parameters monitored
parameters <- c("eta", "n", "sdnoise", "sdobs", "CompIndex", "Bbar")

# MCMC settings
ni <- 50000
nt <- 3
nb <- 10000
nc <- 3

print("Calling JAGS")
bbs.out <- jags(jags.data, inits, parameters, "bbs_model_13.txt",n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel=TRUE)
bbs.BCR=levels(bbs.dat$BCR)
bbs.years=YEAR
bbs.areas<-as.data.frame(aw,row.names=1:nrow(aw))



save(bbs.out,bbs.BCR,bbs.areas,bbs.years,file=OUTFILE)


# years<-seq(1966,2014)
# reg.counts<-matrix(NA,length(years),11)
# reg.ci.025<-matrix(NA,length(years),11)
# reg.ci.975<-matrix(NA,length(years),11)
# for(i in 1:length(years)) reg.counts[i,]<-apply(bbs.out$sims.list$n[,,i],2,mean)
# for(i in 1:length(years)) reg.ci.025[i,]<-apply(bbs.out$sims.list$n[,,i],2,quantile, probs=c(0.025))
# for(i in 1:length(years)) reg.ci.975[i,]<-apply(bbs.out$sims.list$n[,,i],2,quantile, probs=c(0.975))



# reg.trends <- 100*((bbs.out$sims.list$n[,,17]/bbs.out$sims.list$n[,,1])^(1/16)-1)
# reg.tr.mn <- apply(reg.trends, 2, mean)
# reg.tr.ci <- apply(reg.trends, 2, quantile, probs=c(0.025, 0.975))
# 
# 
# reg.tr.df <- data.frame(Reg = levels(bbs.dat$Reg), mn = reg.tr.mn, t(reg.tr.ci))

