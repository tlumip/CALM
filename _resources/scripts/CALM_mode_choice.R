# CALM Mode Share

library(dplyr)
library(rhdf5)

options(scipen=999)

# set working directory to one level above the scenario folder
scenario_dir = "./template"

# function from reporting
summarizeModeShares <- function(Dir="modec"){
  
  # Define vector of trip purposes exclusive of school
  Pr <- c("hbcoll", "hbo", "hbr", "hbs", "hbw", "nhbnw", "nhbw")
  # Define vector of income groups
  Ic <- c("lowInc", "midInc", "highInc")
  # Define a vector that combines purposes and income
  Pi <- as.vector(t(outer(Pr, Ic, FUN="paste", sep="_")))
  # Define a vector of modes
  Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
  # Define a vector of peak and off-peak trip types
  Ty <- c("peakTrips", "offPeakTrips", "totalTrips")
  noPandR <- c("nhbnw", "nhbw", "hbcoll")
  
  # Initialize a matrix to hold the peak trip results
  Trips.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))
  
  # add in distance matrices for PMT cals
  load(gsub("modec","inputs/RData/autoDist.RData",Dir))
  load(gsub("modec","inputs/RData/tranDist.RData",Dir))
  load(gsub("modec","inputs/RData/bikeDist.RData",Dir))
  load(gsub("modec","inputs/RData/walkDist.RData",Dir))
  
  Dist.ZnZnMd <- array(0, dim=c(dim(autoDist)[1:2],length(Md)), dimnames=list(rownames(autoDist),colnames(autoDist),Md))
  Dist.ZnZnMd[,,1:3] <- autoDist
  Dist.ZnZnMd[,,4:5] <- tranDist
  Dist.ZnZnMd[,,6] <- bikeDist
  Dist.ZnZnMd[,,7] <- walkDist
  Dist.ZnZnMd[is.na(Dist.ZnZnMd)] <- 0
  
  # Initialize a matrix to hold the pmt results
  pmt.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))
  
  # Initialize a matrix to hold the pmt results
  intrazonal.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))
  
  
  # Iterate through each purpose and income group and add data to matrix
  for(ty in Ty[1:2]){
    for(pr in Pr){
      for(ic in Ic){
        DataDir <- paste(Dir, "/", pr, "/", ic, "/", sep="")
        File <- paste(ty, ".RData", sep="")
        Path <- paste(DataDir, File, sep="")
        load(Path)
        Trips.Md <- apply(get(ty), 3, sum)
        intzon <- apply(get(ty), 3, function(x) sum(diag(x)))
        PMT <- apply(get(ty)*Dist.ZnZnMd[,,dimnames(get(ty))[[3]]], 3, sum)
        RowName <- paste(pr, ic, sep="_")
        if(pr %in% noPandR){
          Trips.Md <- c(Trips.Md[1:4], parkAndRideBus=0, Trips.Md[5:6])
          PMT <- c(PMT[1:4], parkAndRideBus=0, PMT[5:6])
          intzon <- c(intzon[1:4], parkAndRideBus=0, intzon[5:6])
        }
        Trips.PiMdTy[RowName,,ty] <- Trips.Md
        pmt.PiMdTy[RowName,,ty] <- PMT
        intrazonal.PiMdTy[RowName,,ty] <- intzon
        
      }
    }
  }
  
  # Add the totals
  Trips.PiMdTy[,,3] <- Trips.PiMdTy[,,1] + Trips.PiMdTy[,,2]
  pmt.PiMdTy[,,3] <- pmt.PiMdTy[,,1] + pmt.PiMdTy[,,2]
  intrazonal.PiMdTy[,,3] <- intrazonal.PiMdTy[,,1] + intrazonal.PiMdTy[,,2]
  
  # Calculate percentages
  RowTotals <- apply(Trips.PiMdTy, c(1,3), sum)
  TripPcts.PiMdTy <- round(100 * sweep(Trips.PiMdTy, c(1,3), RowTotals, "/"), 1)
  
  # Return the result
  ModeSummary <- list(trips=Trips.PiMdTy, percents=TripPcts.PiMdTy, PMT = pmt.PiMdTy, intrazonal=intrazonal.PiMdTy)
  ModeSummary
  
}

## University Mode Share

# Table 65:  Modes
# Code	Description
# 1	Drive-alone, general purpose
# 2	Drive-alone, toll-eligible
# 3	Shared ride 2, general purpose
# 4	Shared ride 2, toll-eligible
# 5	Shared ride 3+, general purpose
# 6	Shared ride 3+, toll-eligible
# 7	Walk
# 8	Bike
# 9	Walk-local transit
# 10	Walk-premium transit
# 11	Park-and-ride transit
# 12	Kiss-and-ride transit

uniTrips = read.csv(paste0(scenario_dir,"/unimodel/outputs/trips.csv"))

# trip modes from order in PB documentation
uniDA = uniTrips %>% filter(tripMode %in% c(1,2)) %>% nrow()
uniHOV = uniTrips %>% filter(tripMode %in% c(3,4,5,6)) %>% nrow()
uniWalk = uniTrips %>% filter(tripMode == 7) %>% nrow()
uniBike = uniTrips %>% filter(tripMode == 8) %>% nrow()
uniTransit = uniTrips %>% filter(tripMode == 9) %>% nrow()
uniTotal = uniDA + uniHOV + uniWalk + uniBike + uniTransit

unitotals = c(uniDA,uniHOV,uniWalk,uniBike,uniTransit) #,uniTotal)

## JEMnR Mode Share
setwd(scenario_dir)
jemnrMode <- summarizeModeShares()
jemnrTrips <- colSums(jemnrMode$trips[,,"totalTrips"])
jemDA      = jemnrTrips[[1]]
jemHOV     = jemnrTrips[[2]]
jemWalk    = jemnrTrips[[7]]
jemBike    = jemnrTrips[[6]]
jemTransit = jemnrTrips[[4]]
jemPNRBus  = jemnrTrips[[5]]
jemTotal = jemDA + jemHOV + jemWalk + jemBike + jemTransit + jemPNRBus

totalTrips = uniTotal + jemTotal

totDA = uniDA + jemDA
totHOV = uniHOV + jemHOV
totWalk = uniWalk + jemWalk
totBike = uniBike + jemBike
totTransit = uniTransit + jemTransit
totPNR = jemPNRBus 

jemnrTrips = c(jemDA,jemHOV,jemWalk,jemBike,jemTransit,jemPNRBus)
uniTrips = c(uniDA,uniHOV,uniWalk,uniBike,uniTransit)
totTrips = c(totDA,totHOV,totWalk,totBike,totTransit,totPNR)
totShares = totTrips / totalTrips

