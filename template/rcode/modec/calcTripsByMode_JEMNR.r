#/
#@filename calcTripsByMode.R
#@author Ben Stabler
#@version 1.4
#@date 7/29/04
#
#Calculate trips by purpose by mode by unique market segment
#Created 3/3/04 benjamin.stabler@odot.state.or.us
#Rewritten to work with collapsed market segment utilities
#from processSegmentUtil.R.  The result is about 1/5 the 
#number of calculations.
#3/9/04 Ben Stabler, added peak and offPeak period loop
#Revised 3/24/04 to work with util<mode><purpose><period>
#
#Revised 4/9/04 to work with choices by income RData files 
#instead of choices as a list of three incomes RData files.
#Also changed code to explicitly calculate each choice set 
#probability instead of creating a weighted average and then
#using that to calculate a weighted average probability.
#
#Revised 4/15/05 to account for cval 0 trips produced in 
#nhbw and nhbnw mode choice
#
#Revised 7/29/04 BTS - removed exp() of common utility 
#since already done in modeChoiceCommon()
#
#This module calculates trips by purpose by mode by income
#by period and saves the results to RData files in the modec folder.
#
#Revised 12/03/13 - Martin Mann
#    1.Set output dimanames zone names not position number dimnames(trips) <- list(1:nrow(trips), 1:ncol(trips), cModes)

#PERCENT TRIPS NOTES
                #If the purpose is nhbw or nhbnw then the calculation of percent trips by
                #unique market segment is a little different.  For starters nhbw and nhbnw mode 
                #choice are not by market segment.  Thus, the input nhbw and nhbnw market
                #segment utility csv files are all zeros.  However, cval 0 (no car) households
                #can't choose mode driveAlone or drivePass.  In order to calculate the percent
                #of trips that make up the cval 0 market segment, the trip production arrays
                #are collapsed and the proportion of trips generated by that market segment
                #is calculated.  This is how the percent of trips by unique market segment is
                #calculated for nhbw and nhbnw.  But nhbw and nhbnw trips productions are redistributed in trip
                #distribution according to trip production utility equations.  This then 
                #loses the ability to track trips produced by market segment by zone.  As an
                #alternative, total regional trips produced by cval 0 divided by the total regional trips
                #produced is calculated and this becomes the percent of trips for market 
                #segment cval 0 for nhbw and nhbnw.  This regional percent is applied to all
                #zones and the trips by mode are then calculated.

#CALC TOTAL UTILITY LOOP NOTES        
                      #Calculate total utility for a unique market segment
                      #Looping by mode, take the exponent of the unique market segment utility
                      #and multiply it by the already exp() common utility for that mode.
                      #Then add the mode utility to the running total utility and if
                      #applicable, the the total not transit utility and the total not
                      #transit or park and ride utility.
                                            #The result is to fill in the trips distribution by mode 
                      #object for the current unique market segment.

#CALC PROBABILILTY NOTES
                      #Calculate probability of mode for each mode for unique market segment
                      #and multiply by percent trips in unique market segment to get trips.
                      #This section calculates the mode probability as the exponent
                      #of the mode specific unique market segment utility times the
                      #mode common utility divided by the total utility.
                      #
                      #The trips for the three different choice sets (all modes, no transit, 
                      #and no transit or park and ride) are calculated by calculating
                      #a mode probability and multiplying the trip dist by the mode
                      #probability, the peak factor, the percent of trips that the unique
                      #market segment accounts for, and the corresponding coverage factor.  

#//
#OLD INCORRECT CALC tripProd[x[1],x[2],,x[3]] / tripsByZone))
   
    fun$calcTripsByMode <- function(purpose, period, outputReport=FALSE) {
          
          cat("Calculate total trips by purpose by mode\n\n")
    
          ##/
          #CALCULATE MODE BY PURPOSE, INCOME, AND PERIOD     
          #@return <period>Trips - Array of Zone To Zone trips by mode by period.  Each purpose and inocme are placed in like named folders
          ##/    
    
          ##/
          #LOAD NON-INCOME SPECIFIC INPUTS
          ##/
              if(outputReport) load("inputs/RData/districts.RData")
              load(paste("inputs/RData/", period, "EmpCov.RData", sep=""))
              load(paste("inputs/RData/", period, "HhCov.RData", sep=""))
              load(paste("tripgen/", purpose, "TripProdAry.RData", sep=""))
              load(paste("modec/", purpose, "/marketUtil.RData", sep=""))
                     
          ##/
          #GET MODE CHOICE SETS
          ##/
          
              cModes <- colnames(marketUtil$uniqueMarketUtil)[4:length(colnames(marketUtil$uniqueMarketUtil))]
              transitModes <- as.character(tapply(modes$mode, modes$type, function(x) x)$transit)
              pandrModes <- as.character(tapply(modes$mode, modes$type, function(x) x)$pandr)
        
          ##/
          #HH AND EMP COVERAGE FACTOR PERCENTS BY MODE CHOICE SET
          #Calculate Percent of trips by transit availability choice set which varies by purpose and period
          #Every zone has park and ride access since each zone is falls within a park and ride shed.  
          #nhbw and nhbnw trip purposes are only a function of employment coverage
          ##/
                      
              if(purpose %in% c("nhbw", "nhbnw")) {
                    transitAvail <- get(paste(period, "EmpCov", sep="")) * t(get(paste(period, "EmpCov", sep="")))
                    transitNotAvail <- 1 - transitAvail
                    parkAndRideAvail <- 0 #no park and ride for nhbw and nhbnw
              }  
              if(!purpose %in% c("nhbw", "nhbnw")) {
                    transitAvail <- get(paste(period, "HhCov", sep="")) *  t(get(paste(period, "EmpCov", sep="")))
                    transitNotAvail <- 1 - t(get(paste(period, "EmpCov", sep="")))
                    parkAndRideAvail <- (1 - get(paste(period, "HhCov", sep=""))) * t(get(paste(period, "EmpCov", sep="")))
              }
              rm(list=c(paste(period, "EmpCov", sep=""), paste(period, "HhCov", sep="")))
        
          ##/
          #PREPROCESS TRIP PRODARY
          #Collapse on age and create zonal trip productions by income
          ##/
        
              tripProdAll <- get(paste(purpose, "TripProdAry", sep=""))
              tripProdAll <- apply(tripProdAll, c(1,2,3,5,6), sum)
              rm(list=paste(purpose, "TripProdAry", sep=""))

        
          ############## START INCOME LOOP #################################################################################
          ##################################################################################################################

          ##/    
          #LOOP BY INCOME FINDING MODE UTILITLY BY PURPOSE AND PERIOD
          ##/
          
              incomes <- c("lowInc","midInc","highInc")
              for(income in incomes) {            
      
                    ##/LOAD INCOME SPECIFIC INPUTS ########################                                     
                        
                        cat(paste("\nLoad mode utilities for ", purpose, " ", income, " ", period, "\n\n", sep=""))
          
                        #CURRENT PURPOSE AND INCOME TRIP DISTRIBUTION DATA                                            
                        curDistName <- paste(purpose, income, "Dist", sep="")
                        load(paste("tripdist/",curDistName,".RData", sep=""))
                        assign("curTripDist", get(curDistName))
                        rm(list=curDistName)
                        
                        #CURRENT PURPOSE AND INCOME COMMON UTILITIES FOR MODES                      
                        #Load the common utility (non-market segment specific utility) calculated in modeChoiceCommon.R.        
                        for(rMode in cModes) {
                              ModeIncPurpPeriod <- paste("util",rMode, income, purpose, period, sep="")
                              ModePurpPeriod <- paste("util",rMode, purpose, period, sep="")
                              curMat <- sapply(c(ModeIncPurpPeriod,ModePurpPeriod),function(x) file.exists(paste("modec/common/",x,".RData", sep="")))
                              load(paste("modec/common/",names(curMat)[curMat],".RData", sep=""))
                              assign(paste(rMode, "Util", sep=""),get(names(curMat)[curMat]))
                              rm(list=paste(names(curMat)[curMat], sep=""))
                        }
                  
                    ##/PROCESS TRIP PRODUCTION INOOME SPECIFIC INPUTS ######################## 
                       
                        #For each zone, calculate trip productions by size, workers, autos, for income group
                        indx <- which(c("lowInc","lowInc","midInc","highInc")%in%income)
                        tripProd <- tripProdAll[,,indx,,]
                        if(length(indx)>1) tripProd <- apply(tripProd[,,indx,,],c(1,2,4,5),sum)
                        tripsByZone <- apply(tripProd, c(3), sum)
                                                             
                    ##/CALCULATE PERCENT TRIPS BY UNIQUE MARKET SEGMENT ########################                                            
                        
                        cat("Calculate percent trips by unique market segment combinations\n\n")
                    
                        #Calculate percent productions by each market segment by zone
                        percentTrips <- t(apply(marketUtil$marketUtil[,1:3], 1, function(x) tripProd[x[2],x[1],,x[3]] / tripsByZone))
                        percentTrips[is.na(percentTrips)] <- 0
                        rownames(percentTrips) <- apply(marketUtil$marketUtil[,1:3],1,function(x) paste(x,collapse=" "))
                        
                        #Store a list of the percent trips by market grouped by unique market segment
                        percentTripsLst <- split(as.data.frame(percentTrips),marketUtil$marketUtil[,"index"])
                        names(percentTripsLst) <- unique(marketUtil$marketUtil$index)
                      
                        #Calculate percent trip productions by unique market segment by zone
                        pTrips <- as.matrix(aggregate(percentTrips, list(marketUtil$marketUtil$index),function(x) sum(x))[,-1])
                        pTrips[is.na(pTrips)] <- 0
                        dimnames(pTrips) <- NULL
                        
                        #For nhbw and nhbnw pTrips is percent of regional trips for cval 0 and non cval 0 market segments.
                        if(purpose %in% c("nhbw", "nhbnw")) {
                               #There can only be two unique market segments for nhbw and nhbnw.
                               if(nrow(marketUtil$uniqueMarketUtil) != 2) stop(paste("Error in ", purpose, " market segment coefficients", sep="")) 
                               cval0Percent <- sum(tripProd[,,,"c1"])/sum(tripProd)
                               pTrips[1,] <- cval0Percent
                               pTrips[2,] <- 1 - cval0Percent
                               rm(cval0Percent)
                          }    
                      
                    ##/CALCULATE TRIPS BY MODE BY UNIQUE MARKET SEGMENT ######################## 
                  
                        cat(paste("Calculate unique market segment utilities for ", income, "\n\n", sep=""))
                    
                        #PREPARE NEEDED OUTPUT TABLES                       
                        #Collapsed trip table for all market segments
                        trips <- array(0, c(dim(transitAvail), length(cModes)))
                        
                        #Total utility matrices.
                        totalUtil <- transitNotAvail #to get data structure only, zeroed out below
                                    
                        #Get peak factor
                        pFactor <- get(paste(purpose, "PeakFactor", sep=""))
                        if(period == "offPeak") pFactor <- 1 - pFactor                
                                
                    ########## START MARKET SEGMENT LOOP ###################################################################
                    #########################################################################################################
                        
                    ##/CALC PROBABILITY LOOP THROUGH ALL UNIQUE MARKET SEGMENTS ######################## 
                        for(i in 1:nrow(marketUtil$uniqueMarketUtil)) {                                                 
                              
                              #CALCULATE TOTAL UTILITIES BY MODE LOOP 
                              
                              #Initialize current unique market seqment utilities
                              totalUtil[] <- 0
                              totalNotTransitUtil <- totalUtil
                              totalNotTransitOrPRUtil <- totalUtil 
                        
                              #Add market segment specific utility to the mode common utility and then summing the utilities for all the modes                                
                              for(j in 1:length(cModes)) {                            
                                    #Get market segment ID and market segment specific utility
                                    segmentUtil <- exp(marketUtil$uniqueMarketUtil[i, cModes[j]])
                    
                                    #If unique market segment utility for mode is NA then skip this mode
                                    if(!is.na(segmentUtil)) {                       
                                          #Get current mode period common utility
                                          modeUtil <- get(paste(cModes[j], "Util", sep=""))                       
                                          #totalUtil: for all modes
                                          totalUtil <- totalUtil + (modeUtil * segmentUtil)                                                                  
                                          #totalNotTransitUtil: if mode is not a transit mode
                                          if(!(cModes[j] %in% transitModes)) {
                                              totalNotTransitUtil <- totalNotTransitUtil + (modeUtil * segmentUtil)                                                          
                                              #totalNotTransitOrPRUtil: if mode is not transit and not parkAndRide
                                              if(!(cModes[j] %in% pandrModes)) totalNotTransitOrPRUtil <- totalNotTransitOrPRUtil + (modeUtil * segmentUtil)                                                     
                                          }
                                    }
                              }
     
########################################################################################################

                              #CALCULATE PROBABILITY  LOOP
                              #UniqSegmentProbabilty = ( modeUtil * segmentUtil ) / totalUtil  - for all modes, transit, and P&R
                              #curSegTrips <- pTrips[i,] * pFactor(scalar) * transitAvail(mat) * curTripDist(mat)                                 
                              #curSegModeTrips <-  UniqSegmentProbabilty * curSegTrips
                              #trips <- prevSegModeTrips + curSegModeTrips
                              
                              for(j in 1:length(cModes)) {
                                    segVec <- marketUtil$uniqueMarketUtil[i,1:3]                          
                                    cat(paste("Purpose: ", purpose, "\tUnique Market Segment: ", "h", segVec["h"], "w", segVec["w"], "c", segVec["c"], "\tMode: ", cModes[j], "\n", sep=""))
                                
                                    #Set currentTrips to zero
                                    currentTrips <- 0
                                    
                                    #Get current mode market segment utility and common mode period utility
                                    segmentUtil <- exp(marketUtil$uniqueMarketUtil[i, cModes[j]])
                                    modeUtil <- get(paste(cModes[j], "Util", sep=""))                    
 
                                    #If unique market utility for mode is NA then skip this mode
                                    if(!is.na(segmentUtil)) {       
                                          #Transit Available Trips
                                          currentTrips <- currentTrips + (modeUtil * segmentUtil / totalUtil) * pFactor * pTrips[i,] * transitAvail * curTripDist
                                         
                                          #If mode is not a transit mode then calculate trips for mode for choice set where transit choices not available (but park and ride is)
                                          if(!(cModes[j] %in% transitModes)) {
                                                currentTrips <- currentTrips + (modeUtil * segmentUtil / totalNotTransitUtil) * pFactor * pTrips[i,] * parkAndRideAvail * curTripDist
 
                                                #If mode is not a transit mode and not a park and ride mode then calculate trips for mode for choice set where transit and park and ride choices are not available
                                                if(!(cModes[j] %in% pandrModes)) currentTrips <- currentTrips + (modeUtil * segmentUtil / totalNotTransitOrPRUtil) * pFactor * pTrips[i,] * transitNotAvail *  curTripDist

                                          } 
                                    }                                                                             
                                    currentTrips[is.na(currentTrips)] <- 0

                                     #Add current unique market segment trips for current mode to total modal trips
                                    trips[,,j] <- trips[,,j] + currentTrips                                      
                                                                      
                              } #end Modes 
                              gc()                   
                        } 
                        
                    ########## END MARKET SEGMENT LOOP #####################################################################
                    #########################################################################################################
                        
                        trips[is.na(trips)] <- 0
                        dimnames(trips) <- list(zoneNames,zoneNames, cModes)
                        
                        cat(paste(income,"\n"))
                        cat(write.table(as.data.frame(apply(trips,c(3),function(x) round(sum(x,na.rm=T),0))),""))
                        
                        assign(paste(period, "Trips", sep=""), trips)                               
                        
                        ##/SAVE RESULTS                                                             
                        save(list=paste(period, "Trips", sep=""), file=paste("modec/", purpose, "/", income, "/", period, "Trips", ".RData", sep=""))
                        
                     ########################################################################################################  
                                
                        ##/PRINT OUT SUMMARY RESULTS
                        if(outputReport == TRUE) {
                            distsum(paste(period, "Trips", sep=""), paste(purpose, " ", period, " mode choice distribution", sep=""), "ga", 3, paste("modec/", purpose, "/", income, "/", period, "Trips", sep=""), project, initials, districts)
                        }
                        
                        #rm(list=paste(period, "Trips", sep=""),curTripDist,trips)
                        gc() #Force a garbage collection 
              } 
                        
          ############## END INCOME LOOP ###################################################################################
          ##################################################################################################################

    }

########################################## END #################################################################