#/
#@filename access.R
#@author Ben Stabler
#@date 4/30/04
#@version 1.4
#
#Calculate various accessibility measures by zone 
#This script calculate the urban design variables
#Zones with high accessibility can get to more destinations 
#easier than zones with low accessibility
#
#Revised 9/19/03 benjamin.stabler@odot.state.or.us
#Revised 1/30/04 benjamin.stabler@odot.state.or.us
#Revised 2/27/04 Ben Stabler, to work with NAs in inputs
#Revised 3/22/04 Ben Stabler, to work with renamed inputs
#Revised 4/30/04 Ben Stabler, to be mode sensitive
#Corrected 2/04/08 Alex Bettinardi, to use trip distance instead of walk time to determine accessibilty. 
#See page 94 of the JEMnR User's Guide for a description.
#Revised 1/06/15 Martin Mann, added walkDist, an r calculated shortest walk distance
#/

      cat("Accessibility Measures\n\n")
      
      fun$access <- function() {
          
          #Create directory to store results
          if(!file.exists("pregen")) dir.create("pregen")
          
          #/
          #LOAD  INPUTS
          #Load walkNet distance matrix, total employment, retail employment, intersection data, and households         
          ##/
          
          load("inputs/RData/walkDist.RData")
          load("inputs/RData/totalEmp.RData")
          load("inputs/RData/retEmp.RData")         
          load("inputs/RData/inthm.RData")
          load("inputs/RData/hhs.RData")
          
          #/
          #CALCULATE MIX ACCESSIBILITY VARIABLES  
          #Half mile from centroid to centroid depends on centroid placement. An equation represent employment within 0.5 miles was prepared.
          #employment = if(distance <= 0.34) then zone employment + if(distance>0.34 & <1 * 0.50 - (distance/2)/distance) then zone employment         
          #@return mixthm - mixed use total employment within a half mile
          #@return mixrhm - total mixed use retail employment within a half mile
          ##/
          
          #CALCULATE HH and EMPLOYMENT WITHIN A HALF MILD OF CENTROID          
          #Weight for distances 1/3  to 1 mile apart
          walkTime.weight <- ((walkDist > .34 & walkDist <=1) * ((.50 - (walkDist/2))/walkDist))
          walkTime.weight[is.na(walkTime.weight)] <- 0
          
          #Calculate tothm: Total emp w/in half mile
          tothm <- apply(walkDist, 1, function(x) sum((x <= 0.34) * totalEmp, na.rm=T)) + apply(walkTime.weight, 1, function(x) sum(x * totalEmp, na.rm=T))
          tothm.mean <- mean(tothm, na.rm=T)
          
          #Calculate rethm: Total retail w/in half mile
          rethm <- apply(walkDist, 1, function(x) sum((x <= 0.34) * retEmp, na.rm=T)) + apply(walkTime.weight, 1, function(x) sum(x * retEmp, na.rm=T))
          rethm.mean <- mean(rethm, na.rm=T)
          
          #Calculate hhhm: Households w/in half-mile
          hhhm <- apply(walkDist, 1, function(x) sum((x <= 0.34) * hhs, na.rm=T)) + apply(walkTime.weight, 1, function(x) sum(x * hhs, na.rm=T))
          hhhm.mean <- mean(hhhm, na.rm=T)
          
          #Calculate intersection mean        
          inthm.mean <- mean(inthm, na.rm=T)         
          
          #CALCULATE MIX ACCESSIBILITY VARIABLES                             
          #Calculate mixthm: Total Employment walk accessibility
          mixthm <- (inthm * (tothm * (inthm.mean / tothm.mean)) *
                   (hhhm * (inthm.mean / hhhm.mean))) /
                   (inthm + (tothm * (inthm.mean / tothm.mean)) +
                   (hhhm * (inthm.mean / hhhm.mean)))
          
          mixthm[is.na(mixthm) | mixthm==0] <- 0.0001
          
          #Calculate mixrhm: Total Employment walk accessibility
          mixrhm <- (inthm * (rethm * (inthm.mean / rethm.mean)) *
                   (hhhm * (inthm.mean / hhhm.mean))) /
                   (inthm + (rethm * (inthm.mean / rethm.mean)) +
                   (hhhm * (inthm.mean / hhhm.mean)))
          
          mixrhm[is.na(mixrhm) | mixrhm==0] <- 0.0001
          
          #Save access objects
          save(mixthm, file="pregen/mixthm.RData")
          save(mixrhm, file="pregen/mixrhm.RData")
        
          ##/
          #CALCULATE TOTAL EMPLOYMENT WITHIN 30 MINIUTES BY TRANSIT
          #Calc Total transit time = in vehicle time + initial wait time (50% headway) + transfer wait time (50% headway) + walk time (each capped at 30 min).                
          #Calc Employment within 30 min for values in the "mode" field of the "modes" table
          #Thirty minutes from centroid to centroid depends on centroid placement. An equation represent employment within 30 minutes was prepared.
          #employment = if(time <= 20) then zone employment + if(distance>20 & <60 * 30 - (time/2)/time) then zone employment          
          #@return tot30t - total employment within  30 minutes by transit from zone centroid
          #/          
          
          if(any(modes$type == "transit")) {      
              #Get transit modes
              tModes <- as.character(modes$mode[modes$type == "transit"])
        
              #Calc Transit times by mode
              for(tMode in tModes) {
                  #Load inputs
                  curWaitTimeAoffPeak <- loadFile(paste("inputs/RData/waitTimeAoffPeak", tMode, ".RData", sep=""))[[1]]
                  curWaitTimeBoffPeak <- loadFile(paste("inputs/RData/waitTimeBoffPeak", tMode, ".RData", sep=""))[[1]]
                  curWalkTimeoffPeak <- loadFile(paste("inputs/RData/walkTimeoffPeak", tMode, ".RData", sep=""))[[1]]
                  curIVTimeoffPeak <- loadFile(paste("inputs/RData/ivTimeoffPeak", tMode, ".RData", sep=""))[[1]]
                    
                  #Calculate total transit time and assign
                  tTime <- curWaitTimeAoffPeak + curWaitTimeBoffPeak + curWalkTimeoffPeak + curIVTimeoffPeak
                  assign(paste(tMode, "TTime", sep=""), tTime)
                    
                  #Remove inputs
                  rm(list=c("curWaitTimeAoffPeak", "curWaitTimeBoffPeak","curWalkTimeoffPeak", "curIVTimeoffPeak", "tTime"))
              }

              #Calc Employment accessibitily by Transit by mode and then total
              tot30t <- 0
              for(tMode in tModes) {             
                  #Calculate mode specific employment within 30 minutes
                  cTime <- get(paste(tMode, "TTime", sep=""))
                  cTime.weight <- (cTime > 20 & cTime <=60) * ((30 - (cTime/2))/cTime)                        
                  tot30c <- apply(cTime, 1, function(x) sum((x <= 20) * totalEmp, na.rm=T)) + apply(cTime.weight, 1, function(x) sum(x * totalEmp, na.rm=T))
                  tot30c1 <- apply(cTime, 1, function(x) sum((x <= 20) * totalEmp * taz$offPeakEmpCov, na.rm=T)) + apply(cTime.weight, 1, function(x) sum(x * totalEmp * taz$offPeakEmpCov, na.rm=T)) 
                  tot30c2 <- apply(cTime, 1, function(x) sum((x <= 20) * totalEmp * taz$offPeakEmpCov, na.rm=T))
                       
                  #Add mode specific number to running total
                  tot30t <- tot30t + tot30c
                  rm(cTime.weight, tot30c, cTime)
              }
      
              #Save access object
              save(tot30t, file="pregen/tot30t.RData")
          }
          
          #No transit 
          if(!any(modes$type == "transit")) {cat(paste("No transit modes in modes object so total employment\n","within 30 minutes of transit not calculated (tot30t)\n\n", sep=""))
          }
      
      }

########################################## END #################################################################