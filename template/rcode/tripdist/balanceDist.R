#/
#@filename balanceDist.R
#@author Ben Stabler
#@version 1.3
#@date 4/15/04
#Balance hbw and hbcoll Distribution
#Created 1/28/04 benjamin.stabler@odot.state.or.us
#Revised 2/5/04  benjamin.stabler@odot.state.or.us
#Revised 3/29/04 Ben Stabler to work with changed output of tripDist
#Revised 4/15/04 to calculate hbcoll by income
#Revised 5/26/04 added districtScheme
#Revised 11/10/14 minor syntx formatting change - no impact to code. 
#
#See page 113 of the JEMnR User's Guide for a description.
#/

    cat("Balance hbw and hbcoll trip distribution by income\n\n")
  
    fun$balanceDist <- function(purpose, districtScheme=NULL) {
  
          #/
          #BALANCE HBW DISTRIBUTION BY INCOME TO ATTRACTIONS
          #@param hbwTripProdAry - hbw trip production array
          #@param hbwAttractions - hbw attractions by zone from trip generation
          #@param hbw<income>DistAttract - total attractions by income by zone from destination choice
          #@param hbwDistAttract - total attractions by zone from destination choice
          #@param districts - district definitions
          #@param hbwProd - total productions by income by zone
          #@param hbw<income>Prod - total highInc productions by zone
          #@return hbw<income>Dist - adjusted distribution by income 
          ##/  
  
          if(purpose == "hbw") {
              cat("Balance hbw trip distribution by income\n\n")
              
              #LOAD INPUTS
              load("inputs/RData/districts.RData")        
              load("tripgen/hbwTripProdAry.RData") 
              load("tripgen/hbwAttractions.RData")
              
              #Collapse Trip Productions by income and zone
              hbwProd <- apply(hbwTripProdAry, c(3,5), sum) #total productions by income by zone
              hbwlowIncProd <- hbwProd["i1",] + hbwProd["i2",]
              hbwmidIncProd <- hbwProd["i3",]
              hbwhighIncProd <- hbwProd["i4",]
              
              #Sum total attractions for all income group produced by destination choice model by zone
              hbwDistAttract <- rep(0, length(hbwAttractions))
              for(income in c("lowInc", "midInc", "highInc")) {
                  load(paste("tripdist/hbw", income, "Dist.RData", sep=""))
                  currentSums <- colSums(get(paste("hbw", income, "Dist", sep="")))
                  #print(sum(currentSums))
                  assign(paste("hbw", income, "DistAttract", sep=""), currentSums)
                  hbwDistAttract <- hbwDistAttract + currentSums
                  rm("currentSums")
              }        
          
              #Balance           
              for(income in c("lowInc", "midInc", "highInc")) {            
                  curDistMat <-  paste("hbw", income, "Dist", sep="")
                  curAttactMat <- paste("hbw", income, "DistAttract", sep="")
                  
                  #Factor total attractions by income by zone from destination choice trips by ratio of attractions produced in trip generation
                  #to total attractions for all income group from destination choice.            
                  attrAdjusted <- get(curAttactMat) * (hbwAttractions/hbwDistAttract)
                  attrAdjusted[hbwDistAttract == 0] <- 0
                  
                  #2D balance via IPF
                  #SEED: current Purpose and income distribution
                  #ROWCONTROLS: current Purpose and income productions by zone
                  #COLCONTROLS:  current Purpose and income work destination adjustred to ratio of employment by zone / trip destinations by zone
                  distAdjusted <- balance(get(curDistMat), get(paste("hbw", income, "Prod", sep="")), attrAdjusted, 0.0001, 20)
                  assign(curDistMat, distAdjusted)              
                  rm(attrAdjusted, distAdjusted)
                  
                  #Save distribution by income
                  save(list=curDistMat, file=paste("tripdist/hbw", income, "Dist.RData", sep=""))                  
                  
                  #District summary report
                  if(!is.null(districtScheme)) distsum(curDistMat, paste("hbw Distribution - ", income, sep=""), districtScheme, 3, "tripdist/hbwdist", project, initials, districts)
                  
                  rm(list=curDistMat)
              }
              rm(hbwProd, hbwlowIncProd, hbwmidIncProd, hbwhighIncProd, hbwlowIncDistAttract, hbwmidIncDistAttract, hbwhighIncDistAttract, hbwDistAttract, hbwAttractions)
      
          }
      
      
          #/
          #BALANCE HBCOLL DISTRIBUTION BY INCOME TO ATTRACTIONS
          #hbcoll is balanced collapsed on income
          #Calculate percent of trips by income to split balanced trip distribution.
          #@param hbcollTripProdAry - hbcoll trip production array
          #@param hbcollAttractions - hbcoll attractions by zone from trip generation
          #@param hbcoll<income>Dist - hbcoll raw trip distribution by income
          #@param districts - district definitions
          #@param hbcollDist - hbcoll raw trip distribution (low + mid + high)
          #@param hbcollProductions - hbcoll trip production by zone
          #@param pTripslowInc - percent low income trips
          #@param pTripsmidInc - percent mid income trips
          #@param pTripshighInc - percent high income trips
          #@return hbcoll<income>Dist - hbcoll distribution to RData file by income
          ##/ 
      
          if(purpose == "hbcoll") {
              cat("Balance hbcoll trip distribution by income\n\n")
    
              #LOAD INPUTS
              load("inputs/RData/districts.RData")
              load("tripgen/hbcollTripProdAry.RData")
              load("tripgen/hbcollAttractions.RData")
              load("tripdist/hbcolllowIncDist.RData")
              load("tripdist/hbcollmidIncDist.RData")
              load("tripdist/hbcollhighIncDist.RData")
              
              #Collapse Trip Productions by zone
              hbcollProductions <- apply(hbcollTripProdAry, 5, sum)
              hbcollDist <- hbcolllowIncDist + hbcollmidIncDist + hbcollhighIncDist
              
              #Calculate percent of trips by income by zone
              pTripslowInc <- hbcolllowIncDist / hbcollDist
              pTripsmidInc <- hbcollmidIncDist / hbcollDist
              pTripshighInc <- hbcollhighIncDist / hbcollDist
              pTripslowInc[is.na(pTripslowInc)] <- 0
              pTripslowInc[is.infinite(pTripslowInc)] <- 1
              pTripsmidInc[is.na(pTripsmidInc)] <- 0
              pTripsmidInc[is.infinite(pTripsmidInc)] <- 1
              pTripshighInc[is.na(pTripshighInc)] <- 0
              pTripshighInc[is.infinite(pTripshighInc)] <- 1

              #2D balance via IPF
              #SEED: College trip distribution
              #ROWCONTROLS: Total zonal college trip productions
              #COLCONTROLS:  Total zonal college trip Attraction            
              hbcollDist <- balance(hbcollDist, hbcollProductions, hbcollAttractions, 0.0001, 20)
             
              for(income in c("lowInc", "midInc", "highInc")) {
                  curDistMat <- paste("hbcoll", income, "Dist", sep="")
                  
                  #Proportion Balanced Dist for current income
                  assign(curDistMat, hbcollDist * get(paste("pTrips", income, sep="")))
                  
                  #Save
                  save(list=curDistMat, file=paste("tripdist/hbcoll", income, "Dist.RData", sep=""))
      
                  #District summary report
                  if(!is.null(districtScheme))  distsum(curDistMat, paste("hbcoll Distribution - ", income, sep=""), districtScheme, 3, "tripdist/hbcolldist", project, initials, districts)

                  rm(list=paste("hbcoll", income, "Dist", sep=""))
             }
      
      	  rm(hbcollProductions, hbcollTripProdAry, hbcollAttractions, pTripslowInc,
                pTripsmidInc, pTripshighInc)
      
          }  
    }
  
########################################## END #################################################################
