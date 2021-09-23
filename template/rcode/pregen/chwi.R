#/
#@filename chwi.R  
#@author Ben Stabler
#@version 1.2
#@date 2/24/04
#
#Car Ownership Model
#Edited by WRS 11/26/02
#Revised 10/1/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 1/15/04 benjamin.stabler@odot.state.or.us
#Revised 1/30/04 benjamin.stabler@odot.state.or.us
#Revised 2/24/04 benjamin.stabler@odot.state.or.us
#   added cvalAry which is just cvalIndexAry 
#   collapsed on the zone dimension
#
#See page 97 of the JEMnR User's Guide for a description. Note that
#the utility functions in the report are not up-to-date.
#/

    cat("Car Ownership Submodel\n\n")

    fun$chwi <- function() {

          ##/
          #CALCULATE CAR OWNERSHIP UTILITIES BY MARKET SEGMENT    
          #@return whiazcAry - Array of cross tabulations of workers,household size, age of householder, by zones 
          #@return cvalIndexAry - cval index by zone
          #@return cvalAry - cval index (without zone dimension)
          ##/ 
          
          ##/    
          #NEEDED FUNCTIONS
          ##/
          
          #Define THE non-zonal component of Car Ownership Utility Function to calculate the utility of a household having a certain number of cars.
          calcUtilityCarOwner <- function(sizeVariables, utilityRow) {
                                    utilityRow["constant"] + utilityRow[paste("hh", sizeVariables["h"], sep="")]  + 
                                    utilityRow[paste("wkr", sizeVariables["w"], sep="")] + 
                                    utilityRow[paste("inc", sizeVariables["i"], sep="")] 
                                  }    
        
          ##/
          #LOAD INPUTS
          ##/
           
          load("inputs/RData/hiazAry.RData")
          #mixthm total employment/intersection density "mix" w/in half mile
          load("pregen/mixthm.RData")
          #tot30t (total employment w/i 30 minutes of transit)
          load("pregen/tot30t.RData")
          load("pregen/whiazAry.RData")
          load("inputs/RData/percentSingleFamily.RData")
          
          ##/
          #DEFINE ALL COMBINATIONS OF HOUSEHOLD SIZE
          ##/
          
          #Define h, w, and i class variables for utility functions
          hClasses <- as.numeric(gsub("h","",segmentsH))
          wClasses <- as.numeric(gsub("w","",segmentsW))
          iClasses <- as.numeric(gsub("i","",segmentsI))
          
          #Create all combinations of h1-4, w1-4, and i1-4
          classesDf <- expand.grid(hClasses,wClasses,iClasses)
          classesDf <- classesDf[order(classesDf[,1],classesDf[,2],classesDf[,3]),]
          colnames(classesDf) <- c("h","w","i")

          ##/
          #CALCULATE NUMBER OF WORKERS UTILITIES
          ##/
          
          #Calculate Car Ownership Utilities
          utilitiesC1 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, carCoeffMtx[1,])))
          utilitiesC2 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, carCoeffMtx[2,])))
          utilitiesC3 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, carCoeffMtx[3,])))
          utilitiesC4 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, carCoeffMtx[4,])))
    
          ##/
          #CREATE A UTILITIES ARRY
          ##/

          #Convert Utility Values to a 5D array of I,W,H,C,Zone and repeat utilities numZones times
          utilitiesAry <- array(c(utilitiesC1, utilitiesC2, utilitiesC3, utilitiesC4), c(numI, numW, numH, numC, numZones))
          #Convert Utilities Array to 6D array by repeating it numA times so it has the age of head dimension since age of head is not a factor in the utility calculation
          utilitiesAry <- array(utilitiesAry, c(numI, numW, numH, numC, numZones, numA))
          utilitiesAry <- aperm(utilitiesAry, c(2,3,1,6,5,4))  #Order is now WHIAZC
          dimnames(utilitiesAry) <- list(segmentsW, segmentsH, segmentsI, segmentsA, zones, segmentsC)
    
          ##/
          #ADD ZONE SPECIFIC VARIABLES TO UTILITIES
          ##/

          #Calculate Mixtot and Tot30t Part of Utility By Zone
          mixtotMtx <- t(sapply(mixthm, function(x) log(x)*carCoeffMtx[,"mixtot"]))
          tot30tMtx <- t(sapply(tot30t, function(x) (x/1000)*carCoeffMtx[,"tot30t"]))
          zoneSpecificUtilDf <- mixtotMtx + tot30tMtx #Dimensions are zone by car ownership class
          
          #Add zone specific portion of utility to each zone utility
          utilitiesAry <- sweep(utilitiesAry, c(5,6), zoneSpecificUtilDf, "+")
      
          #Duplicate utitlitiesAry for single family dwellings by zone
          utilitiesSfAry <- sweep(utilitiesAry, 6, carCoeffMtx[,"sfdwell"], "+")
          
          #Change utility from utility to e^utility
          utilitiesAry <- exp(utilitiesAry)
          utilitiesSfAry <- exp(utilitiesSfAry)
    
          ##/
          #GET PROBABILITy ARRAYS FOR SINGLE OR NONSINGLE FAMILY MARKETS
          ##/
    
          #Calculate probability by car ownership class for non-single family.
          utilitiesSumAry <- apply(utilitiesAry, c(1,2,3,4,5), sum) #sum non-single fam by mrkt segment
          probC1Ary <- utilitiesAry[,,,,,"c1"]/utilitiesSumAry
          probC2Ary <- utilitiesAry[,,,,,"c2"]/utilitiesSumAry
          probC3Ary <- utilitiesAry[,,,,,"c3"]/utilitiesSumAry
          probC4Ary <- utilitiesAry[,,,,,"c4"]/utilitiesSumAry
          
          
          #Calculate probability by car ownership class for  non-single family.
          utilitiesSfSumAry <- apply(utilitiesSfAry, c(1,2,3,4,5), sum) #sum single fam by mrkt segment
          probSfC1Ary <- utilitiesSfAry[,,,,,"c1"]/utilitiesSfSumAry
          probSfC2Ary <- utilitiesSfAry[,,,,,"c2"]/utilitiesSfSumAry
          probSfC3Ary <- utilitiesSfAry[,,,,,"c3"]/utilitiesSfSumAry
          probSfC4Ary <- utilitiesSfAry[,,,,,"c4"]/utilitiesSfSumAry
          
          ##/
          #APPLY ZONAL PROBABILITIES BY SF AND NONSF TO WHIAZAry
          ##/

          #Multiply whiaz array by the percent single  ane nonsingle family array
          whiazSfAry <- sweep(whiazAry, 5, percentSingleFamily, "*")   
          whiazAry <- sweep(whiazAry, 5, (1 - percentSingleFamily), "*") 

          #Apply these two arrays to the two probability arrays families (probSf and prob)          
          
          #Non-Single Familty
          whiaC1Ary <- whiazAry * probC1Ary
          whiaC2Ary <- whiazAry * probC2Ary
          whiaC3Ary <- whiazAry * probC3Ary
          whiaC4Ary <- whiazAry * probC4Ary
          
          #Single Familty
          whiaSfC1Ary <- whiazSfAry * probSfC1Ary
          whiaSfC2Ary <- whiazSfAry * probSfC2Ary
          whiaSfC3Ary <- whiazSfAry * probSfC3Ary
          whiaSfC4Ary <- whiazSfAry * probSfC4Ary
          
          #Sum SF and nonSF arrays for final arrays of number of HHs by car ownership by whia by zone
          whiaC1Ary <- whiaSfC1Ary + whiaC1Ary
          whiaC2Ary <- whiaSfC2Ary + whiaC2Ary
          whiaC3Ary <- whiaSfC3Ary + whiaC3Ary
          whiaC4Ary <- whiaSfC4Ary + whiaC4Ary
    
          ##/
          #BUILD WHIAZCARY
          ##/
          
          whiazcAry <- array(c(whiaC1Ary, whiaC2Ary, whiaC3Ary, whiaC4Ary), c(numW, numH, numI, numA, numZones, numC))
          dimnames(whiazcAry) <- list(segmentsW, segmentsH, segmentsI, segmentsA, zoneNames, segmentsC)
          names(dimnames(whiazcAry)) <- c("worker","hhSize","hhIncome","ageOfHead","zone","car") 
    
          ##/
          #BUILD CVAL INDEX ARRAY
          #cval is the relationship between number of cars and number of workers per household (used in the mode choice model)
          #cval0, cars = 0, cval1, cars < workers, cval2, cars = workers, cval3, cars > workers    
          ##/
        
          #Create empty cvalAry (Dimensions are w, h, i, a, c ) 
          cvalAry <- array(NA, c(numW, numH, numI, numA, numC))
          dimnames(cvalAry) <- dimnames(whiazcAry)[names(dimnames(whiazcAry)) != "zone"]
      
          #CVAL 0
          cvalAry[,,,,"c1"] <- 0
        
          #CVAL 1
          cvalAry["w3",,,,"c2"] <- 1
          cvalAry["w4",,,,"c2"] <- 1
          cvalAry["w4",,,,"c3"] <- 1
        
          #CVAL 2
          cvalAry["w2",,,,"c2"] <- 2
          cvalAry["w3",,,,"c3"] <- 2
          cvalAry["w4",,,,"c4"] <- 2
        
          #CVAL 3
          cvalAry["w1",,,,"c2"] <- 3
          cvalAry["w1",,,,"c3"] <- 3
          cvalAry["w2",,,,"c3"] <- 3
          cvalAry["w1",,,,"c4"] <- 3
          cvalAry["w2",,,,"c4"] <- 3
          cvalAry["w3",,,,"c4"] <- 3
        
          #Save cval without zone dimension and replicated for each zone
          cvalIndexAry  <- cvalAry
          cvalIndexAry <- array(rep(cvalIndexAry, numZones), c(numW, numH, numI, numA, numC, numZones))
          cvalIndexAry <- aperm(cvalIndexAry, c(1,2,3,4,6,5))
          dimnames(cvalIndexAry) <- dimnames(whiazcAry)

          #Print Out Results by cval
          print(round(apply(whiazcAry, "car",sum),1))
          #print(c(tapply(whiazcAry, cvalIndexAry, sum), total=sum(whiazcAry)))
    
          ##/
          #SAVE RESULTS
          ##/
          
          save(cvalAry, file="pregen/cvalAry.RData")
          save(cvalIndexAry, file="pregen/cvalIndexAry.RData")
          save(whiazcAry, file="pregen/whiazcAry.RData")

    }

########################################## END #################################################################