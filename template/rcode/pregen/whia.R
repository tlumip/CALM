#/
#@filename whia.R
#@author Ben Stabler
#@version 1.1
#@date 1/30/04
#
#Calculate number of HHs by number of workers, HH size, HH income group, and age of head
#
#Revised 9/20/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 1/15/04 benjamin.stabler@odot.state.or.us
#Revised 1/30/04 benjamin.stabler@odot.state.or.us
#
#See page 96 of the JEMnR User's Guide for a description. Note that
#the utility functions in the report are not up-to-date.
#/

    cat("Worker Submodel\n\n")
    
    fun$whia <- function() {
        
          ##/
          #CALCULATE NUMBER OF WORKERS UTILITIES BY MARKET SEGMENT      
          #@return whiazAry - Array of cross tabulations of workers,household size, age of householder, by zones 
          ##/    
        
          ##/
          #NEEDED FUNCTIONS
          ##/
              
              #Define Worker Utility Function
              calcUtilityWorker <- function(sizeVariables, utilityRow) {
                                      exp( utilityRow["constant"] + utilityRow["hhsize"]*sizeVariables["h"] +
                                      utilityRow[paste("inc", sizeVariables["i"], sep="")] +
                                      utilityRow[paste("age", sizeVariables["a"], sep="")])
                                    }    
        
          ##/
          #LOAD INPUTS
          ##/
           
              load("inputs/RData/hiazAry.RData")
        
          ##/
          #DEFINE ALL COMBINATIONS OF HOUSEHOLD SIZE, INCOME, AGE OF HOUSEHOLDER
          ##/
            
              #Define h, i, and a class variables for utility functions
              hClasses <- as.numeric(gsub("h","",segmentsH))
              iClasses <- as.numeric(gsub("i","",segmentsI))
              aClasses <- as.numeric(gsub("a","",segmentsA))
               
              #Create all combinations of h1-4, i1-4 and a1-4
              classesDf <- expand.grid(hClasses,iClasses,aClasses)
              classesDf <- classesDf[order(classesDf[,1],classesDf[,2],classesDf[,3]),]
              colnames(classesDf) <- c("h","i","a")   
           
          ##/
          #CALCULATE NUMBER OF WORKERS UTILITIES
          ##/
          
              #Apply the function, using the workerCoeffMtx to the combinations   
              utilitiesW0 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, workerCoeffMtx[1,])))
              utilitiesW1 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, workerCoeffMtx[2,])))
              utilitiesW2 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, workerCoeffMtx[3,])))
              utilitiesW3 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, workerCoeffMtx[4,])))
    
              #Adujst Utilities: a HH with size of 1 cannot have 2 or 3 workers and a HH of size 2 cannot have 3 workers          
              utilitiesW2[classesDf[,"h"] < 2] <- 0
              utilitiesW3[classesDf[,"h"] < 3] <- 0
    
          ##/
          #GET PROBABILITIES
          ##/
            
              #Create table of all utilities and calculate utility sums by HIA class
              utilitiesAllDf <- cbind(classesDf,utilitiesW0,utilitiesW1,utilitiesW2,utilitiesW3)
              utilitiesAllDf$utilitiesSum <- apply(utilitiesAllDf[,4:7], 1, sum)
            
              #Calculate Number of By Worker Probabilities by HIA class
              probAllDf <- apply(utilitiesAllDf[,4:7], 2, function(x) x/utilitiesAllDf$utilitiesSum)
              colnames(probAllDf) <- c("probW0","probW1","probW2","probW3")
              utilitiesAllDf <- cbind(utilitiesAllDf, probAllDf)
              rm(probAllDf)
        
              #Create Worker Probability Array        
              probAry <- array(unlist(utilitiesAllDf[,9:12]), c(numA, numI, numH, numW))
              #Transpose to hiaw
              probAry <- aperm(probAry, c(3,2,1,4))
              dimnames(probAry) <- list(segmentsH, segmentsI, segmentsA, segmentsW)
       
          ##/
          #CREATE HIAWZARY 
          ##/
          
              #Create a 5D array to hold the h,i,a,w,z data
              hiawzAry <- array(0, c(numH, numI, numA, numW, numZones))
                
              #Calculate number of HHs by number of workers by hia class by zone
              #For each zone rep hia 4 times and multiply by 4 worker class probabilities
              for(i in zones) hiawzAry[,,,,i] <- array(hiazAry[,,,i], c(dim(hiazAry)[1:3], numW)) * probAry
        
                
              #Name the 5 dimensional array (h,i,a,w, zone)
              dimnames(hiawzAry) <- list(segmentsH, segmentsI, segmentsA, segmentsW, zoneNames)
                
              #Reorganize the array dimensions
              whiazAry <- aperm(hiawzAry, c(4,1,2,3,5))
              names(dimnames(whiazAry)) <- c("worker","hhSize","hhIncome","ageOfHead","zone")
    
              #Return Number of HHs by Number of Workers by size by income by age of head by zone
              print(round(apply(whiazAry, "worker",sum),1))
            
          ##/
          #SAVE RESULTS
          ##/
          
              save(whiazAry, file="pregen/whiazAry.RData")
    }

########################################## END #################################################################