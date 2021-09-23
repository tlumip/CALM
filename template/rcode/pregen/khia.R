#/
#@filename khia.R
#@author Ben Stabler
#@version 1.0
#@date 1/30/04
#
#Children Model
#Calculate number of households with by num kids, hh size, income group, age of head
#for the school trip purpose
#
#Revised 10/6/03 benjamin.stabler@odot.state.or.us
#Revised 10/14/03 benjamin.stabler@odot.state.or.us
#Revised 1/13/04 benjamin.stabler@odot.state.or.us
#   Input is whiazAry instead of mf.hia
#Revised 1/30/04 benjamin.stabler@odot.state.or.us
#
#See page 100 of the JEMnR User's Guide for a description. Note that
#the utility functions in the report are not current.
#/

    cat("Child Submodel\n\n")
    
    fun$khia <- function() {
    
          #/
          #CALCULATE NUMBER OF CHILDREN UTILITIES BY MARKET SEGMENT 
          #@return khiazAry - child probability array (hiak dimensions)
          #/    

          #First define all combinations of household size and age of head. 
          #Define a function to calculate the utility of a household having a certain number of children.
          #Apply the function, using the kidCoeffMtx to the combinations of household size and age of head classes. 
          #Adjust utilities where there are illogical combinations such as a  A HH with size of 2 cannot have 2 or 3 kids, a HH of size 3 cannot have 3 kids, and a HH size of 1 cannot have 1, 2 or 3 kids (old is <1, <2, <3)
          #Calculate the probabilityof a household size and age of head combination having 0, 1, 2, or 3 children. 
          #Create an array of probabilities that matches the input existing market segments (hia).
          
          ##/
          #NEEDED FUNCTIONS
          ##/
          
          #Define Worker Utility Function
          calcUtilityKid <- function(sizeVariables, utilityRow) {
                              exp( utilityRow["hhsize"]*sizeVariables["h"] + utilityRow["age"]*sizeVariables["a"] )
                            }    
        
          ##/
          #LOAD INPUTS
          ##/
          
          load("pregen/whiazAry.RData")

          ##/
          #DEFINE ALL COMBINATIONS OF HOUSEHOLD SIZE
          ##/
                              
          #Define h and a class variables for utility functions
          hClasses <- as.numeric(gsub("h","",segmentsH))
          aClasses <- as.numeric(gsub("a","",segmentsA))
          
          #Create all combinations of h1-4 and a1-4
          classesDf <- expand.grid(hClasses,aClasses)
          classesDf <- classesDf[order(classesDf[,1],classesDf[,2]),]
          colnames(classesDf) <- c("h","a")
      
          ##/
          #CALCULATE NUMBER OF KIDS UTILITIES
          ##/
      
          #Apply the function, using the kidCoeffMtx to the combinations 
          utilitiesK0 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, kidCoeffMtx[1,])))
          utilitiesK1 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, kidCoeffMtx[2,])))
          utilitiesK2 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, kidCoeffMtx[3,])))
          utilitiesK3 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, kidCoeffMtx[4,])))
      
          #Adjust utilities where there are illogical combinations such as:
          # A HH with size of 2 cannot have 2 or 3 kids;
          # A HH of size 3 cannot have 3 kids;
          # A HH size of 1 cannot have 1, 2 or 3 kids (old is <1, <2, <3)
          utilitiesK1[classesDf[,"h"] < 2] <- 0
          utilitiesK2[classesDf[,"h"] < 3] <- 0
          utilitiesK3[classesDf[,"h"] < 4] <- 0
      
          ##/
          #GET PROBABILITIES
          ##/
          
          #Create table of all utilities and utility sums by HIA class
          utilitiesAllDf <- cbind(classesDf, utilitiesK0, utilitiesK1, utilitiesK2, utilitiesK3)
          utilitiesAllDf$utilitiesSum <- apply(utilitiesAllDf[,3:6], 1, sum)
      
          #Calculate Number of Children Probabilities by HIA class
          probAllDf <- apply(utilitiesAllDf[,3:6], 2, function(x) x/utilitiesAllDf$utilitiesSum)
          colnames(probAllDf) <- c("probK0","probK1","probK2","probK3")
          utilitiesAllDf <- cbind(utilitiesAllDf, probAllDf)
          rm(probAllDf)
      
          #Create Child Probability Array and 
          probAry <- array(unlist(utilitiesAllDf[,8:11]), c(numA, numH, numK, numI))
          #Transpose to hiak
          probAry <- aperm(probAry, c(2,4,1,3))
          dimnames(probAry) <- list(segmentsH, segmentsI, segmentsA, segmentsK)
          
          ##/
          #CREATE HIAWZARY 
          ##/
          
          #Collapse on workers to 4-D array
          hiazAry <- apply(whiazAry, c(2,3,4,5), sum)
      
          #Create a 5D array to hold the h,i,a,k,z data
          hiakzAry <- array(0, c(numH, numI, numA, numK, numZones))
          
          #Calculate Number of HHs by Number of Children by HIA class by zone
          #For each zone rep hia 4 times and multiply by 4 children class probabilities
          for(i in zones) hiakzAry[,,,,i] <- array(hiazAry[,,,i], c(dim(hiazAry)[1:3], numK))*probAry
         
          #Name the 5 dimensional array (h,i,a,k,zone), summarize and return array
          dimnames(hiakzAry) <- list(segmentsH, segmentsI, segmentsA, segmentsK, zoneNames)
          khiazAry <- aperm(hiakzAry, c(4,1,2,3,5))
          names(dimnames(khiazAry)) <- c("kid","hhSize","hhIncome","ageOfHead","zone")
          print(round(apply(khiazAry, "kid",sum),1))
          
          ##/
          #SAVE RESULTS
          ##/
          
          save(khiazAry, file="pregen/khiazAry.RData")
    }

########################################## END #################################################################