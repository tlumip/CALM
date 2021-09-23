  #/
    #@filename inputsSave_tazData.R
    #@author Martin Mann, martin.a.mann@odot.state.or.us
    #@date 11/10/14
    #@Description:
    #   runs the pregen submodel scripts whia, chia, khia, and vistors 
    #   Returns the results to calling function 

    pregen <- function(curSynPopData){

            ## INPUTS ##############################################################################
            
            curHiazAry <- curSynPopData[["hiazAry"]]
            curMixthm <- curSynPopData[["mixthm"]]
            curTot30t <- curSynPopData[["tot30t"]]
            curPercentSingleFamily <- curSynPopData[["percentSingleFamily"]]
            curWorkerCoeffMtx <- curSynPopData[["workerCoeffMtx"]]
            curCarCoeffMtx <- curSynPopData[["carCoeffMtx"]]
            curKidCoeffMtx <- curSynPopData[["kidCoeffMtx"]]
            
            ## WORKER SUBMODEL #######################################################################

            #Calculate Number of Workers Utilities By Market Segment
            hClasses <- as.numeric(gsub("h","",segmentsH))
            iClasses <- as.numeric(gsub("i","",segmentsI))
            aClasses <- as.numeric(gsub("a","",segmentsA))

            #Create all combinations of h1-4, i1-4 and a1-4
            classesDf <- expand.grid(hClasses,iClasses,aClasses)
            classesDf <- classesDf[order(classesDf[,1],classesDf[,2],classesDf[,3]),]
            colnames(classesDf) <- c("h","i","a")

            #Define Worker Utility Function
            calcUtilityWorker <- function(sizeVariables, utilityRow) {
                      exp( utilityRow["constant"] + utilityRow["hhsize"]*sizeVariables["h"] +
                      utilityRow[paste("inc", sizeVariables["i"], sep="")] +
                      utilityRow[paste("age", sizeVariables["a"], sep="")])
            }

            #Calculate Number of Workers Utilities
            utilitiesW0 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, curWorkerCoeffMtx[1,])))
            utilitiesW1 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, curWorkerCoeffMtx[2,])))
            utilitiesW2 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, curWorkerCoeffMtx[3,])))
            utilitiesW3 <- unlist(apply(classesDf, 1, function(x) calcUtilityWorker(x, curWorkerCoeffMtx[4,])))

            #Adjust utilities since a HH with size of 1 cannot have 2 or 3 workers and a HH of size 2 cannot have 3 workers
            utilitiesW2[classesDf[,"h"] < 2] <- 0
            utilitiesW3[classesDf[,"h"] < 3] <- 0

            #Create table of all utilities and calculate utility sums by HIA class
            utilitiesAllDf <- cbind(classesDf,utilitiesW0,utilitiesW1,utilitiesW2,utilitiesW3)
            utilitiesAllDf$utilitiesSum <- apply(utilitiesAllDf[,4:7], 1, sum)

            #Calculate Number of By Worker Probabilities by HIA class
            probAllDf <- apply(utilitiesAllDf[,4:7], 2, function(x) x/utilitiesAllDf$utilitiesSum)
            colnames(probAllDf) <- c("probW0","probW1","probW2","probW3")
            utilitiesAllDf <- cbind(utilitiesAllDf, probAllDf)
            rm(probAllDf)

            #Create Worker Probability Array and transpose to hiaw
            probAry <- array(unlist(utilitiesAllDf[,9:12]), c(numA, numI, numH, numW))
            probAry <- aperm(probAry, c(3,2,1,4))
            dimnames(probAry) <- list(segmentsH, segmentsI, segmentsA, segmentsW)

            #Calculate Number Of Households By Number Of Workers by HIA class
            hiawzAry <- array(0, c(numH, numI, numA, numW, numZones))
            for(i in zones) {
                hiawzAry[,,,,i] <- array(curHiazAry[,,,i], c(dim(curHiazAry)[1:3], numW)) * probAry
            }
            dimnames(hiawzAry) <- list(segmentsH, segmentsI, segmentsA, segmentsW, zoneNames)
            whiazAry <- aperm(hiawzAry, c(4,1,2,3,5))
            names(dimnames(whiazAry)) <- c("worker","hhSize","hhIncome","ageOfHead","zone")

            #Report Number of Households by Number of Workers
            print(summary(whiazAry, "worker"))

            ## VECHICLE SUBMODEL #########################################################################

            #Calculate Car Ownership Utilities By Market Segment

            #Define h, w, and i class variables for utility functions
            hClasses <- as.numeric(gsub("h","",segmentsH))
            wClasses <- as.numeric(gsub("w","",segmentsW))
            iClasses <- as.numeric(gsub("i","",segmentsI))
            #Create all combinations of h1-4, w1-4, and i1-4
            classesDf <- expand.grid(hClasses,wClasses,iClasses)
            classesDf <- classesDf[order(classesDf[,1],classesDf[,2],classesDf[,3]),]
            colnames(classesDf) <- c("h","w","i")

            #Define non-zonal component of Car Ownership Utility Function
            calcUtilityCarOwner <- function(sizeVariables, utilityRow) {
                        utilityRow["constant"] +
                        utilityRow[paste("hh", sizeVariables["h"], sep="")]  +
                        utilityRow[paste("wkr", sizeVariables["w"], sep="")] +
                        utilityRow[paste("inc", sizeVariables["i"], sep="")]
            }

            #Calculate Car Ownership Utilities
            utilitiesC1 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, curCarCoeffMtx[1,])))
            utilitiesC2 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, curCarCoeffMtx[2,])))
            utilitiesC3 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, curCarCoeffMtx[3,])))
            utilitiesC4 <- unlist(apply(classesDf, 1, function(x) calcUtilityCarOwner(x, curCarCoeffMtx[4,])))

            #Convert Utility Values to a 5D array of I,W,H,C,Zone
            #and repeat utilities numZones times
            utilitiesAry <- array(c(utilitiesC1, utilitiesC2, utilitiesC3, utilitiesC4),
                c(numI, numW, numH, numC, numZones))
            #Convert Utilities Array to 6D array by repeating
            #it numA times so it has the age of head dimension
            utilitiesAry <- array(utilitiesAry, c(numI, numW, numH, numC, numZones, numA))
            utilitiesAry <- aperm(utilitiesAry, c(2,3,1,6,5,4))  #Order is now WHIAZC
            dimnames(utilitiesAry) <- list(segmentsW, segmentsH, segmentsI, segmentsA, zones, segmentsC)

            ##/
            #Add zone-specific variables to utilities
            #Calculate the mixtot and tot30t portions of utility by zone.

            #Calculate Mixtot and Tot30t Part of Utility By Zone)
            mixtotMtx <- t(sapply(curMixthm, function(x) log(x)*curCarCoeffMtx[,"mixtot"]))
            tot30tMtx <- t(sapply(curTot30t, function(x) (x/1000)*curCarCoeffMtx[,"tot30t"]))
            zoneSpecificUtilDf <- mixtotMtx + tot30tMtx #Dimensions are zone by car ownership class

            #Add zone specific portion of utility to each zone utility
            utilitiesAry <- sweep(utilitiesAry, c(5,6), zoneSpecificUtilDf, "+")

            #Duplicate utitlitiesAry for single family dwellings by zone
            utilitiesSfAry <- sweep(utilitiesAry, 6, carCoeffMtx[,"sfdwell"], "+")

            #Change utility from utility to e^utility
            utilitiesAry <- exp(utilitiesAry)
            utilitiesSfAry <- exp(utilitiesSfAry)

            #Calculate Number of Households By Car Ownership Market Segment
            utilitiesSumAry <- apply(utilitiesAry, c(1,2,3,4,5), sum) #sum non-single fam by mrkt segment
            probC1Ary <- utilitiesAry[,,,,,"c1"]/utilitiesSumAry
            probC2Ary <- utilitiesAry[,,,,,"c2"]/utilitiesSumAry
            probC3Ary <- utilitiesAry[,,,,,"c3"]/utilitiesSumAry
            probC4Ary <- utilitiesAry[,,,,,"c4"]/utilitiesSumAry

            utilitiesSfSumAry <- apply(utilitiesSfAry, c(1,2,3,4,5), sum) #sum single fam by mrkt segment
            probSfC1Ary <- utilitiesSfAry[,,,,,"c1"]/utilitiesSfSumAry
            probSfC2Ary <- utilitiesSfAry[,,,,,"c2"]/utilitiesSfSumAry
            probSfC3Ary <- utilitiesSfAry[,,,,,"c3"]/utilitiesSfSumAry
            probSfC4Ary <- utilitiesSfAry[,,,,,"c4"]/utilitiesSfSumAry

            #Multiply whiaz array by the percent single family array and then apply these two arrays to the two probability arrays families (probSf and prob)

            whiazSfAry <- sweep(whiazAry, 5, curPercentSingleFamily, "*")   #sweep across zones
            whiazNsfAry <- sweep(whiazAry, 5, (1 - curPercentSingleFamily), "*") #sweep across zones

            #Calculate Number of HHs by whia by Car Ownership class by dwelling type
            whiaC1Ary <- whiazNsfAry * probC1Ary
            whiaC2Ary <- whiazNsfAry * probC2Ary
            whiaC3Ary <- whiazNsfAry * probC3Ary
            whiaC4Ary <- whiazNsfAry * probC4Ary

            whiaSfC1Ary <- whiazSfAry * probSfC1Ary
            whiaSfC2Ary <- whiazSfAry * probSfC2Ary
            whiaSfC3Ary <- whiazSfAry * probSfC3Ary
            whiaSfC4Ary <- whiazSfAry * probSfC4Ary

            #Final Arrays of number of HHs by car ownership by whia by zone
            whiaC1Ary <- whiaSfC1Ary + whiaC1Ary
            whiaC2Ary <- whiaSfC2Ary + whiaC2Ary
            whiaC3Ary <- whiaSfC3Ary + whiaC3Ary
            whiaC4Ary <- whiaSfC4Ary + whiaC4Ary

            #Build whiazc array
            whiazcAry <- array(c(whiaC1Ary, whiaC2Ary, whiaC3Ary, whiaC4Ary),
                c(numW, numH, numI, numA, numZones, numC))
            dimnames(whiazcAry) <- list(segmentsW, segmentsH, segmentsI, segmentsA, zoneNames, segmentsC)
            names(dimnames(whiazcAry)) <- c("worker","hhSize","hhIncome","ageOfHead","zone","car")
            #Summary
            print(summary(whiazcAry,"car"))

            ##/
            #Build cval Index Array
            cvalIndexAry <- array(NA, c(numW, numH, numI, numA, numC))
            dimnames(cvalIndexAry) <- dimnames(whiazcAry)[names(dimnames(whiazcAry)) != "zone"]
            #Dimensions are w, h, i, a, c (cars)

            #CVAL 0
            cvalIndexAry[,,,,"c1"] <- 0

            #CVAL 1
            cvalIndexAry["w3",,,,"c2"] <- 1
            cvalIndexAry["w4",,,,"c2"] <- 1
            cvalIndexAry["w4",,,,"c3"] <- 1

            #CVAL 2
            cvalIndexAry["w2",,,,"c2"] <- 2
            cvalIndexAry["w3",,,,"c3"] <- 2
            cvalIndexAry["w4",,,,"c4"] <- 2

            #CVAL 3
            cvalIndexAry["w1",,,,"c2"] <- 3
            cvalIndexAry["w1",,,,"c3"] <- 3
            cvalIndexAry["w2",,,,"c3"] <- 3
            cvalIndexAry["w1",,,,"c4"] <- 3
            cvalIndexAry["w2",,,,"c4"] <- 3
            cvalIndexAry["w3",,,,"c4"] <- 3

            #Save cval without zone dimension and replicated for each zone
            cvalAry <- cvalIndexAry
            cvalIndexAry <- array(rep(cvalIndexAry, numZones),
                c(numW, numH, numI, numA, numC, numZones))
            cvalIndexAry <- aperm(cvalIndexAry, c(1,2,3,4,6,5))
            dimnames(cvalIndexAry) <- dimnames(whiazcAry)

            #Print Out Results by cval
            print(c(tapply(whiazcAry, cvalIndexAry, sum), total=sum(whiazcAry)))

            ## CHILDREN SUBMODEL #######################################################################


            #Calculate Number of Children Utilities By Market Segment
               
            #Define h and a class variables for utility functions
            hClasses <- as.numeric(gsub("h","",segmentsH))
            aClasses <- as.numeric(gsub("a","",segmentsA))
            #Create all combinations of h1-4 and a1-4
            classesDf <- expand.grid(hClasses,aClasses)
            classesDf <- classesDf[order(classesDf[,1],classesDf[,2]),]
            colnames(classesDf) <- c("h","a")
        
            #Define Kid Utility Function
            calcUtilityKid <- function(sizeVariables, utilityRow) {
                   exp( utilityRow["hhsize"]*sizeVariables["h"] + 
                    utilityRow["age"]*sizeVariables["a"] )
            }
        
            #Calculate Number of Kids Utilities
            utilitiesK0 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, curKidCoeffMtx[1,])))
            utilitiesK1 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, curKidCoeffMtx[2,])))
            utilitiesK2 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, curKidCoeffMtx[3,])))
            utilitiesK3 <- unlist(apply(classesDf, 1, function(x) calcUtilityKid(x, curKidCoeffMtx[4,])))
        
            #Adjust utilities since a HH with size of 2 cannot have 2 or 3 kids
            #a HH of size 3 cannot have 3 kids, and a HH size of 1 cannot have 1, 2 or 3 kids
            #CORRECTED ADJUSTMENTS 10/14/03 (different from Dora model) (old is <1, <2, <3)
            utilitiesK1[classesDf[,"h"] < 2] <- 0
            utilitiesK2[classesDf[,"h"] < 3] <- 0
            utilitiesK3[classesDf[,"h"] < 4] <- 0
        
            #Create table of all utilities and utility sums by HIA class
            utilitiesAllDf <- cbind(classesDf, utilitiesK0, utilitiesK1, utilitiesK2, utilitiesK3)
            utilitiesAllDf$utilitiesSum <- apply(utilitiesAllDf[,3:6], 1, sum)
        
            #Calculate Number of Children Probabilities by HIA class
            probAllDf <- apply(utilitiesAllDf[,3:6], 2, function(x) x/utilitiesAllDf$utilitiesSum)
            colnames(probAllDf) <- c("probK0","probK1","probK2","probK3")
            utilitiesAllDf <- cbind(utilitiesAllDf, probAllDf)
            rm(probAllDf)
        
            #Create Child Probability Array and transpose to hiak
            probAry <- array(unlist(utilitiesAllDf[,8:11]), c(numA, numH, numK, numI))
            probAry <- aperm(probAry, c(2,4,1,3))
            dimnames(probAry) <- list(segmentsH, segmentsI, segmentsA, segmentsK)
        
            #Create a 5D array to hold the h,i,a,k,z data
            hiakzAry <- array(0, c(numH, numI, numA, numK, numZones))
            #Calculate Number of HHs by Number of Children by HIA class by zone
            #For each zone rep hia 4 times and multiply by 4 children class probabilities
            for(i in zones) {
                hiakzAry[,,,,i] <- array(curHiazAry[,,,i], c(dim(curHiazAry)[1:3], numK))*probAry
            }
            
            #Name the 5 dimensional array (h,i,a,k,zone), summarize and return array
            dimnames(hiakzAry) <- list(segmentsH, segmentsI, segmentsA, segmentsK, zoneNames)
            khiazAry <- aperm(hiakzAry, c(4,1,2,3,5))
            names(dimnames(khiazAry)) <- c("kid","hhSize","hhIncome","ageOfHead","zone")
            print(summary(khiazAry,"kid"))
            
            ## VISTORS SUBMODEL #######################################################################
        
            #Load the lodging data
        
              if(!file.exists("inputs/lodging.csv")) {
                       stop ("No lodging file found. No visitor households will be computed.")}
               
              lodgings <- read.csv("inputs/lodging.csv")
              names(lodgings) <- tolower(names(lodgings))
        
              #Convert rooms into visitor households
        
              # Initialize a vector to hold the results
              visitorHh.Za <- numeric(length(zoneNames))
              names(visitorHh.Za) <- zoneNames
              # Calculate total number of visitor households per taz
              visitorHh <- tapply(vacancyRate * lodgings[,"rooms"], lodgings[,"taz"], sum)
              # Place the values for the tazs with visitor households into the vector
              visitorHh.Za[names(visitorHh)] <- visitorHh
              # Sum over the zone dimension
              visWhiaAry <- apply(whiazAry, c(1,2,3,4), sum)
              # Remove low income households
              visWhiaAry[,,"i1",] <- 0
              # Calculate proportions and apply to visitor households
              whiaProp <- visWhiaAry /sum(visWhiaAry)
              visitorWhiazAry <- outer(whiaProp, visitorHh.Za, "*")
        
              #Add the car ownership dimension to the visitor array
        
              # Initialize a visitorWhiazcAry and populate by duplicating visitorWhiazAry values
              visitorWhiazcAry <- outer(visitorWhiazAry, c(1,1,1,1), "*")
              dimnames(visitorWhiazcAry)[[5]] <- zoneNames
              dimnames(visitorWhiazcAry)[[6]] <- segmentsC
              # It is assumed that all visitors have cars, so set the c1 part of the array to 0
              visitorWhiazcAry[,,,,,"c1"] <- 0
              # Visitor households are in the c2 category (cars less than workers) if they are
              # in the w3 (2 worker) or w4 (3+ worker) categories. Therefore there must be 0
              # in the combination of w1, w2 and c2.
              visitorWhiazcAry[c("w1","w2"),,,,,"c2"] <- 0
              # Households are in the c3 category (cars equal workers) if they are in the
              # w2 category (1 worker). Therefore there must be 0 in the combination of
              # w1, w3, w4 and c3.
              visitorWhiazcAry[c("w1","w3","w4"),,,,,"c3"] <- 0
              # Households are in the c4 category (cars greater than workers) if they are
              # in the w1 category (0 worker). Therefore there must be zero values in the
              # combination of w2, w3, w4 and c4.
              visitorWhiazcAry[c("w2","w3","w4"),,,,,"c4"] <- 0
        
        
              ## OUTPUTS ############################################################################
              
              return(list("whiazAry"=whiazAry,"whiazcAry"=whiazcAry,"khiazAry"=khiazAry,"visitorWhiazcAry"=visitorWhiazcAry,"cvalAry"=cvalAry,"cvalIndexAry"=cvalIndexAry))
            }
########################################################## END #################################################################