#/
#@filename nhbGen.110105v1.R
#@author Ben Stabler
#@version 1.2
#@date 8/12/04
#
#nhb (work and non-work) trip productions
#Revised 10/14/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 8/12/04 benjamin.stabler@odot.state.or.us - pulled out calibration factors
#Revised 10/31/05 brian.j.gregor@odot.state.or.us - loads in visitorWhiazcAry if it exists and adds to the resident population array
#Revised 11/01/05 brian.j.gregor@odot.state.or.us - scales nhbw trip productions to employment only if the nhbwTotalEmploymentCalibrationFactor is not NULL
#Revised 10/17/14 Martin Mann - added lines to accommodate using TripProdCalibrationFactors by district
#Revised 01/11/16 Martin Mann - Removed lines to accommodate using TripProdCalibrationFactors by district for nhbnw
#                               Added lines to boost productions from settings file factort for nhbnw 
#See page 105 of the JEMnR User's Guide for a description.
#/

  cat("Non-Home-Based Work and Non-Work Trip Generation\n\n")

    fun$nhbGen <- function(purpose) {
      
        cat(paste(purpose, " trip generation\n\n", sep=""))
    
        #/
        #LOAD INPUTS
        #/
        
        #University Model inputs
        if("nhbw"%in%purpose & as.logical(runUniversityModel)) {
            curObjs <- loadListObj("inputs/SynPop_StdFacLst.RData",c("whiazcAry","visitorWhiazcAry","totalEmp"))
            for(x in names(curObjs)) assign(x,curObjs[[x]])
        }
        #Non University Model inputs
        if(!("nhbw"%in%purpose & as.logical(runUniversityModel))) {        
            load("pregen/whiazcAry.RData")
            load("inputs/RData/totalEmp.RData")
             if(file.exists("pregen/visitorWhiazcAry.RData")) load("pregen/visitorWhiazcAry.RData")  
        }
    
        #/
        #NHBNW TRIP GENERATION  
        #NHBNW productions are a function of the number of workers and hhsize.
        #Boost NHBNW trip productions by district (ga field in districts file) using a trip boosting factor.
        #If there is a visitor model, these trips are added to the current trip production
        #@param tripBoost - nhbnw trip gen calibration factor from a table by district
        #@param nhbnwGenRatesMtx - nhbnwgeneration rates
        #@param whiazcAry - whiazc household array
        #@return tripProdAry - purpose specific trip productions array
        ##/
                                    
        if(purpose == "nhbnw") {            
            
            #CALCULATE PRODUCTIONS
            #Apply generation rates to whiazcAry
            nhbnwGenRatesMtx <- get(paste(purpose, "GenRatesMtx", sep=""))
            #Load visitorWhiazcAry and add to whiazcAry
            if(file.exists("pregen/visitorWhiazcAry.RData")) whiazcAry <- whiazcAry + visitorWhiazcAry * nhbnwVisitorCalibrationFactor
            tripProdAry <- sweep(whiazcAry, c(1,2), as.matrix(nhbnwGenRatesMtx), "*")
            
            #BOOST PRODUCTIONS
            tripProdAry <- tripProdAry * nhbnwTripProdCalibrationFactor
        }       
        
        ##/
        #NHBW TRIP GENERATION 
        #NHBW productions are a function of the number of workers by zone
        #@param nhbwGenRatesMtx - hbw generation rates
        #@param whiazcAry - whiazc household array
        #@paramnhbwTotalEmploymentCalibrationFactor - This scales nhbw productions by zone so the 
        #total prods match a value of total employment that has been scaled 
        #@return tripProdAry - nhbw trip productions arrays
        ##/
                                   
        if(purpose == "nhbw") {                 
            
            #CALCULATE PRODUCTIONS
            #Sweep the trip production vector across the worker dimension of the whiazcAry array
            tripProdAry <- sweep(whiazcAry, 1, get(paste(purpose, "GenRatesMtx", sep=""))$rate, "*")
        
            #SCALE PRODUCTIONS
            #Note: hbw attractions would no longer neccesarily match productions 
            if(!is.null(nhbwTotalEmploymentCalibrationFactor)) {
                #Calculate factor for HBW gen based on total employment 
                nhbwFactor <- (sum(totalEmp) * nhbwTotalEmploymentCalibrationFactor)/ sum(tripProdAry)
                #Scale the hbw trip production array by the factor
                tripProdAry <- tripProdAry * nhbwFactor
            }
        }
        
        ##/      
        #PRINT OUT SUMMARY RESULTS
        ##/
        
        print(summary(tripProdAry, "worker"))
        load("pregen/cvalIndexAry.RData")
        print(c(tapply(tripProdAry, cvalIndexAry, sum), total=sum(tripProdAry)))
    
        ##/      
        #SAVE RESULTS
        ##/
        
        assign(paste(purpose, "TripProdAry", sep=""), tripProdAry)
        save(list=paste(purpose, "TripProdAry", sep=""), file=paste("tripgen/", purpose, "TripProdAry.RData", sep=""))

        ##/      
        #REMOVE UNIVERSITY OBJECTS IF REQUIRED
        ##/
                      
        if("nhbw"%in%purpose & as.logical(runUniversityModel)) rm(list=names(curObjs))
    }   
  
########################################## END #################################################################