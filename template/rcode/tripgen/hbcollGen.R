#/
#@filename hbcollGen.R
#@author Ben Stabler
#@version 1.2
#@date 8/12/04
#
#home-based college trip productions
#College vehicles used directly as attractions for scaling to productions
#Revised 10/14/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 1/15/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 8/12/04 benjamin.stabler@odot.state.or.us - pulled out boosting factor
# BTS 03/14/12 - deleted code for reading in trip production calibration factor 
#Revised 10/17/14 - Martin Mann:
#  1_Added lines to accommodate using TripProdCalibrationFactors by district    
#  2_Modifed the code to balance productions to attractions rather than attractions to productions.  
#    Over half the trips were lost the other way of balancing
#Revised 12/31/14 - Martin Mann
#   Removed boosting of productions since that was undone by the balancing to attractions (colVeh)
#Revised 01/11/16 - Martin Mann
#   Set Boost to Attractions 


#See page 107 of the JEMnR User's Guide for a description.
#/
  
    cat("Home-Based College Trip Generation\n\n")
    
    fun$hbcollGen <- function() {
    
        #/
        #LOAD INPUTS
        #/
        
        load("pregen\\whiazcAry.RData")
        load("inputs\\RData\\colveh.RData") 
        
        ##/
        #HBCOLL TRIP GENERATION 
        #HBW productions are a function of the age of HH and hhsize.
        #Boost HBCOLL trip productions by district (ga field in districts file) using a trip boosting factor.
        #Scale productions to attractions and save results #Switched from attractions to Productions
        #@param hbcollTripProdCalibrationFactor - trip generation calibration factor
        #@param whiazcAry - wkr-hhs-age-inc-veh-zone Array
        #@param hbcollGenRatesMtx - hbcoll generation rates
        #@param colveh - college vehicle trips by zone
        #@return hbcollAttractions - hbcoll attractions vector
        #@return hbcollTripProdAry - hbcoll trip productions array
        ##/
        
        #CALCULATE PRODUCTIONS 
        hbcollGenRatesMtx <- hbcollGenRatesMtx 
        hbcollTripProdAry <- sweep(whiazcAry, c(2,4), as.matrix(hbcollGenRatesMtx), "*")  
             
        #CALCULATE ATTRACTIONS 
        hbcollAttractions <- colveh * hbcollTripCalibrationFactor
        names(hbcollAttractions) <- zoneNames    
        
        #SCALE PRODUCTIONS TO ATTRACTIONS
        hbcollProductions <- apply(hbcollTripProdAry, 5, sum)        
        hbcollTripProdAry <- hbcollTripProdAry *  sum(hbcollAttractions)/sum(hbcollProductions ) 
        
        ##/      
        #PRINT OUT SUMMARY RESULTS
        ##/
  
        print(summary(hbcollTripProdAry, "worker"))
        load("pregen\\cvalIndexAry.RData")
        print(c(tapply(hbcollTripProdAry, cvalIndexAry, sum), total=sum(hbcollTripProdAry)))
    
        ##/      
        #SAVE RESULTS
        ##/
        
        save(hbcollAttractions, file="tripgen\\hbcollAttractions.RData")
        save(hbcollTripProdAry, file="tripgen\\hbcollTripProdAry.RData")
    }   
    
########################################## END #################################################################