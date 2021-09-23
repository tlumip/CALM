#/
#@filename hbsroGen.103105v1.R
#@author Ben Stabler
#@date 8/12/04
#@version 1.2
#
#hbs (shop), hbr (recreation), and hbo (other) trip generation
#Revised 10/14/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 1/15/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 8/12/04 benjamin.stabler@odot.state.or.us - pulled out calibration factor
#Revised 10/31/05 brian.j.gregor@odot.state.or.us - loads in visitorWhiazcAry if it exists and adds to the resident population array
#Revised 10/17/14 Martin Mann - Added lines to accommodate using TripProdCalibrationFactors by district
#Revised 01/11/16 Martin Mann - Removed lines to accommodate using TripProdCalibrationFactors by district
#                               Added lines to boost productions from settings file factor    
#See page 104 of the JEMnR User's Guide for a description.
#/

    cat("Home-Based Shop, Rec, and Other Trip Generation\n\n")
    
    fun$hbsroGen <- function(purpose) {
    
        cat(paste(purpose, " trip generation\n\n", sep=""))
                  
        #/
        #LOAD INPUTS
        #/
  
        load("pregen/whiazcAry.RData")      
        if(file.exists("pregen/visitorWhiazcAry.RData")) load("pregen/visitorWhiazcAry.RData")
        
        #/
        #HBR, HBS, HBO TRIP GENERATION
        #HBW productions are a function of the number of workers and hhsize.
        #Boost HBO,HBS, and HBR trip productions by district (ga field in districts file) using a trip boosting factor.
        #If there is a visitor model, these trips are added to the current trip production
        #@param tripBoost - hbsro trip gen calibration factor from a table by district
        #@param <purpose>GenRatesMtx - <purpose> generation rates
        #@param whiazcAry - whiazc household array
        #@return <purpose>TripProdAry - purpose specific trip productions array
        ##/            

        #CALCULATE PRODUCTIONS  
        #Apply generation rates to whiazcAry
        genRatesMtx <- get(paste(purpose, "GenRatesMtx", sep=""))             
        
        #Load visitorWhiazcAry and add to whiazcAry
        if(file.exists("pregen/visitorWhiazcAry.RData")) whiazcAry <- whiazcAry + visitorWhiazcAry * hbsroVisitorCalibrationFactor
        tripProdAry <- sweep(whiazcAry, c(1,2), as.matrix(genRatesMtx), "*")
        
        #BOOST PRODUCTIONS        
        tripProdAry <- tripProdAry * hbsroTripProdCalibrationFactor
        
        ##/      
        #PRINT OUT SUMMARY RESULTS
        ##/
        
        print(summary(tripProdAry,"hhIncome"))
        load("pregen/cvalIndexAry.RData")
        print(c(tapply(tripProdAry, cvalIndexAry, sum), total=sum(tripProdAry)))
        
        ##/      
        #SAVE RESULTS
        ##/
        
        assign(paste(purpose, "TripProdAry", sep=""), tripProdAry)
        save(list=paste(purpose, "TripProdAry", sep=""), file=paste("tripgen/", purpose, "TripProdAry.RData", sep=""))
    }   
########################################## END #################################################################  