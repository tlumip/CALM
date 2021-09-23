#===================
#visitors.103105v1.r
#===================

#:Author: Brian Gregor
#:Contact: brian.j.gregor@odot.state.or.us
#:Date: 10/31/05
#:Copyright: 2005, Oregon Department of Transportation
#:License: GPL2

#Description
#===========
#This script defines a function which, when executed, produces an array of visitor households by number of workers, household size, income, age of household head, zone and car ownership. 
#The resulting array is added to the resident household array in trip generation for HBS, HBO, HBR and NHBNW trips.

      fun$visitors <- function() {

            ##/
            #CALCULATE NUMBER OF WORKERS UTILITIES BY MARKET SEGMENT
            #It is assumed that every visitor household has one car and only one car.      
            #@return visitorWhiazcAry - Array of cross tabulations of workers, household size, age of householder,by zones for visitor householdd            
            ##/
                    
            ##/
            #LOAD INPUTS
            ##/

            if(!file.exists("inputs/lodging.csv")) stop ("No lodging file found. No visitor households will be computed.")       
            lodgings <- read.csv("inputs/lodging.csv")
            names(lodgings) <- tolower(names(lodgings))
            load("pregen/whiazAry.RData")
             
            ##/
            #CONVERT ROOMS INTO VISITOR HOUSEHOLDS  
            #Rooms are converted into visitor households by zone by multiplying the number of rooms by the vacancy rate and summing the results by zone. 
            #The households are proportioned into worker, hhsize, income and age using the proportions of the resident population with the exception of low income households.
            ##/
            
            #Calculate total number of visitor households per taz
            visitorHh.Za <- numeric(length(zoneNames))
            names(visitorHh.Za) <- zoneNames        
            visitorHh <- tapply(vacancyRate * lodgings[,"rooms"], lodgings[,"taz"], sum)
            visitorHh.Za[names(visitorHh)] <- visitorHh            
            
            #Calculate proportions and apply to visitor households
            whiaAry <- apply(whiazAry, c(1,2,3,4), sum)
            whiaAry[,,"i1",] <- 0            
            whiaProp <- whiaAry /sum(whiaAry)
            visitorWhiazAry <- outer(whiaProp, visitorHh.Za, "*")

            ##/
            #ADD THE CAR OWNERSHIP DIMENSION TO THE VISITOR ARRAY
            #The car ownership attribute is a direct function of the number of workers in the visitor household. 
            #The array is created by extending the visitorWhiazAry into an additional dimension by doing an outer product with a vector of ones. 
            ##/

            visitorWhiazcAry <- outer(visitorWhiazAry, c(1,1,1,1), "*")
            dimnames(visitorWhiazcAry)[[5]] <- zoneNames
            dimnames(visitorWhiazcAry)[[6]] <- segmentsC
            
            ##/
            #ADJUST ARRAYS                    
            #Ajustment based on logic that all visitors have cars 
            #Car 1: c1 part of the array to 0
            #Car 2: c2 part of the array to 0 in the combination of w1, w2 and c2 (Cars less than workers).
            #Car 3: c3 part of the array to 0 in the combination of w1, w3, w4 and c3 (Cars equal workers).
            #Car 4: c4 part of the array to 0 in the combination of w2, w3, w4 and c4 (Cars greater than workers).            
            ##/
            
            visitorWhiazcAry[,,,,,"c1"] <- 0
            visitorWhiazcAry[c("w1","w2"),,,,,"c2"] <- 0
            visitorWhiazcAry[c("w1","w3","w4"),,,,,"c3"] <- 0
            visitorWhiazcAry[c("w2","w3","w4"),,,,,"c4"] <- 0

            ##/
            #SAVE RESULTS
            ##/
            
            save(visitorWhiazcAry, file="pregen/visitorWhiazcAry.RData")
      
      }
      
########################################## END #################################################################      
     
           
