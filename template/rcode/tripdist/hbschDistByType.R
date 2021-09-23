#/
#@filename hbschDistByType.R
#@author Ben Stabler
#@version 1.0
#@date 8/5/04
#
#Revised 02/09/15 Martin Mann
#   Modified to accomodate the new outputs from trip generation
#home based school distribution by school type
#/

  cat("Calculate hbsch trip distribution by school type\n\n")
  
  fun$hbschDistByType <- function() {
  
        #/
        #Load and preprocess inputs
        #/
        
        ##/
        #Distribute school trips by school type
        #Each zone must have a zone identified for elementary, middle and high school hbsch trips.  
        #These school zone schemes are input in the districts csv file as "elem", "middle", "high".  
        #School trips produced by trip generation are factored up to match total model area school enrollment if it exists
        #@param districts - district defintions ("elem" "middle" "high" district defintion used)       
        #Distribute school trips by school type within school sheds
        #School sheds created from public schools only
        #Distribute trips in PA form to zones
        #@param hbschTripProdAry - hbsch trips by zone from trip generation
        #@param elemPct - percent of total regional school enrollment that is elementary
        #@param middlePct - percent of total regional school enrollment that is middle
        #@param highPct - percent of total regional school enrollment that is high
        #@param totalEnroll - total regional school enrollment
        #@param elemhbschTripProdAry - elementary school trip productions
        #@param middlehbschTripProdAry - middle school trip productions
        #@param highhbschTripProdAry - high school trip productions
        #@param districts - district defintions
        #@return hbschDist - save hbsch trip distribution by school type     
        ##/
        
        #Load hbsch trip productions array
        load("inputs/RData/districts.RData")
        load("tripgen/hbschTripProdDF.RData")
    
        #Create emply School Distribution array
        hbschDist <- array(0, c(length(districts$zone), length(districts$zone), 3))
        dimnames(hbschDist) <- list(districts$zone, districts$zone, c("elem","middle","high"))     
        
        #Split productions to grade classes and Distribute school trips by school type within each school shed
        
        #ELEMENTARY
        elemTrips <- hbschTripProdDF[,"elem"]
        names(elemTrips)<- districts$zone
        for(i in 1:length(elemTrips)) hbschDist[names(elemTrips)[i], as.character(districts$elem)[i], 1] <- hbschDist[names(elemTrips)[i], as.character(districts$elem)[i], 1] + elemTrips[i]
        
        #MIDDLE
        middleTrips <- hbschTripProdDF[,"middle"]
        names(middleTrips)<- districts$zone
        for(i in 1:length(middleTrips)) hbschDist[names(middleTrips)[i], as.character(districts$middle)[i], 2] <- hbschDist[names(middleTrips)[i], as.character(districts$middle)[i], 2] + middleTrips[i]
        
        #HIGH
        highTrips <- hbschTripProdDF[,"high"]
        names(highTrips)<- districts$zone      
        for(i in 1:length(highTrips)) hbschDist[names(highTrips)[i], as.character(districts$high)[i], 3] <- hbschDist[names(highTrips)[i], as.character(districts$high)[i], 3] + highTrips[i]
         
        indx <- !districts$zone%in%externalZones
        distEnrol <- tapply(rowSums(districts[indx,c("elemEnrl","middleEnrl","highEnrl")]), districts[indx,"sch"],sum)
        distTrips <- tapply( apply(hbschDist,2,sum)[indx], districts[indx,"sch"],sum)
        cat(paste("TripPerStudentByDistrict","\n",sep=""))
        print(round((distTrips/distEnrol),2))
         
        #Save result
        save(hbschDist, file="tripdist/hbschDist.RData")
        rm(hbschDist)
  
    }

########################################## END #################################################################
