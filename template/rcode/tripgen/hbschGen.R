#/
#@filename hbschGen.R
#@author Ben Stabler
#@version 1.2
#@date 8/5/04
#
#hbsch (school) trip productions
#Revised 10/14/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revised 1/15/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 8/5/04 benjamin.stabler@odot.state.or.us
# BTS 03/14/12 - Refer to new scaleHbschAttractionsToProductions setting in settings.csv
#Revised  01/16/15 - removed scaling productions as this is no longer used in current distribution script 
#See page 108 of the JEMnR User's Guide for a description.
#Revised 02/09/15 Martin Mann
#   Complete overhaul of trip production adjustment.  School trips calculated by the generation rates and kidsPerHH array are adjusted by each school to ensure a constant
#   Trips_Per_Student_Enrolled is maintained for each school. The enrollment is kept in the districts.csv file as "elemEnrl","middleEnrl","highEnrl".
#/

      cat("Home-Based School Trip Generation\n\n")
    
      fun$hbschGen <- function() {
         
          #/
          #LOAD INPUTS
          #/
          
          load("pregen/khiazAry.RData")
          load("inputs/RData/districts.RData")

          ##/
          #HBSCH TRIP GENERATION
          #HBSCH trips are a function of the kids in household and hhsize.
          #@param khiazAry - households by number of children array
          #@param hbschGenRatesMtx - hbsch trip generation rates
          #@return hbschTripProdAry - hbsch trip productions (also saved)
          #@return hbschTripProdDF - Adjusted hbsch trip productions
          ##/
  
          #Sweep the hbsch trip production matrix across the num kids and hhsize dimensions of the array
          hbschTripProdAry <- sweep(khiazAry, c(1,2), as.matrix(hbschGenRatesMtx), "*")
          
          #Calculate Trips per zone
          tripsPerZone <- apply(hbschTripProdAry,c("zone"),sum)

          #Find enrollment by grade
          EnrolByGrade <- colSums(districts[,c("elemEnrl","middleEnrl","highEnrl")])
          pctEnrolbyGrade <- EnrolByGrade / sum(EnrolByGrade)
    
          #Loop through each grade and adjust trips contributing to each school to a constant trip per student ratio
          hbschTripProdLst <- lapply(c("elemEnrl","middleEnrl","highEnrl"),function(curGrd) { 
                                    curSchZones <- districts[districts[,curGrd] > 0,"zone"] 
                                    pctEnrollperSch <- districts[,curGrd] /EnrolByGrade[curGrd]
                                    names(pctEnrollperSch) <- districts$zone
                                    curGrdTrips <- tripsPerZone * pctEnrolbyGrade[curGrd]
                                    adjSchTrips <-  sum(curGrdTrips) * pctEnrollperSch
                                    adjTrips <- unlist(lapply(curSchZones, function(x) {
                                                    curTrips <-  curGrdTrips[districts[,gsub("Enrl","",curGrd)]%in%(x)]
                                                    sum(adjSchTrips[as.character(x)]) * (curTrips / sum(curTrips))
                                                }))
                                    adjTrips <- adjTrips[as.character(districts$zone)]
                                    adjTrips
                               })
          
          ##/      
          #PRINT OUT SUMMARY RESULTS
          ##/ 
          
          cat(paste("Total Kid HH:",round(sum(khiazAry),0),"\n"))  

          curTripDF <- as.data.frame( lapply(pctEnrolbyGrade,function(x) x * tripsPerZone))
          colnames(curTripDF) <- c("elem", "middle", "high") 
          
          tripsPerStd <-lapply(colnames(curTripDF), function(x) {
                                curTrips <- tapply(curTripDF[,x],districts[,x],sum)
                                curTrips <- curTrips[!names(curTrips)%in%as.character(externalZones)]
                                curEnrol <-   districts[,paste(x,"Enrl",sep="")]
                                names(curEnrol) <- districts$zone
                                curEnrol <- curEnrol[curEnrol>0]
                                unique(round(curTrips / curEnrol[names(curTrips)],2))
                        })
          names(tripsPerStd) <-  c("elem","middle","high") 
          cat("Unique Current Trips per grade:",sep="\n") 
          print(tripsPerStd) 
                                                                                                      
          names(hbschTripProdLst) <- c("elem","middle","high")  
          hbschTripProdDF <- round(as.data.frame(hbschTripProdLst),3)  
          hbschTripProdDF[is.na(hbschTripProdDF)] <- 0
          rownames(hbschTripProdDF) <- zoneNames
          tripsPerStd <- unlist(lapply(colnames(hbschTripProdDF), function(x) {
                                curTrips <- tapply(hbschTripProdDF[,x],districts[,x],sum)
                                curTrips <- curTrips[!names(curTrips)%in%as.character(externalZones)]
                                curEnrol <-   districts[,paste(x,"Enrl",sep="")]
                                names(curEnrol) <- districts$zone
                                curEnrol <- curEnrol[curEnrol>0]
                                unique(round(curTrips / curEnrol[names(curTrips)],2))
                        }))
          names(tripsPerStd) <-  c("elem","middle","high") 
          cat("Unique Adjusted Trips per grade:",sep="\n") 
          print(tripsPerStd) 
       
             
          ##/      
          #SAVE RESULTS
          ##/
          save(hbschTripProdDF, file="tripgen/hbschTripProdDF.RData")
          save(hbschTripProdAry, file= "tripgen/hbschTripProdAry.RData")    
      }   
    
########################################## END #################################################################

	
		

