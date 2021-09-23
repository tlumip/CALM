#/
#@filename peaking.R
#@author Ben Stabler
#@version 1.1
#@date 8/12/04
#NOTES:
    #Calculate park and ride <mode class> trips by period
    ##Loops through assignment periods and purposes as specified as 
    #colnames in pandr<class>PeriodFactors.
    #First load the <mode class> vehicle and transit matrices for the purpose.
    #Then get the pa factor and ap factor for that purpose for the assignment
    #period. Multiply matrices by the factors and add the pa to the transpose 
    #of the ap.  Save the results to <period><class>Vehicle.RData and
    #<period><class>Transit.RData in the peaking folder.
    #
    #@param <class>PeriodFactors - transit leg <class> period factors
    #@param pandrVehiclePeriodFactors - drive leg mode period factors
    #@param modes - mode definitions
    #@param <class>Vehicle - <class> vehicle trips for <purpose>
    #@param <class>Transit - <class> transit trips for <purpose>
    #@return <period><class>Vehicle.RData - <class> vehicle trips to assign
    #@return <period><class>Transit.RData - <class> transit trips to assign
    ##/     

    fun$pandrPeaking <- function() {
          
          #/
          #CONVERT PA MATS TO OD MATS BY ASSIGNED CLASS'S, APPLY PERIOD FACTOR, COLLAPSE ON PURPOSE 
          #@return <period><class>.RData - <class> trips to assign
          #/
     
          cat("Create Peaking (Directional) and Assignment Period Specific Matrices\n\n")

          #Get classes, purposes, and TOD Directional Factors for classes
          tPurposes <- c(purposes, "hbsch")           
          indx <- modes$type%in%c("pandr")
          classes <- as.character(unique(modes[indx,"class"]))
          classes <- paste(classes,c("Vehicle","Transit"),sep="")
          #TOD_Directional Factors
          
          curClassFactLst <- lapply(c("pandrVehicle","pandrBus"),function(cClass) get(paste(cClass, "PeriodFactors", sep="")))
          names(curClassFactLst) <- classes
          
          #Make a a list by class and periods
          allTripsLst <-lapply(classes,function(x) {
                            temp <- rep(list(matrix(rep(0,length(zones)^2),ncol=length(zones),)),dim(curClassFactLst[[x]])[2])
                            names(temp) <- colnames(curClassFactLst[[x]])
                            temp}
                        )
          names(allTripsLst) <- classes 
         
         #Loops through purposes and sums the OD matrices by class to allTripsLst      
         for(purp in tPurposes){
              curPurpMatLst <- lapply(classes,function(cClass)  loadFile(paste("peaking/", purp, "/", cClass, ".RData", sep=""))[[1]])
              names(curPurpMatLst) <- classes               
              for(cClass in classes){
                   periods <- colnames(curClassFactLst[[cClass]])
                   paTripsLst <- lapply(periods,function(x) curPurpMatLst[[cClass]] * curClassFactLst[[cClass]][paste(purp, "pa", sep=""), x])
                   apTripsLst  <- lapply(periods,function(x) curPurpMatLst[[cClass]] * curClassFactLst[[cClass]][paste(purp, "ap", sep=""), x])
                   cTripsLst <- lapply(1:length(periods),function(x) {
                                    temp <- paTripsLst[[x]] + t(apTripsLst[[1]])
                                    if(all(dim(temp) == 1)) temp <- 0
                                    temp
                                 })
                   allTripsLst[[cClass]] <- lapply(1:length(periods),function(x) allTripsLst[[cClass]][[x]] + cTripsLst[[x]])
                   rm(paTripsLst,apTripsLst,cTripsLst)
              }
         }  
         
         #//
         #SAVE RESULTS
         #//
         subVec <- c("vehicle","bus")
         names(subVec) <- names(curClassFactLst)
         for(cClass in names(curClassFactLst)){
              periods <- colnames(curClassFactLst[[cClass]])           
              for(period in 1:length(periods)){                                 
                   
                   curPerClass <- paste(periods[period], cClass, sep="")
                   assign(curPerClass, allTripsLst[[cClass]][[period]])
                   save(list=curPerClass,file=paste("peaking/",curPerClass, ".RData", sep=""))
                   
                   curPerVeh <-  tolower(paste(periods[period],subVec[cClass],sep=""))
                   load(paste("peaking/",curPerVeh,".RData", sep=""))
                   assign(curPerVeh, get(curPerVeh) + get(curPerClass))
                   save(list=curPerVeh,file=paste("peaking/",curPerVeh, ".RData", sep=""))  
                   rm(list=c(curPerClass,curPerVeh))                  
              }
          }                       
    }
########################################## END #################################################################
