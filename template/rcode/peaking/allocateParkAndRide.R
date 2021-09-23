#/
#@filename allocateParkAndRide.R
#@author Ben Stabler
#@version 1.0
#@date 4/21/04
#
#Split park and ride trips to vehicle and transit matrices.
#Note that each zone has a designated park and ride lot
#Resulting vehicle and transit trip matrices are saved.
#/

    fun$allocateParkAndRide <- function() {

         #/
         #ALLOCATE PARK AND RIDE TRIPS TO VEHICLE AND TRANSIT MATRICES
         # /
     
         cat("Split park and ride trips to vehicle and transit matrices\n\n")
    
         ##/
         #LOAD INPUTS
         ##/
        
         load("inputs/RData/parkAndRideLot.RData")
     
         ##/
         #SPLIT PARK AND RIDE TRIPS TO VEHICLE AND TRANSIT TRIPS
         #For each purpose (including hbsch) load the park and ride matrix.  
         #Lookup up lot and create pandr vehicle trips.  Then create transit trips to zones according to lot value.  
         #This code section loops through all the park and ride classes. 
         #@return <class>Vehicle - park and ride vehicle trips for class
         #@return <class>Transit - park and ride transit trips for class
         ##/
     
             #Get park and ride modes and their classes 
             pandrModes <- as.character(modes$mode[modes$type == "pandr"])
             pModes <- modes[modes$mode %in% pandrModes,]
             pandrModesByClass <- tapply(as.character(pModes$mode), as.character(pModes$class), function(x) x)
     
             #Loop through purposes
             tPurposes <- c(purposes, "hbsch")
             for(purpose in tPurposes) {     
                  #Loop through park and ride classes
                  for(pMode in names(pandrModesByClass)) {
                       #Load park and ride class input created in collapseTables()
                       load(paste("peaking/", purpose, "/", pMode, ".RData", sep="")) 
                                                          
                       if(sum(get(pMode)) > 0) {
                             #Create matrices to store results
                             pVehicle <- matrix(0, length(parkAndRideLot), length(parkAndRideLot), dimnames = list(zoneNames, zoneNames))
                             pTransit <- matrix(0, length(parkAndRideLot), length(parkAndRideLot), dimnames = list(zoneNames, zoneNames))                                          
                             
                             #Allocate park and ride trips to vehicle and transit zones
                             firstLegVehicle <- apply(get(pMode), 1, sum)
                             indx <- unique(parkAndRideLot)
                             for(x in as.character(indx[indx>0])) {
                                    pVehicle[as.character(parkAndRideLot)%in%x,x] <- firstLegVehicle[ as.character(parkAndRideLot)%in%x] 
                                    pTransit[x,] <- pTransit[x,] + colSums(get(pMode)[as.character(parkAndRideLot)%in%x,])
                             }
                       }   
                       
                       #Replace matrix of zeros with just a zero
                       if(sum(get(pMode)) == 0)  pVehicle <-pTransit <- 0
              
                       #Name and save resulting matrix
                       outFiles <- paste(pMode, c("Vehicle","Transit"), sep="")
                       assign(paste(outFiles[1], sep=""), pVehicle)
                       assign(paste(outFiles[2], sep=""), pTransit)
                       save(list=outFiles[1], file=paste("peaking/", purpose, "/",outFiles[1],".RData", sep=""))
                       save(list=outFiles[2],file=paste("peaking/", purpose, "/",outFiles[2],".RData", sep=""))
                       rm(pVehicle, pTransit)           
                 }     
            }
    }

########################################## END #################################################################
