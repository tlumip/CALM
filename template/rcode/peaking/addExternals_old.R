#/
#@filename addExternals.R
#@author Ben Stabler
#@version 1.1
#@date 8/5/04
#
#Add external model results to assignment period matrices
#By default, external trips are added to the assignment 
#class that corresponds to the driveAlone mode.  This can
#be changed since it is an argument to the function.
#
#If external matrix is calculated outside this process, then the
#matrix will be stored in the inputs folder and the default for the
#directory argument will work.  If external.R creates the external
#matrix then the directory argument should be changed to "peaking"
#since the external model (externalModel.R) outputs its external
#matrix to that folder.  Note that the external model matrix input
#to this function is an OD matrix.
#
# BTS 03/14/12 - Added separate time-of-day factoring of E-E vs. I-E and E-I trips
#/

cat("Add external model results to assignment period matrices\n\n")

fun$addExternals_old <- function(vehicleMode = "driveAlone", directory="inputs/RData") {
     
     #/
     #Add external model trips to vehicle assignment periods
     #/
     
     cat("Add external model results to assignment period matrices\n\n")
     
     ##/
     #Add external trips to each external period.  
     #First load the demand for the assignment class for vehicles 
     #(which by default is "driveAlone"). This will be the class that 
     #external trips are added to.  Then load the external source 
     #matrix for that class.  Multiply the external source matrix by the 
     #period percent and add it to the vehicle class.  Save the resulting 
     #matrix.  If external matrix is calculated outside this process, then 
     #then matrix will be stored in the inputs folder and the default for the
     #directory argument will work.  If external.R creates the external
     #matrix then the directory argument should be changed to "peaking"
     #since the external model (externalModel.R) outputs its external
     #matrix to that folder.  Note that the external model matrix input
     #to this function is an OD matrix.
     #@param modes - mode defintions
     #@param directory - location of external matrix
     #@param <external matrix name> - external source matrix
     #@param externalPeriodFactors - external period definitions
     #@param <period><vehicle mode class> - vehicle class matrix identified in modes 
     #@return <period><vehicle mode class> - vehicle class matrix identified in modes
     ##/
     
     for(i in 1:nrow(externalPeriodFactors)) {
     
          #Get vehicle class name
          vClass <- as.character(modes$class[modes$mode == vehicleMode])
          
          #Load external source matrix and vehicle matrix to add to
          #optional directory argument - location of external matrix
          load(paste(directory, "/", externalPeriodFactors$matrix[i], ".RData", sep=""))
          load(paste("peaking/", externalPeriodFactors$period[i], vClass, ".RData", sep=""))
          
          #Calculate external trips for period and external movement type (ie, ei, ee)
          extMat <- get(paste(externalPeriodFactors$matrix[i], sep=""))
          
          eZones <- 1:nrow(externals)
          iZones <- (max(eZones)+1):nrow(extMat)
          extMat[iZones,eZones] <- extMat[iZones,eZones] * externalPeriodFactors$percent_ie[i]
          extMat[eZones,iZones] <- extMat[eZones,iZones] * externalPeriodFactors$percent_ei[i]
          extMat[eZones,eZones] <- extMat[eZones,eZones] * externalPeriodFactors$percent_ee[i]
          
          #Add to vehicles and save
          assign(paste(externalPeriodFactors$period[i], vClass, sep=""), 
               get(paste(externalPeriodFactors$period[i], vClass, sep="")) + extMat)
          save(list=paste(externalPeriodFactors$period[i], vClass, sep=""), 
               file=paste("peaking/", externalPeriodFactors$period[i], vClass, ".RData", sep=""))
     }
     
}
