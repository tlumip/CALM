#/
#@filename externalModel.R
#@author Ben Stabler
#@version 1.0
#@date 8/5/04
#
#External model
#The external model calculates EI and IE distributions based
#on external station ADT and a percent EI and IE.  It uses the
#percent of model area productions by zone for hbw, hbs, hbr, 
#hbo, and hbcoll for vehicle trips to distribute IE trips.  It 
#uses the percent of model area attractions (the As of the vehicle
#PA matrices) by zone for the same purposes above to distribute
#EI trips.  Thus it assumes that EI trips are attracted to internal
#destinations at the same rate as internal trips, and vice versa for
#IE trips.  The daily EE trips matrix must be input as well.
#/

cat("External model\n\n")

fun$externalModel <- function() {
     
    #/
    #Calculate EI and IE Distribution
    #/
    
    ##/
    #Load EE distribution
    #@param eeDist - EE distribution
    #@return eeDist - EE distribution
    ##/
    load("inputs/RData/eeDist.RData")
    eeDist <- ipf(cbind(externals[,"ADT"]*0.5*(1-externals[,"ei.pct"]-externals[,"ie.pct"]),externals[,"ADT"]*0.5*(1-externals[,"ei.pct"]-externals[,"ie.pct"])), eeDist)
        
    ##/
    #Add up total PA matrices for "hbw" "hbs" "hbr" "hbo" "hbcoll" for vehicle trips
    #colSums is total attractions by zone and rowSums is total productions by zone
    #Remove ee, ei, and ie from totalPA (they should all be zeros anyway)
    #@param <purpose>/vehicle.RData - vehicle PA by purpose
    #@param eeDist - EE distribution matrix
    #@return totalPA - total PA trips for the five trip purposes
    ##/
    purposes <- c("hbw","hbs","hbr","hbo","hbcoll")
    totalPA <- 0
    for(purpose in purposes) {
         load(paste("peaking/", purpose, "/vehicle.RData", sep=""))
         totalPA <- totalPA + vehicle
    }
    totalPA <- totalPA[(nrow(eeDist)+1):nrow(totalPA),(nrow(eeDist)+1):nrow(totalPA)]
    
    ##/
    #Calculate IE and EI distribution
    #@param externals - external station data ordered by zone (including percent ie and ei)
    #@param totalPA - total PA trips for the five trip purposes
    #@return ieDist - IE distribution matrix
    #@return eiDist - EI distribution matrix
    ##/
    ie <- externals$ADT * externals$ie.pct
    ieDist <- outer(rowSums(totalPA) / sum(totalPA), ie)    
    ei <- externals$ADT * externals$ei.pct
    eiDist <- outer(ei, colSums(totalPA) / sum(totalPA))
    
    ##/
    #Build external matrix
    #Build full matrix with external cell values filled in
    #Create a daily external OD demand matrix and a daily external PA matrix
    #@param totalPA - total PA trips for the five trip purposes
    #@param ieDist - IE distribution matrix
    #@param eiDist - EI distribution matrix
    #@param eeDist - EE distribution matrix
    #@return externalpa - save external PA matrix (zeros for II)
    #@return externalod <- save daily external OD matrix
    ##/
    iiDummy <- matrix(0, nrow(totalPA), nrow(totalPA))
    external <- rbind(cbind(eeDist, eiDist), cbind(ieDist, iiDummy))
    zoneNames <- c(rownames(eeDist), rownames(ieDist))
    dimnames(external) <- list(zoneNames, zoneNames)
    externalod <- (external + t(external))/2 #0.5 + 0.5*t()
    externalpa <- external
    
    #Save out external matrix
    save(externalpa, file="peaking/externalpa.RData")
    save(externalod, file="peaking/externalod.RData")
    cat("External matrix created\n\n")
    
}
