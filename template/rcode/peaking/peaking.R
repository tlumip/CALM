#/
#@filename peaking.R
#@author Ben Stabler
#@version 1.1
#@date 8/12/04
#NOTES:
#Apply directional (time-of-day) factors to convert matrices from
#P->A format to O->D format and to create assignment
#period matrices.  OD = pa + t(ap).  Collapses all
#trip purposes for <class>PeriodFactors to one OD.
#convert p->a to o->d for assignment period by applying directional (time-of-day) factors              
 #Loops through assignment periods and purposes as specified as in colnames in <class>PeriodFactors.
          #First load the <mode class> matrix for the purpose.  
          #Then get the pa factor and ap factor for that purpose for the assignment period.  Multiply matrix by factors and add the pa to the transpose of the ap
          #Save the result to <period><class>.RData in the peaking folder.    
          #The class "other" in the modes defintions table is used to identify modes that are not assigned and are thus omitted from this process.
          #park and ride classes are skipped and instead processed in pandrPeaking.R.
      

    fun$peaking <- function() {
          
          #/
          #CONVERT PA MATS TO OD MATS BY ASSIGNED CLASS'S, APPLY PERIOD FACTOR, COLLAPSE ON PURPOSE 
          #@return <period><class>.RData - <class> trips to assign
          #/
     
          cat("Create Peaking (Directional) and Assignment Period Specific Matrices\n\n")

          #Get classes, purposes, and TOD Directional Factors for classes
          tPurposes <- c(purposes, "hbsch")           
          indx <- !modes$type%in%c("pandr","other")
          classes <- as.character(unique(modes[indx,"class"]))
          curClassFactLst <- lapply(classes,function(cClass) get(paste(cClass, "PeriodFactors", sep="")))
          names(curClassFactLst) <- classes
          
          #Make a a list by class and periods
          allTripsLst <- lapply(classes,function(x) {
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
                                    temp <- paTripsLst[[x]] + t(apTripsLst[[x]])
                                    if(all(dim(temp) == 1)) temp <- 0
                                    temp
                                 })
                   names(cTripsLst) <- periods
                   allTripsLst[[cClass]] <- lapply(1:length(periods),function(x) allTripsLst[[cClass]][[x]] + cTripsLst[[x]])
                   rm(paTripsLst,apTripsLst,cTripsLst)
              }
         }  
         print(lapply(allTripsLst,function(x) lapply(x,function(y)round(sum(y,na.rm=T),0))))
         #//
         #SAVE RESULTS
         #//
         for(cClass in names(curClassFactLst)){
              periods <- colnames(curClassFactLst[[cClass]])
              for(period in 1:length(periods)){         
                   curPerClass <- paste(periods[period], cClass, sep="")
                   assign(curPerClass, allTripsLst[[cClass]][[period]])
                   save(list=curPerClass,file=paste("peaking/",curPerClass, ".RData", sep=""))                    
              }
          }                       
    }

########################################## END #################################################################
     