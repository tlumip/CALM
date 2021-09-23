#/
#@filename collapseTables.R
#@author Ben Stabler
#@version 1.0
#@date 4/20/04
#
#Collapse mode choice results.
#This script collapses mode choice tables by income and peak and offPeak.
#It then collapses modes to classes based on the class input in modes.csv.
#
#/

    cat("Collapse mode choice results\n\n")

    fun$collapseTables <- function(purpose) {
         
          #/
          #Collapse peak and income specific matrices to just daily by purpose
          #/
          
              cat(paste("Collapse mode choice matrices for ", purpose, "\n\n", sep=""))     
              
              #Create directory to store results
              if(!file.exists("peaking")) dir.create("peaking")
              if(!file.exists(paste("peaking/", purpose, sep=""))) dir.create(paste("peaking/", purpose, sep=""))
    
              ##/
              #COLLAPSE PA MATRICES FROM MODE CHOICE BY PERIOD AND INCOME
              ##/
                  trips <- 0     
                  if(purpose != "hbsch") {          
                       incomes <- c("lowInc","midInc","highInc")
                       for(income in incomes) {
                           load(paste("modec/", purpose, "/", income, "/peakTrips.RData", sep=""))
                           load(paste("modec/", purpose, "/", income, "/offPeakTrips.RData", sep=""))
                           trips <- trips + (peakTrips + offPeakTrips)
                           rm(peakTrips, offPeakTrips)
                       }          
                  }
          
                  #Read in hbsch trips (hbsch trips are not by income or peak/offPeak) 
                  if(purpose == "hbsch") load(paste("modec/", purpose, "/trips.RData", sep=""))
             
              ##/
              #COLLAPSE PA MATRICES INTRIPS OBJECT FROM MODES TO CLASS     
              #Classes are input in modes.csv 
              #@return <class> - trips by mode class
              ##/
     
              #Get modes by class Select only those modes that are actually in the trips array                  
              modesByClass <- tapply(modes$mode, modes$class, function(x) x)
              modesByClass <- lapply(modesByClass, as.character)
    
                  #Loop thru current class's modes and add trips
                  for(i in 1:length(modesByClass)) {
                       cClass <- names(modesByClass)[i]
                       cModes <- modesByClass[[cClass]][modesByClass[[cClass]] %in% dimnames(trips)[[3]]]
                       assign(cClass, 0)
                       for(tMode in cModes) assign(cClass, get(cClass) + trips[,,tMode])
                       cat(paste(cClass,": ",round(sum(get(cClass)),0),"\n",sep=""))
                       
                       #Save Results
                       save(list=cClass, file= paste("peaking/", purpose, "/", cClass, ".RData", sep=""))
                       rm(list=cClass)
                  }

    }
