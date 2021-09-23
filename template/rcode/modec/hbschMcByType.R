#/
#@filename hbschMcByType.R
#@author Ben Stabler
#@version 1.3
#@date 8/5/04
#
# home-based school trips mode choice by school type
# Revised 2/23/04 benjamin.stabler@odot.state.or.us
# Revised 4/21/04 to output one array of all modes instead
#  of a matrix for each mode.
# Revised 5/5/04 to not assume districts numbered 1:numZones
# Revised 8/5/04 to do a mode split by school type
# Each zone must have a zone identified for elementary, middle and high school
# hbsch trips.  These school zone schemes are input in the districts csv file 
# as "elem", "middle", "high".  School trips produced are factored up to 
# match total model area school enrollment if it exists
# Inputs: districts, hbschDist, hbschModeChoicePercents (with school type)   
#School districts are coded as district scheme "sch" and are input in the districts.csv file  hbsch trips are only distributed within districts 
#Calculate trips for each mode by applying percent in hbschModeChoicePercents to the total trips for the appropriate zones by school type. 


    fun$hbschMcByType <- function() {
 
        ##/
        #CALCULATE SCHOOL MODE BY GRADE, INCOME, AND PERIOD   
        #@return util<mode><period><purpose>.rdata - Array of cross tabulations of workers,household size, age of householder, by zones 
        ##/
                   
        #Create directory to store results
        if(!file.exists("modec/hbsch")) dir.create("modec/hbsch")
      
        ##/
        #LOAD INPUTS
        ##/    
            load("tripdist/hbschDist.RData")          
            load("inputs/RData/districts.RData")
          
            #Get zones for each school district 
            zoneIndexByDistrict <- tapply(as.character(districts$zone), districts$sch, function(x) x)       
        
        #STOP PROCESSING IF ERROR
        #Check to make sure districts match in mode choice percents and in districts
            if(!all(hbschModeChoicePercents[,1] %in% names(zoneIndexByDistrict))) stop("Values in 'Dist' field of hbschModeChoicePercents do not match 'sch' field of the districts file")          
          
        ##/
        #CALCULATE HBSCH MODE CHOICE FOR EACH DISTRICT FOR EACH SCHOOL TYPE      
        ##/

            #Create trips array to store results by mode
            trips <- array(0, c(nrow(hbschDist), ncol(hbschDist), ncol(hbschModeChoicePercents) - 2))
            dimnames(trips) <- list(rownames(hbschDist), colnames(hbschDist),colnames(hbschModeChoicePercents)[3:ncol(hbschModeChoicePercents)])
              
            #LOOP THROUGH EACH MODE
            for(i in 3:ncol(hbschModeChoicePercents)) {                  
                  cat(paste("Calculate hbsch trips for ", colnames(hbschModeChoicePercents)[i], "\n\n", sep=""))
                    
                  #Build array to store results
                  hbschModeChoice <- array(0, c(nrow(districts), nrow(districts), 3))
                  dimnames(hbschModeChoice) <- list(districts[,1], districts[,1], c("elem","middle","high"))
                    
                   #LOOP THROUGH DISTRICT                      
                   for(k in 1:nrow(hbschModeChoicePercents)){                        
                        curDistType <- unlist(hbschModeChoicePercents[k,1:2])
                        index <- zoneIndexByDistrict[[as.character(curDistType[1])]]
                        hbschModeChoice[index,index,curDistType[2]] <- hbschDist[index,index,curDistType[2]]* hbschModeChoicePercents[k,i]
                   }
          
                  #Collapse on SchoolChoice 
                  hbschModeChoice <- apply(hbschModeChoice, c(1,2), sum)
                  trips[,,i-2] <- hbschModeChoice                  
                  
                  #GENERATE REPORT
                  #distsum("hbschModeChoice", paste(colnames(hbschModeChoicePercents)[i], sep=""),"sch", 3, "modec/hbsch/hbschMc", project, initials, districts)

                  rm(hbschModeChoice)
            }
              
            #Save resulting array
            save(trips, file="modec/hbsch/trips.RData")    
    }
    
########################################## END #################################################################