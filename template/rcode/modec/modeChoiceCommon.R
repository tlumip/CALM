#/
#@filename modeChoiceCommon.R
#@author Ben Stabler, benjamin.stabler@odot.state.or.us
#@version 1.2
#@date 3/22/04
#
#This script calculates mode choice inputs by first reading in
#required inputs for a choice for all purposes.  Then it calculates
#a utility for each choice for each purpose and saves the 
#results.  Only the common non-demographic variables in
#the utilities are calculated in this script.
#
#Revised 3/22 to work with variable utility functions.  See
#accessPartOne.R for more information.
#Revised 4/22/08 Alex Bettinardi, to calculate mixRetA, as called for in the 2003 Utilies
#Revised 11/10/14 Martin Mann, All RData file loads were moved to the front of the script and 
#                 placed in a conditional control structure which loads different population data sets 
#                 depending on whether the University Model is being run.
#
#Variable names cannot have numbers in them - replace 
# numbers with letter 1 to A, 2 to B, etc.

#See pages 116 of JEMnR Model User's Guide for a description.
#/

cat("Calculate mode choice common inputs\n\n")

fun$modeChoiceCommon <- function(purpose) {
    
          ##/
          #CALCULATE ACCESSIBITILY FOR MODE UTILITIES BY MODE AND PERIOD AND PUPPOSE    
          #@return util<mode><period><purpose>.rdata - Array of cross tabulations of workers,household size, age of householder, by zones 
          ##/            
          
          #Create directory to store results
          if(!file.exists("modec")) dir.create("modec")
          if(!file.exists("modec/common")) dir.create("modec/common")
                    
          ##/
          #REQUIRED FUNCTION
          #This function calculates a utility based on the variables input in the utility function file for the purpose.   
          #For modal utilities that differ by income, the appropriate way to specify the choice is <mode><lowInc|midInc|highInc> depending on the income class.  
          #Unlike the logsums for trip distribution, some inputs may vary by period since mode choice is calculated for peak and offpeak.         
          #The utilities are exponentiated and NAs are replaced with zeros
          #@return util - utilities matrix
          ##/
    
          calcModeCommonUtil <- function(choiceUtils, choice, period) {
    
                                  ##/
                                  #FIND VARIABLES FOR UTILITY
                                  #Get unique variable names in AccessUtils and check to see if any of the variable names conflict with objects already in workspace.
                                  ##/
                                   
                                  varNames <- unique(getVarNames(choiceUtils[choice]))
                                  varsToLoad <- varNames[!sapply(varNames,exists)]
                                  varsToLoad <- varsToLoad[!sapply(varsToLoad, function(x) exists(x, where=parent.frame(3)))]
                                  
                                  incomes <- c("lowInc","midInc","highInc")
                                  incomeSpecificChoice <- as.logical(length(grep("lowInc|midInc|highInc", choice)))
                                  choiceOnly <- choice
                                  if(incomeSpecificChoice)  choiceOnly <- gsub("lowInc|midInc|highInc", "", choice)
                                  
                                  ##/
                                  #LOAD VARIABLES FOR UTILITY
                                  #Load variables based on name of Matrix, Matrix-Mode(income), or Matrix-Mode-Purpose
                                  ##/
                                   
                                  for(mtx in varsToLoad) {
                                      matName <- mtx                                            
                                      matModeName <-paste(mtx, choiceOnly, sep="")
                                      matModePurpName <- paste(mtx, choiceOnly, purpose, sep="")
                                      matPeriodModeName <- paste(mtx, period, choiceOnly, sep="")
                                      curMat <- sapply(c(matName,matModeName,matModePurpName,matPeriodModeName),function(x) file.exists(paste("inputs/RData/",x,".RData", sep="")))
           
                                      switch(as.character(which(curMat)),
                                             "1" =  load(paste("inputs/RData/",matName,".RData", sep="")),
                                             "2" = {load(paste("inputs/RData/",matModeName,".RData", sep=""))
                                                      assign(mtx, get(matModeName))},
                                             "3" = {load(paste("inputs/RData/",matModePurpName,".RData", sep=""))
                                                      assign(mtx, get(matModePurpName))},                                       
                                             "4" = {load(paste("inputs/RData/",matPeriodModeName,".RData", sep=""))
                                                      assign(mtx, get(matPeriodModeName))}                                          
                                      )       
                                  }
                                 
                                  ##/
                                  #EVALUATE UTILITY
                                  #Evaluate Utility using inputs and then exponentiate
                                  #@return util - utility for choice
                                  ##/
                                 
                                  #Evaulate utility functions for purpose and turn off warnings (for log(-Inf))
                                  options(warn=-1)
                                  util <- eval(parse(text=choiceUtils[choice]))
                                  options(warn=0)
                                  #Take exponential function of utility and replace NAs with 0
                                  util <- exp(util)
                                  util[is.na(util)] <- 0 
                                  util
                              }
             
          #/
          #LOAD INPUTS
          ##/
    
          #University Model inputs
          if(purpose%in%c("hbw","nhbw") & as.logical(runUniversityModel)) {
                curObjs <- loadListObj("inputs/SynPop_StdFacLst.RData",c("mixrhm","mixthm"))
                for(x in names(curObjs)) assign(x,curObjs[[x]])
                cat(print("Running NoStudent_NoFaculty Population"))
          }
          #Non University Model inputs
          if(!(purpose%in%c("hbw","nhbw") & as.logical(runUniversityModel))) {          
                load("pregen/mixrhm.RData")
                load("pregen/mixthm.RData") 
                if(as.logical(runUniversityModel)) cat(print("Running NoStudentOnly Population"))
          }  
    
          ##/
          #CALCULATE UTILITY INPUTS
          #Read in accessibility measures calculated in pre-gen, log (natural), and rep to full matrix
          #@return mixTotA - total employment access within 1/2 mile of attraction zone replicated to full matrix
          #@return mixRetP - total retail employment access within 1/2 mile of production zone replicated to full matrix    
          ##/
                
          mixRetP <- matrix(log(mixrhm),length(mixrhm),length(mixrhm))
          mixRetA <- matrix(log(mixrhm),length(mixrhm),length(mixrhm), byrow=T)
          mixTotA <- matrix(log(mixthm),length(mixthm),length(mixthm), byrow=T)
          rm(mixrhm, mixthm)
          choiceUtils <- get(paste(purpose, "ModeUtils", sep=""))      
       
          ##/ 
          #EVALUATE UTILITY BY MODE,INCOME, AND PERIOD
          #@return <various> - saves results to a <mode><purpose>RData file
          ##/
    
          #Call function, assign to applicable name, save and remove object
          cModes <- names(choiceUtils)
          #sink("MCCommon.txt")
          for(rMode in cModes) {  	
              cat("Current Purpose: ", purpose, "\tCurrent Choice: ", rMode, "\n")
        	
        	    #Call function for peak and offPeak for mode choice
        	    peakUtil <- calcModeCommonUtil(choiceUtils, rMode, "peak")
        	    offPeakUtil <- calcModeCommonUtil(choiceUtils, rMode, "offPeak")    
              
              #print(round(sum(peakUtil)+sum(offPeakUtil),0))
                   	     	      	    
              #Save Peak
              curPeakUtil <- paste("util", rMode, purpose, "peak", sep="")
        	    assign(curPeakUtil, peakUtil)
        	    save(list=curPeakUtil, file=paste("modec/common/",curPeakUtil,".RData", sep=""))
        	    
              #Save OffPeak
              curOffPeakUtil <- paste("util", rMode, purpose, "offPeak", sep="")
        	    assign(curOffPeakUtil, offPeakUtil)
        	    save(list=curOffPeakUtil, file=paste("modec/common/",curOffPeakUtil,".RData", sep=""))
        	    
        	    rm(list=c(curPeakUtil, curOffPeakUtil, "peakUtil", "offPeakUtil"))
    	    }
         #sink()
          ##/      
          #REMOVE UNIVERSITY OBJECTS IF REQUIRED
          ##/
           	
          if(purpose%in%c("hbw","nhbw") & as.logical(runUniversityModel)) rm(list=names(curObjs)[!names(curObjs)%in%c("mixrhm","mixthm")])
      }

########################################## END #################################################################