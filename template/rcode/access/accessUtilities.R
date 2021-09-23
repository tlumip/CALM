#/
#@filename accessUtilities.R
#@author Ben Stabler, benjamin.stabler@odot.state.or.us
#@version 1.2
#@date 4/6/04
#
#Calculate accessibility utilities for each mode by trip purpose
#Created 1/20/04 benjamin.stabler@odot.state.or.us
#Revised 1/26/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 2/27/04 Ben Stabler, to work with NAs in inputs
#Revised 3/22/04 Ben Stabler, to work with flexible
#utility functions and to calculate an average daily travel time 
#before exp() the utility for all modes not just auto modes.
#Revised 4/6/04 to average peak and offpeak utilities before
#Revised 3/04/08 Alex Bettinardi, to calculate mixRetA, as called for in the 2003 Utilies
#Revised 11/10/14 Martin Mann, 1.	All RData file loads were moved to the front of the script and 
 #placed in a conditional control structure which loads different population data sets depending 
 #on whether the University Model  is being run. 
#
#being exp() (average the whole utility not just individual inputs).
#Variable names cannot have numbers in them - replace 
# numbers with letter 1 to A, 2 to B, etc.

#This script reads in
#required inputs for a choice for all purposes.  Then it calculates
#a utility for each choice for each purpose and saves the 
#results.  The next script calculates the log sum of the choices.
#
#Inputs are various RData files depending on the choice and purpose
#Returns utilities by income for HBW, HBSHOP, HBREC, and HBOTH purposes
#Naming convention is util<mode><purpose>.RData
#
#The utility function depends on the variables specified in the
#input coefficient matrices.  This revision allows functions to be changed
#by simply changing the column (variable) names in the coefficient input
#CSV file.
#
#See pages 109 of the JEMnR User's Guide for a description.
#/

  cat("Calculate multimodal accessibility\n\n")
   
      fun$accessUtilities <- function(purpose) {
  
          #Create directory to store results
          if(!file.exists("access")) dir.create("access")

          ##/
          #REQUIRED FUNCTION
          #This function calculates a utility based on the variables input in the utility function file for the purpose.   
          #For modal utilities that differ by income, the appropriate way to specify the choice is <mode><lowInc|midInc|highInc> depending on the income class.  
          #The utilities are exponentiated and NAs are replaced with zeros
          #@param choiceUtils - choice utilities for purpose
          #@param choice - current choice
          #@return util - utilities matrix
          ##/
          
          calcAccessUtil <- function(choiceUtils, choice) {                           
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
                                  curMat <- sapply(c(matName,matModeName,matModePurpName),function(x) file.exists(paste("inputs/RData/",x,".RData", sep="")))
                                  switch(as.character(which(curMat)),
                                         "1" =  load(paste("inputs/RData/",matName,".RData", sep="")),
                                         "2" = {load(paste("inputs/RData/",matModeName,".RData", sep=""))
                                                  assign(mtx, get(matModeName))},
                                         "3" = {load(paste("inputs/RData/",matModePurpName,".RData", sep=""))
                                                  assign(mtx, get(matModePurpName))}                                       
                                  )       
                              }
                    
                              ##/
                              #EVALUATE UTILITY
                              #Evaluate Utility using inputs and then exponentiate
                              #@return util - utility for choice
                              ##/  
                             
                              #Evaluate utility function for purpose and turn off warnings (for log(-Inf))
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
          #/
      
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
    
          ##/
          #GET CURRENT UTILITY EQUATIONS
          #@return choiceUtils - choice utility defintions for purpose
          ##/
      
          choiceUtils <- get(paste(purpose, "AccessUtils", sep=""))      

          ##/
          #EVALUATE UTILITY BY MODE and INCOME
          #Calculate <mode> daily utility for each income class
          #Call function, assign to applicable name, save and remove object
          #@return <various> - saves results to a <mode><purpose>RData file
          ##/
      
          cModes <- names(choiceUtils)
          for(rMode in cModes) {  	
              cat("Current Purpose: ", purpose, "\tCurrent Choice: ", rMode, "\n")
  	
              #Call function to evaluate utility and assign result
              util <- calcAccessUtil(choiceUtils, rMode)
  	          assign(paste("util", rMode, purpose, sep=""), util)
    
              #Save Results             
              #print(table(round(util,1)))
              print(sum(util))
              save(list=paste("util", rMode, purpose, sep=""), file=paste("access/util", rMode, purpose, ".RData", sep=""))
              rm(list=c(paste("util", rMode, purpose, sep=""), "util"))
 	        }
          
          ##/      
          #REMOVE UNIVERSITY OBJECTS IF REQUIRED
          ##/
          
          if(purpose%in%c("hbw","nhbw") & as.logical(runUniversityModel)) rm(list=names(curObjs))
      }

########################################## END #################################################################