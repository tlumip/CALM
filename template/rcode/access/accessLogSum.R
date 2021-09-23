#/
#@filename accessLogSum.R
#@author Ben Stabler, benjamin.stabler@odot.state.or.us
#@version 1.2
#@date 4/6/04
#
#Calculate multimodal accessibility (daily logsums)
#This script calculates the log sum of the utilities for all choices for each purpose.  
#Created 1/20/04 benjamin.stabler@odot.state.or.us
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 3/22/04 to work with util<mode><purpose> inputs
#Revised 4/6/04 to work with util<modeIncludingIncome><purpose> inputs
#
#See page 110 of the JEMnR Model User's Guide for a description.
#/
      
      fun$accessLogSum <- function(purpose) {

          ##/
          #CALCULATE DAILY LOGSUM FOR EACH PURPOSE
          #Each mode's exponentiated utility is read in and added to the running total utility by purpose by income.
          #After all the modes have been added in, the total utility is logged, with infinities replaced with zero.
          #The resulting log sum (a list of three log sums by income) is then saved out.
          #Logsums that do not vary by income are output in this format as well for code generalization purposes.
          #@param <various> - various util<mode><purpose> utilities by income for each choice for each purpose
          #@return hbwlogSum - saves hbw log sum to RData file
          #@return hbslogSum - saves hbw log sum to RData file
          #@return hbrlogSum - saves hbr log sum to RData file
          #@return hbologSum - saves hbo log sum to RData file
          #@return nhbwlogSum - saves nhbv log sum to RData file
          #@return nhbnwlogSum - saves NHBNW log sum to RData file
          #@return hbcolllogSum - saves HBCOLL log sum to RData file
          ##/
      
          #Print message to console to info user
          cat("\nCalculating logSum for ", purpose, "multimodal accessibility\n")
      
          #Create object to store sum of all choices
          logSum <- list("lowInc"=0, "midInc"=0, "highInc"=0)
      
          #GET TRIP CHOICES FOR CURRENT PURPOSE
          choices <- names(get(paste(purpose, "AccessUtils", sep="")))
          incomes <- c("lowInc","midInc","highInc")
          choicesNoIncome <- unique(gsub("lowInc|midInc|highInc", "", choices))
          
          #ADD UTILITIES TOGETHER BY INCOME (COLLAPSE ON MODE)
          for(choice in choicesNoIncome) {
      	    cat("Current Purpose: ", purpose, "\tCurrent Choice: ", choice, "\n")
      	    
               if(choice %in% choices) {
                    #If true then utility does not vary by income
                    load(paste("access/", "util", choice, purpose, ".RData", sep=""))
                    logSum$lowInc <- logSum$lowInc + get(paste("util", choice, purpose, sep=""))
                    logSum$midInc <- logSum$midInc + get(paste("util", choice, purpose, sep=""))
                    logSum$highInc <- logSum$highInc + get(paste("util", choice, purpose, sep=""))
                    rm(list=paste("util", choice, purpose, sep=""))
               } else {
                    #Utility varies by income
                    load(paste("access/", "util", choice, "lowInc", purpose, ".RData", sep=""))
                    load(paste("access/", "util", choice, "midInc", purpose, ".RData", sep=""))
                    load(paste("access/", "util", choice, "highInc", purpose, ".RData", sep=""))
                    logSum$lowInc <- logSum$lowInc + get(paste("util", choice, "lowInc", purpose, sep=""))
                    logSum$midInc <- logSum$midInc + get(paste("util", choice, "midInc", purpose, sep=""))
                    logSum$highInc <- logSum$highInc + get(paste("util", choice, "highInc", purpose, sep=""))
                    rm(list=c(paste("util", choice, "lowInc", purpose, sep=""),
                          paste("util", choice, "midInc", purpose, sep=""),
                          paste("util", choice, "highInc", purpose, sep="")))
               }
          }
      
          #TAKE LOG OF UTILITIES BY INCOME
          logSum <- lapply(logSum, log)
          logSum <- lapply(logSum, function(x) { x[is.infinite(x)] <- 0; x } )
          #lapply(logSum,function(x) table(round(x,0)))
          #SAVE RESULTS 
          assign(paste("logSum", purpose, sep=""), logSum)
          rm(logSum)
          save(list=paste("logSum", purpose, sep=""), file=paste("access/logSum", purpose, ".RData", sep=""))
          rm(list=paste("logSum", purpose, sep=""))
      }

########################################## END #################################################################
