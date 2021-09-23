#/
#@filename hbwGen.102805v1.R
#@author Ben Stabler
#@version 1.3
#@date 10/28/05
#
#hbw trip productions
#Edited by WRS 11/05/02
#Revised 9/20/03 benjamin.stabler@odot.state.or.us
#Revised 10/31/03 benjamin.stabler@odot.state.or.us
#Revisd 1/28/04 benjamin.stabler@odot.state.or.us
#   saved out hbwAttractions for distribution balancing
#Revised 2/2/04 benjamin.stabler@odot.state.or.us
#Revised 8/12/04 benjamin.stabler@odot.state.or.us - pulled out calibration factor
#Revised 10/28/05 brian.j.gregor@odot.state.or.us - scales hbw trip productions only if the hbwTotalEmploymentCalibrationFactor is not NULL
#Revised 11/10/14 Martin Mann, All RData file loads were moved to the front of the script and placed in a conditional control structure which loads different population data sets depending on whether the University Model is being run.
#Revised 01/16/15 Martin Mann, Formatted code and comments, Removed Total Employment Calibration Factor
#
#See page 102 of the JEMnR User's Guide for a description.
#/

      cat("Home-Based Work Trip Generation\n\n")
    
      fun$hbwGen <- function() {
    
          #Create directory to store results
          if(!file.exists("tripgen")) dir.create("tripgen")
          
          #/
          #LOAD INPUTS
          #/
          
          #UNIVERSITY MODEL RUN INPUTS
          if(as.logical(runUniversityModel)) {
                  curObjs <- loadListObj("inputs/SynPop_StdFacLst.RData",c("cvalIndexAry","whiazcAry","retEmp","totalEmp"))
                  for(x in names(curObjs)) assign(x,curObjs[[x]])
          }
          #NON-UNIVERSITY MODEL RUN INPUTS
          if(!as.logical(runUniversityModel)) {          
                load("pregen/cvalIndexAry.RData")
                load("pregen/whiazcAry.RData")
                load("inputs/RData/retEmp.RData")
                load("inputs/RData/totalEmp.RData")  
          }

          cat(paste("Total Worker HH:",sum(whiazcAry)))
          
          ##/
          #HBW TRIP GENERATION 
          #HBW productions are a function of the number of workers by zone
          #HBW attractions are a function of employment by zone
          #HBW attractions are scaled so the total attractions matches total productions 
          #Optionally scale HBW productions by zone so the total prods match a value of total employment that has been scaled          
          #@param hbwGenRatesMtx - hbw generation rates
          #@param whiazcAry - wkr-hhs-age-inc-veh-zone Array
          #@param totalEmp - Total employment by zone  
          #@hbwTotalEmploymentCalibrationFactor - factor to scal employment and productions     
          #@return hbwTripProdAry - hbw trip productions arrays      
          #@return hbwAttractions - trip attractions
          ##/

          #CALCULATE PRODUCTIONS
          #Sweep the hbw trip production matrix across the worker dimension of the array
          hbwTripProdAry <- sweep(whiazcAry, getDims(whiazcAry,"worker"), hbwGenRatesMtx$rate, "*")
               
          #CALCULATE ATTRACTIONS and SCALE TO PRODUCTIONS
          avgHbwTripRateByEmp <-  sum(hbwTripProdAry) / sum(totalEmp) #avg HBW trips per employee
          hbwAttractions <- totalEmp * avgHbwTripRateByEmp #HBW trip attractions by zone
          names(hbwAttractions) <- zoneNames
          
          #SCALE PRODUCTIONS TO SCALED EMPLOYMENT
          #Note: hbw attractions would no longer neccesarily match productions MM 
          if(!is.null(hbwTotalEmploymentCalibrationFactor)) {
              #Calculate factor for HBW gen based on total employment 
              hbwFactor <- (sum(totalEmp) * hbwTotalEmploymentCalibrationFactor)/ sum(hbwTripProdAry)
              #Scale the hbw trip production array by the factor
              hbwTripProdAry <- hbwTripProdAry * hbwFactor
              #Rescale the hbw attractions vector by the factor
              hbwAttractions <- hbwAttractions * hbwFactor #added MM 020216
          }
       
          ##/      
          #PRINT OUT SUMMARY RESULTS
          ##/

          print(round(summary(hbwTripProdAry,"hhIncome"),4))
          print(round(c(tapply(hbwTripProdAry, cvalIndexAry, sum), total=sum(hbwTripProdAry)),0))
          
          ##/      
          #SAVE RESULTS
          ##/
          
          save(hbwTripProdAry, file="tripgen/hbwTripProdAry.RData")
          save(hbwAttractions, file="tripgen/hbwAttractions.RData")
          
          ##/      
          #REMOVE UNIVERSITY OBJECTS IF REQUIRED
          ##/
          
          if(as.logical(runUniversityModel)) rm(list=names(curObjs))

      }

########################################## END #################################################################