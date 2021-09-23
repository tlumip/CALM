#/
#@filename inputCheck_V4.R
#@author Alex Bettinardi
#@version 1.0
#@date 3/12/08
#
# Checks Inputs for Coding Errors and Inconsitentcies.
#
    # modified 12/6/2010 to list EMP categories > 1  instead of not equal (one could be rounding error) bap
    # modified 4/8/2016  - by Martin Mann
    #         1. Changed ePopUp verbage on HH distribution checks and removed check on AHHS that returned no ePopUp
    #         2. Modified school comparisons to update to current method
    #         3. Added Conditional check on off peak and peak transit matrix check
    #         4. Removed TOD check on transit period factors as they have nothing to do with TOD periods table
    # modeified 5/11/2016 - Alex Bettinardi - changed the file name to inputCheck_V4.R in the header above

         ePopUp <- function(eText, warnName="error") {
                         #  Only run interactive buttons if wantQ=T
                         if(wantQ){

                                  eQ <- tktoplevel()
                                  e1 <- tklabel(eQ, text=paste(eText,collapse="\n"))
                                  NoP <- tkbutton(eQ, text="Close Warning")
                                  blank <- tklabel(eQ, text="")
                                  Stop <- tklabel(eQ, text="Press ESC to STOP")
                                  blank2 <- tklabel(eQ, text="")
                                  tkpack(e1, blank, Stop, blank2, NoP)
                                  #  Function that reacts to user pressing "NoP"

                                  NOP <- function() {
                                         tkdestroy(eQ)
                                      }

                                  tkconfigure(NoP, command = NOP)

                          }# end of if wantQ statement

                          if(file.exists(paste(warnName, "Report.txt", sep=""))) {
                             error <- readLines(paste(warnName, "Report.txt", sep=""))
                          } else {
                             error <- c()
                          }
                          error <-c(date(), "",eText, "","", error)
                          writeLines(error, paste(warnName, "Report.txt", sep=""))

                    } # end of axisOpt question

    # Warning if Block Group numbers are NA
         if(any(is.na(taz$BGBASE))) {
            ePopUp(c("The following Block Groups are not populated", taz$TAZ[is.na(taz$BGBASE)]))
         }

    # Warning if Block Group numbers are all the same
         if(length(unique(taz$BGBASE))<3) {
            ePopUp(c("It looks like you have lost your Block Group data after opening in Excel"))
         }

    # Warning if average house size is less than 1
         if(any(taz$AHHSBASE<1 & taz$HHBASE>0)) {
            ePopUp(c("Check AHHS in the following zones", taz$TAZ[taz$AHHSBASE<1 & taz$HHBASE>0]))
         }

    # Warning if employment by industry doesn't match total
         if(any(abs(rowSums(taz[,paste(toupper(c("afr", "min", "con", "mfg", "tcp", "wst", "ret", "fin", "svc", "gvt")),"EMP", sep="")])- taz$EMPBASE)>1)) {
            ePopUp(c("Total Emp and Emp-by-Industry don't match for these Zones", taz$TAZ[abs(rowSums(taz[,paste(toupper(c("afr", "min", "con", "mfg", "tcp", "wst", "ret", "fin", "svc", "gvt")),"EMP", sep="")])- taz$EMPBASE)>1]))
         }

    # Warning if distribution does not sum to 100%
         distProb <- taz[taz$HHBASE > 0, c(paste("HHS",1:4,"BASE",sep=""), paste("HHI",1:4,"BASE",sep=""), paste("AGE",1:4,"BASE",sep=""))]
         rownames(distProb) <-  taz$TAZ[taz$HHBASE > 0]
         probTAZs <- distProb[round(rowSums(distProb)) != 3 ,]
         if(nrow(probTAZs) > 0) ePopUp(c("The following zones have HHs,\nbut have incomplete distributions:",rownames(probTAZs)))
        #/

    # Warning to make sure no zones have close to 100% distribution in one demographic
         distProb <- taz[taz$HHBASE > 10, c(paste("HHS",1:4,"BASE",sep=""), paste("HHI",1:4,"BASE",sep=""), paste("AGE",1:4,"BASE",sep=""))]
         rownames(distProb) <-  taz$TAZ[taz$HHBASE > 10]
         if(any(distProb > 0.95)) {
            ePopUp(c("The following zones have unusual distributions and should be checked",rownames(distProb)[apply(distProb, 1, function(x) any(x > 0.95))]))
         }
         #/

    # Warning if a school location is not assigned a destination or if a school trip destination has no school location #Modified by MM - 4/8/16
         schDestinations <- unique(c(districts$elem, districts$middle, districts$high))
         schDestinations <- schDestinations[!schDestinations%in%externalZones]
         taz$SCHEBASE <- rowSums(districts[rownames(taz),c("elemEnrl","middleEnrl","highEnrl")])
         schLocations <- taz$TAZ[taz$SCHEBASE > 0]
         zoneIntersection <- intersect(schDestinations ,schLocations)
         if(any(!schLocations%in%zoneIntersection)) {
            ePopUp(c("There are schools in these Zones,", "but the Districts file does not have these as destinations", "for the following zones", schLocations[!schLocations%in%zoneIntersection]))
         }
         if(any(!schDestinations%in%zoneIntersection)) {
              ePopUp(c("These zones are coded to be part of a school destination, \nbut there is no school enrolment coded", schDestinations[!schDestinations%in%zoneIntersection]))
         }

    # Ensure university model folder exists if running the model
         if(as.logical(runUniversityModel)) {
    	         if(!file.info('./unimodel')$isdir){
    		              ePopUp("University model directory is missing")
    		              stop("University model directory is missing")
    	         }
    	         if(!file.info('./unimodel/ctlfiles')$isdir){
    		            ePopUp("University model controls directory is missing")
    		            stop("University model controls directory is missing")
    	         }
    	         if(!file.info('./unimodel/tpau.jar')$size > 50000) {
    		            ePopUp("University model JAR file (tpau.jar) is missing or empty")
    		            stop("University model JAR file (tpau.jar) is missing or empty")
    	         }
         }

    # Ensure that the TOD period definitions are consistent
         TOD_period_names  <- (read.csv("inputs/TOD_Periods.csv", header=T, as.is=T))$Period
         PNRVeh_period_names  <- colnames(read.csv("inputs/pandrVehiclePeriodFactors.csv", header=T, as.is=T))
         Veh_period_names  <- colnames(read.csv("inputs/vehiclePeriodFactors.csv", header=T, as.is=T))

         if(!(TOD_period_names %in% PNRVeh_period_names & TOD_period_names %in% Veh_period_names) ) {
              ePopUp("Model time periods are inconsistent")
    		      stop("Model time periods are inconsistent")
    	   }

          rm(distProb, probTAZs, schDestinations, schLocations)

######################################################### END ####################################################################################
