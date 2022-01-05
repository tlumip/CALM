      #/
      #@filename inputsSaveMedford.R
      #@author Ben Stabler, benjamin.stabler@odot.state.or.us
      #@version 1.2
      #@date 8/12/04
      #@revauthor Brian Gregor, brian.j.gregor@odot.state.or.us
      #@revdate 6/02/05
      #@revauthor Alex Bettinardi, alexander.o.bettinardi@odot.state.or.us
      #@revdate 2/04/08
      #
      #This revised version of the script includes notes and revisions
      #relating to negative input values where they should not exist.
      #Discovered positive mode choice log sums. These should not exist. Negative input values
      #can contribute to the problem.
      #
      #Preprocess all model inputs that are saved to RData files.
      #All model inputs that are saved out are preprocessed in this file.
      #All of the inputs are saved in R binary format as RData files.
      #Replaced all 9999 (OD pair not available) to NA
      #
      #Almost all of the model inputs in inputsSave.R are either a vector
      #for each zone or a full square matrix of values.  Each input listed
      #in this file has an example and a source (which is either developer
      #or client).  Client source inputs are those that are usually provided
      #by the customer.
      #
      #Currently, most of the data is stored in an EMME/2 databank.  So the input
      #format is the format from the bank - which assumes the zones are ordered
      #from lowest to highest for both vectors and square matrices.  However,
      #this file can be edited to read in input matrices and vectors as CSV files
      #or as EMME/2 batch out format, or any other format that is needed.
      #
      #Since the EMME/2 square matrix and vector formats do not have any row or column
      #names (i.e the inputs are just the data) an example of a file is not needed.
      #Instead, the example tag is either "vector" or "square matrix" to identify the
      #input format.
      #
      #If an alternative to the databank is used, then the following format is suggested.
      #Use a CSV file with the first column the from zone, the second column the two
      #zone and the third column the value.  For vectors, just input the zone as
      #column one and the value as column two.  It is important to have a row/entry
      #for every zone or OD pair even if the value is zero or not available.  The
      #EMME/2 batch out format can be read in as well with the readEmme2 function.  It
      #assumes the file format is as follows:  The frist four rows are skipped and each
      #OD pair is a row in the form of: fromnode tonode: value.  For example: 4 5: 3.4
      #
      #Finally, if mode names other than the default supplied in the modes object are
      #choosen, all of the saved RData file names in this file must be changed to reflect
      #the new names.  It is recommended that the default mode names be used.
      #
      #Revised 3/19/04 to create RData files with a more standard
      #naming convention <variable><period><mode><purpose>.RData
      #Also removed interCapping for pasted names - just used
      #interCapping for names by themselves.  For example:
      #opCostlrtBus.  Create unique files for each auto mode
      #to be able to generalize the code further.
      #
      #Revised 8/12/04 to process various inputs from databank
      #
      #---------------------------------------------------------
      #Many of the inputs came from the jemnrInputs folder in
      #the MedfordModel folder on 6420only
      #---------------------------------------------------------
      #
      #Variable names defined in utility definitions must be the same as
      #the names of the variables created in inputsSave.R and inputs.R.
      #/
      # Revised 2/4/08, there in the utilities that is caused by a zero walk travel time
      # The error is that a zero travel time, because inf accessible in the accessiblity utilities
      # The code then correctly assumes that the Inf should be zero and makes intrazonal trips
      # have a zero for the access utility, basically removing all intralzonal trips (except sch)
      # to correct this the walk travel time is moved past line 500 (after the intralzonal distance has been calculated)
      # This creates the correct non-zero values for intrazonal travel time.

      # Revised bts 1/11/12 to read tAvailpeak and tAvailoffPeak now

      # BTS 03/14/12 - multiple chages to update JEMnR to work with a settings.csv file,
      # read matrices from batchout files, read new/updated inputs such as TAZ.csv and coverageFactors.csv

      #12/03/13 Martin Mann, moved zone information retrieval from districts file to inputs, added taz rdata save

      # Revised 4/2014, Sriram Narayanamoorthy (narayanamoorthys@pbworld.com)
      #   1) Added log of tripDistance for auto

      # Revised 10/9/2014, Martin Mann:
      #   1) Removed any reading of TAZ and Districts csv
      #   2) Removed vector of names for census taz distributions
      #   3) Changed format of  pumaFileName object from the settings file to not include ".csv" and the path

      # Revised 11/10/14, Martin Mann minor syntx formatting change - no impact to code.

      # Revised 01/9/2015, Martin Mann:
      #   1) Added Log of Auto and Walk Trip Distance
      #   2) Formatting change and removal of unused code for readability

      # Revised 06/23/2016, Martin Mann:
      #   Added Printing of table of Iteration to close IPF of HIAZAry

      # Revised 04/07/2021, Michael McCarthy:
      #   Read Skims from VISUM via OMX
      #   Write matrices to Zip Matrix (ZMX) format for University model

####################################### START ############################################################

      cat("Read in, preprocess, and save inputs\n\n")

      # PUT Daily: 1 Transit Demand; 2 IVT; 3 Aux Time; 4 await; 5 bwait; 6 walk; 9 transfers; 10 journey dist
      # PUT Peak: 1 IVT; 2 Aux; 3 await; 4 bwait; 5 walk; 6 access time; 7 egress time; 8 transfers; 9 journey dist
      # PrT Daily: 2 auto demand; 3 t0 time; 4 tcur time; 5 distance
      # PrT Peak: 2 auto demand; 3 t0 time; 4 tcur time; 5 distance
      #matNumsPrT <- if(iter == 1){ c(tt = 2, dist = 3) }else{ c(tt = 4, dist = 5) } # Off-peak + peak travel time, distances
      #matNumsPuT <- if(iter == 1){ c(ivt = 4, await = 6, bwait = 7, walk = 8, transfers = 11, dist = 12) }else{ c(ivt = 6, await = 8, bwait = 9, walk = 10, transfers = 13, dist = 14) } # In-Vehicle Time, origin wait, transfer wait, walk time, number of transfers, distance
      dailySkim <- "outputs/matrices/daily_skim.omx"
      peakSkim <- "outputs/matrices/peak_skim.omx"
      bikeskim <- "outputs/matrices/bike_skim.omx"
      walkskim <- "outputs/matrices/walk_skim.omx"

      matZones <- readLookupOMX("outputs/matrices/daily_skim.omx","NO")

      ##

      ##/
      #COVERAGE FACTORS AND TRANSIT AVAILABILITY
      #replicated to full matrix by column (origin)
      #Coverage Factors are the percent of households or employment in each zone that is within walking distance to transit.
      #The usual criteria is 1/4 mile to a bus line or 1/2 mile to a transit center or LRT station
      #Set each zone to zero if no transit available.
      #@return peakHhCov - peak household coverage factor
      #@return offPeakHhCov - offpeak household coverage factor
      #@return peakEmpCov - peak employment coverage factor
      #@return OFFpeakEmpCov - peak employment coverage factor
      ##/

      #Peak Household Coverage Factor
      peakHhCov <- taz$peakHhCov
      peakHhCov <- c(rep(0, length(externalZones)), peakHhCov)
      peakHhCov[is.na(peakHhCov)] <- 0
      peakHhCov <- matrix(peakHhCov, length(peakHhCov), length(peakHhCov))
      save(peakHhCov, file="inputs/RData/peakHhCov.RData")

      #offPeak Household Coverage Factor
      offPeakHhCov <- taz$offPeakHhCov
      offPeakHhCov <- c(rep(0, length(externalZones)), offPeakHhCov)
      offPeakHhCov[is.na(offPeakHhCov)] <- 0
      offPeakHhCov <- matrix(offPeakHhCov, length(offPeakHhCov), length(offPeakHhCov))
      save(offPeakHhCov, file="inputs/RData/offPeakHhCov.RData")

      #peak Employment Coverage Factor
      peakEmpCov <- taz$peakEmpCov
      peakEmpCov <- c(rep(0, length(externalZones)), peakEmpCov)
      peakEmpCov[is.na(peakEmpCov)] <- 0
      peakEmpCov <- matrix(peakEmpCov, length(peakEmpCov), length(peakEmpCov))
      save(peakEmpCov, file="inputs/RData/peakEmpCov.RData")

      #offPeak Employment Coverage Factor
      offPeakEmpCov <- taz$offPeakEmpCov
      offPeakEmpCov <- c(rep(0, length(externalZones)), offPeakEmpCov)
      offPeakEmpCov[is.na(offPeakEmpCov)] <- 0
      offPeakEmpCov <- matrix(offPeakEmpCov, length(offPeakEmpCov), length(offPeakEmpCov))
      save(offPeakEmpCov, file="inputs/RData/offPeakEmpCov.RData")

      ##/
      #TRANSIT AVAILABILITY
      #This vector is used to NA out OD pairs where transit is not
      #available so the choice model has an accurate representation of the choices available.
      #If (hh + emp coverage) > 0 then transit available
      #@param coverageFactors - converage factors
      #@return tAvailpeak - transit availability vector for peak
      #@return tAvailoffPeak - transit availability vector for offpeak
      ##/

      tAvailpeak <- (peakHhCov[,1] + peakEmpCov[,1]) > 0
      tAvailoffPeak <- (offPeakHhCov[,1] + offPeakEmpCov[,1]) > 0

########################## PEAK TRANSIT TRAVEL TIME MATRICES #########################################

      ##/
      #AM PEAK BUS FOR IN-VEHICLE TIME
      #@return ivTimepeakbusWalk - in-vehicle time for Bus for BusWalk choice
      #If value > 9999, or !tAvail is TRUE, or value = 0, set all to NA (OD pair not available)
      #Set diagonal of matrix to NA
      ##/

      ivTimepeakbusWalk <- readSelectedOMX(peakSkim, ivTimepeakbusWalk_omxName)
      dimnames(ivTimepeakbusWalk) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(ivTimepeakbusWalk,paste0("unimodel/emmemat/mf",ivTimepeakbusWalk_matNum,".zmx"),path7zipexe)
      diag(ivTimepeakbusWalk) <- NA
      ivTimepeakbusWalk[ivTimepeakbusWalk > 9999] <- NA
      ivTimepeakbusWalk[ivTimepeakbusWalk == 0] <- NA
      ivTimepeakbusWalk[!tAvailpeak,] <- ivTimepeakbusWalk[,!tAvailpeak] <- NA
      save(ivTimepeakbusWalk, file="inputs/RData/ivTimepeakbusWalk.RData")
      rm(ivTimepeakbusWalk)

      ##/
      #AM PEAK BUS FOR WALK TO TRANSIT TIME
      #@return walkTimepeakbusWalk - walk time to Bus for BusWalk choice
      #If value > 9999 or !tAvail is TRUE, set all to NA (OD pair not available)
      #Adjust times to cap at 30 minutes
      #Set diagonal of matrix to NA
      ##/

      walkTimepeakbusWalk <- readSelectedOMX(peakSkim, walkTimepeakbusWalk_omxName)
      dimnames(walkTimepeakbusWalk) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(walkTimepeakbusWalk,paste0("unimodel/emmemat/mf",walkTimepeakbusWalk_matNum,".zmx"),path7zipexe)
      diag(walkTimepeakbusWalk) <- NA
      walkTimepeakbusWalk[walkTimepeakbusWalk > pkWalkTimeCap & walkTimepeakbusWalk < 9998] <- pkWalkTimeCap
      walkTimepeakbusWalk[walkTimepeakbusWalk > 9999] <- NA
      walkTimepeakbusWalk[!tAvailpeak,] <- walkTimepeakbusWalk[,!tAvailpeak] <- NA
      save(walkTimepeakbusWalk, file="inputs/RData/walkTimepeakbusWalk.RData")
      rm(walkTimepeakbusWalk)

      ##/
      #AM PEAK BUS WAIT TIME
      #@return waitTimeAoffPeakbusWalk - initial wait time for Bus for BusWalk choice
      #@return waitTimeBoffPeakbusWalk - wait time 2 for Bus for BusWalk choice
      #Adjust times to cap at 30 minutes (except those coded 9999, which is Metro's
      #code for choice not available between OD)
      #Replace >9999 and !tAvail with NA (OD pair not available)
      #Set diagonal of matrix to NA
      #Second wait is the difference: totalWait - FirstWait
      ##/


      firstWait <- readSelectedOMX(peakSkim, waitTimeApeakbusWalk_omxName)
      secondWait <- readSelectedOMX(peakSkim, waitTimeBpeakbusWalk_omxName)
      dimnames(firstWait) <- list(matZones$Lookup,matZones$Lookup)
      dimnames(secondWait) <- list(matZones$Lookup,matZones$Lookup)

      writeZipMat(firstWait,paste0("unimodel/emmemat/mf",waitTimeApeakbusWalk_matNum,".zmx"),path7zipexe)
      writeZipMat(secondWait,paste0("unimodel/emmemat/mf",waitTimeBpeakbusWalk_matNum,".zmx"),path7zipexe)

      #The totalWait and firstWalk have a large number of values that are extremely high (1e20). All of these are in zones where there is no transit available.
      #secondWait has a very large number of zones with fractional negative values. If all the values for totalWait and firstWait where transit is not available are
      #changed to NA, the number of negative numbers drops hugely. Still the wait should not be negative, so change these to 0
      firstWait[!tAvailpeak,] <- firstWait[,!tAvailpeak] <- NA
      secondWait[secondWait < 0 ] <- 0

      #FIRST WAIT
      waitTimeApeakbusWalk <- firstWait
      diag(waitTimeApeakbusWalk) <- NA
      waitTimeApeakbusWalk[waitTimeApeakbusWalk > pkWaitTimeACap & waitTimeApeakbusWalk < 9998] <- pkWaitTimeACap
      waitTimeApeakbusWalk[waitTimeApeakbusWalk > 9999] <- NA
      waitTimeApeakbusWalk[!tAvailpeak,] <- waitTimeApeakbusWalk[,!tAvailpeak] <- NA
      save(waitTimeApeakbusWalk, file="inputs/RData/waitTimeApeakbusWalk.RData")
      rm(waitTimeApeakbusWalk)

      #SECOND WAIT
      waitTimeBpeakbusWalk <- secondWait
      diag(waitTimeBpeakbusWalk) <- NA
      waitTimeBpeakbusWalk[waitTimeBpeakbusWalk > pkWaitTimeBCap & waitTimeBpeakbusWalk < 9998] <- pkWaitTimeBCap
      waitTimeBpeakbusWalk[waitTimeBpeakbusWalk > 9999] <- NA
      waitTimeBpeakbusWalk[!tAvailpeak,] <- waitTimeBpeakbusWalk[,!tAvailpeak] <- NA
      save(waitTimeBpeakbusWalk, file="inputs/RData/waitTimeBpeakbusWalk.RData")
      rm(waitTimeBpeakbusWalk)
      rm(totalWait,firstWait,secondWait)

      ##/
      #AM PEAK BUS FOR BOARDINGS
      #@return boardingsoffPeakbusWalk - number of boardings on Bus for BusWalk choice
      #Subtract one from number of boardings
      #Replace >9999 and !tAvail with NA (OD pair not available)
      ##/

      boardingspeakbusWalk <- readSelectedOMX(peakSkim, boardingspeakbusWalk_omxName) # number of transfers
      dimnames(boardingspeakbusWalk) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(boardingspeakbusWalk,paste0("unimodel/emmemat/mf",boardingspeakbusWalk_matNum,".zmx"),path7zipexe)
      boardingspeakbusWalk[boardingspeakbusWalk > 9999] <- NA
      # Fix for boardings/transfers difference
      #boardingspeakbusWalk[boardingspeakbusWalk > 0 & !is.na(boardingspeakbusWalk)] <- boardingspeakbusWalk[boardingspeakbusWalk > 0 & !is.na(boardingspeakbusWalk)] - 1
      boardingspeakbusWalk[!tAvailpeak,] <- boardingspeakbusWalk[,!tAvailpeak] <- NA
      save(boardingspeakbusWalk, file="inputs/RData/boardingspeakbusWalk.RData")
      rm(boardingspeakbusWalk)

########################## OFF PEAK TRANSIT  TRAVEL TIME MATRICES #########################################

      ##/
      #MIDDAY OFFPEAK BUS FOR IN-VEHICLE TIME
      #@return ivTimeoffPeakbusWalk - in-vehicle time for Bus for BusWalk choice
      #If value > 9999, or !tAvail is TRUE, or value = 0, set all to NA (OD pair not available)
      #Set diagonal of matrix to NA
      ##/

      ivTimeoffPeakbusWalk  <- readSelectedOMX(dailySkim, ivTimeoffPeakbusWalk_omxName)
      dimnames(ivTimeoffPeakbusWalk) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(ivTimeoffPeakbusWalk,paste0("unimodel/emmemat/mf",ivTimeoffPeakbusWalk_matNum,".zmx"),path7zipexe)
      diag(ivTimeoffPeakbusWalk) <- NA
      ivTimeoffPeakbusWalk[ivTimeoffPeakbusWalk > 9999] <- NA
      ivTimeoffPeakbusWalk[ivTimeoffPeakbusWalk == 0] <- NA
      ivTimeoffPeakbusWalk[!tAvailoffPeak,] <- ivTimeoffPeakbusWalk[,!tAvailoffPeak] <- NA
      save(ivTimeoffPeakbusWalk, file="inputs/RData/ivTimeoffPeakbusWalk.RData")
      rm(ivTimeoffPeakbusWalk)

      ##/
      #MIDDAY OFFPEAK BUS FOR WALK TO TRANSIT TIME
      #@return walkTimeoffPeakbusWalk - walk time to Bus for BusWalk choice
      #If value > 9999 or !tAvail is TRUE, set all to NA (OD pair not available)
      #Adjust times to cap at 30 minutes
      #Set diagonal of matrix to NA
      ##/

      walkTimeoffPeakbusWalk <- readSelectedOMX(dailySkim, walkTimeoffPeakbusWalk_omxName)
      dimnames(walkTimeoffPeakbusWalk) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(walkTimeoffPeakbusWalk,paste0("unimodel/emmemat/mf",walkTimeoffPeakbusWalk_matNum,".zmx"),path7zipexe)
      diag(walkTimeoffPeakbusWalk) <- NA
      walkTimeoffPeakbusWalk[walkTimeoffPeakbusWalk > opWalkTimeCap & walkTimeoffPeakbusWalk < 9998] <- opWalkTimeCap
      walkTimeoffPeakbusWalk[walkTimeoffPeakbusWalk > 9999] <- NA
      walkTimeoffPeakbusWalk[!tAvailoffPeak,] <- walkTimeoffPeakbusWalk[,!tAvailoffPeak] <- NA
      save(walkTimeoffPeakbusWalk, file="inputs/RData/walkTimeoffPeakbusWalk.RData")
      rm(walkTimeoffPeakbusWalk)

      ##/
      #MIDDAY OFFPEAK BUS WAIT TIME
      #@return waitTimeAoffPeakbusWalk - initial wait time for Bus for BusWalk choice
      #@return waitTimeBoffPeakbusWalk - wait time 2 for Bus for BusWalk choice
      #Adjust times to cap at 30 minutes (except those coded 9999, which is Metro's
      #code for choice not available between OD)
      #Replace >9999 and !tAvail with NA (OD pair not available)
      #Set diagonal of matrix to NA
      #Second wait is the difference: totalWait - FirstWait
      ##/

      firstWait <- readSelectedOMX(dailySkim, waitTimeAoffPeakbusWalk_omxName)
      secondWait <- readSelectedOMX(dailySkim, waitTimeBoffPeakbusWalk_omxName)
      dimnames(firstWait) <- list(matZones$Lookup,matZones$Lookup)
      dimnames(secondWait) <- list(matZones$Lookup,matZones$Lookup)

      writeZipMat(firstWait,paste0("unimodel/emmemat/mf",waitTimeAoffPeakbusWalk_matNum,".zmx"),path7zipexe)
      writeZipMat(secondWait,paste0("unimodel/emmemat/mf",waitTimeBoffPeakbusWalk_matNum,".zmx"),path7zipexe)

      #The totalWait and firstWalk have a large number of values that are extremely high (1e20). All of these are in zones where there is no transit available.
      #secondWait has a very large number of zones with fractional negative values. If all the values for totalWait and firstWait where transit is not available are
      #changed to NA, the number of negative numbers drops hugely. Still the wait should not be negative, so change these to 0
      firstWait[!tAvailoffPeak,] <- firstWait[,!tAvailoffPeak] <- NA
      secondWait[secondWait < 0 ] <- 0

      #FIRST WAIT
      waitTimeAoffPeakbusWalk <- firstWait
      diag(waitTimeAoffPeakbusWalk) <- NA
      waitTimeAoffPeakbusWalk[waitTimeAoffPeakbusWalk > opWaitTimeACap & waitTimeAoffPeakbusWalk < 9998] <- opWaitTimeACap
      waitTimeAoffPeakbusWalk[waitTimeAoffPeakbusWalk > 9999] <- NA
      waitTimeAoffPeakbusWalk[!tAvailoffPeak,] <- waitTimeAoffPeakbusWalk[,!tAvailoffPeak] <- NA
      save(waitTimeAoffPeakbusWalk, file="inputs/RData/waitTimeAoffPeakbusWalk.RData")
      rm(waitTimeAoffPeakbusWalk)

      #SECOND WAIT
      waitTimeBoffPeakbusWalk <- secondWait
      diag(waitTimeBoffPeakbusWalk) <- NA
      waitTimeBoffPeakbusWalk[waitTimeBoffPeakbusWalk > opWaitTimeBCap & waitTimeBoffPeakbusWalk < 9998] <- opWaitTimeBCap
      waitTimeBoffPeakbusWalk[waitTimeBoffPeakbusWalk > 9999] <- NA
      waitTimeBoffPeakbusWalk[!tAvailoffPeak,] <- waitTimeBoffPeakbusWalk[,!tAvailoffPeak] <- NA
      save(waitTimeBoffPeakbusWalk, file="inputs/RData/waitTimeBoffPeakbusWalk.RData")
      rm(waitTimeBoffPeakbusWalk)
      rm(totalWait,firstWait,secondWait)

      ##/
      #MIDDAY OFFPEAK BUS FOR BOARDINGS
      #@return boardingsoffPeakbusWalk - number of boardings on Bus for BusWalk choice
      #Subtract one from number of boardings
      #Replace >9999 and !tAvail with NA (OD pair not available)
      ##/

      boardingsoffPeakbusWalk <- readSelectedOMX(dailySkim, boardingsoffPeakbusWalk_omxName)
      dimnames(boardingsoffPeakbusWalk ) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(boardingsoffPeakbusWalk,paste0("unimodel/emmemat/mf",boardingsoffPeakbusWalk_matNum,".zmx"),path7zipexe)
      boardingsoffPeakbusWalk[boardingsoffPeakbusWalk > 9999] <- NA
      # Fix for boardings/transfers difference
      #boardingsoffPeakbusWalk[boardingsoffPeakbusWalk > 0 & !is.na(boardingsoffPeakbusWalk)] <- boardingsoffPeakbusWalk[boardingsoffPeakbusWalk > 0 & !is.na(boardingsoffPeakbusWalk)] - 1
      boardingsoffPeakbusWalk[!tAvailoffPeak,] <- boardingsoffPeakbusWalk[,!tAvailoffPeak] <- NA
      save(boardingsoffPeakbusWalk, file="inputs/RData/boardingsoffPeakbusWalk.RData")
      rm(boardingsoffPeakbusWalk)

############################ TRIP DISTRIBUTION INPUTS #####################################

      ##/
      #COLEBASE
      #College Vehicle Trips by zone from taz file
      #@return colveh - college vehicle trips by zone
      #Note that this vector should be scaled according to ITE rates below:
      #4 year college vehicle trips = students * 2.5 or staff * 9.8
      #2 year college vehicle trips = students * 1.5 or staff * 28.2
      ##/

      colveh <- c(rep(0, length(externalZones)), taz$COLEBASE)
      save(colveh, file="inputs/RData/colveh.RData")
      rm(colveh)

      ##/
      #SHSQFT.CSV
      #Shopping Center Square Footage
      #@return shsqft - shopping center square footage for each zone
      ##/

      shsqft <- read.csv("inputs/sqft.csv")
      shsqft <- tapply(shsqft$SQFT_BLDG,shsqft$TAZ,sum)
      shsqft <- shsqft[match(zoneNames,names(shsqft))][]
      shsqft[is.na(shsqft)] <- 0
      save(shsqft, file="inputs/RData/shsqft.RData")
      rm(shsqft)

      ##/
      #PARK ACRES
      #Park acres per zone from taz file
      #REPLICATED BY ROW SO IT IS A DESTINATION ZONE VALUE
      #@return parkAcres - Park acres per zone
      ##/

      parkAcres <- c(rep(0, length(externalZones)), taz$parkAcres)
      parkAcres <- matrix(parkAcres, length(parkAcres), length(parkAcres), byrow=T)
      save(parkAcres, file="inputs/RData/parkAcres.RData")
      rm(parkAcres)

      ##/
      #ZONAL EMPLOYMENT VECTORS
      #Zoneal employment from taz file
      #Replicated 0 before vector for external station numbers for all employment categories.
      #@return xxxEmp - current employment by zone
      #@return totalEmp - Sum all employment categories by zone
      ##/

      empVec <- c("AFREMP","MINEMP","CONEMP","MFGEMP","TCPEMP","WSTEMP","RETEMP","FINEMP","SVCEMP","GVTEMP")
      names(empVec)<- c("afrEmp","minEmp","conEmp","mfgEmp","tcpEmp","wstEmp","retEmp","finEmp","svcEmp","gvtEmp")

      for(curEmp in names(empVec)) {
          assign(curEmp,c(rep(0,length(externalZones)),taz[,empVec[curEmp]]))
          save(list=c(curEmp), file=paste("inputs/RData/",curEmp,".RData",sep=""))
      }

      totalEmp <- afrEmp + minEmp + conEmp + finEmp + gvtEmp + mfgEmp + retEmp + svcEmp + tcpEmp + wstEmp
      save(totalEmp, file="inputs/RData/totalEmp.RData")

      rm(totalEmp, afrEmp, minEmp, conEmp, finEmp, gvtEmp, mfgEmp, retEmp, svcEmp, tcpEmp, wstEmp)

########################## WALK ACCESSABILITY MEASURES BY ZONE ###############################

      ##/
      #INTERSECTIONS
      #2000 intersections w/i 0.5 mile of zone centroid from taz file
      #Does not include intersections with freeways or freeway ramps
      #@return inthm - 2000 intersections w/i 0.5 mile of zone
      ##/

      inthm <- c(rep(0, length(externalZones)), taz$intersections)
      save(inthm, file="inputs/RData/inthm.RData")
      rm(inthm)

      ##/
      #PERCENT SINGLE FAMILY
      #Percent Single Family by zone from taz file
      #Census ACS (Units in Structure) at the block group level
      #Does not include mobile homes (field 10) or rv,vans,etc (field 11)
      #@return percentSingleFamily - percent single family households by zone
      ##/

      percentSingleFamily <- taz$percentSingleFamilyDetached
      percentSingleFamily <- c(rep(0, length(externalZones)), percentSingleFamily)
      save(percentSingleFamily, file="inputs/RData/percentSingleFamily.RData")
      rm(percentSingleFamily)

      ##/
      #HOUSEHOLDS
      #Households by zone from taz file
      #@return hhs - 2000 households by zone
      ##/

      hhs <- c(rep(0, length(externalZones)), taz$HHBASE)
      save(hhs, file="inputs/RData/hhs.RData")
      rm(hhs)

########################## CENSUS CROSS TABULATION FROM PUMS ##########################

      ##/
      #HIAZ ARRAY
      #2000 household size, income group, age of head of HH market segments
      #hiaMtx is the the total number of households in each market segment combination of H, I, and A.
      #requires ipf() in JEMNRFunctions.R
      #@param: pumaXXX.csv
      #@return hiazAry - Array of Households by Income by AgeofHousholder by Zone
      ##/

      #Read and prep PUMA file
      puma <- read.csv(paste("inputs/",pumaFileName,".csv",sep=""))
      hiaAry <- array(puma$puma, c(4,4,4))
      dimnames(hiaAry) <- list(hs, hi, ha)
      hiazAry <- array(0, c(4,4,4,nrow(taz)))
      iterLst <- list()

      #Create 3way cross tab for zones
      for(i in 1:nrow(taz)) {
          temp <- ipf(cbind(unlist(taz[i,hs])*taz[i,"HHBASE"], unlist(taz[i,hi])*taz[i,"HHBASE"], unlist(taz[i,ha])*taz[i,"HHBASE"]), hiaAry)
          iterLst[[i]] <- attributes(temp)$iter
          hiazAry[,,,i] <- temp
      }
      cat(paste("Table of iterations taken for IPF closure"),"\n")
      print(table(unlist(iterLst)))

      extAry <- array(0, c(4,4,4,length(externalZones)))
      hiazAry <- array(c(extAry, hiazAry), c(4,4,4,length(zones)))
      dimnames(hiazAry) <- list(segmentsH,segmentsI,segmentsA,zoneNames)
      save(hiazAry, file="inputs/RData/hiazAry.RData")
      rm(hiazAry,iterLst)

########################## LOG DISTANCE, WALK, BIKE, WALK_TO_AUTO, AUTO, TRAVEL TIME MATRICES ##########################

      ##/
      #AUTO TRIP DISTANCE
      #Added to incorporate Ivan mode choice model
      ##/

      #setwd(paste(basedir, "\\", bankFolder, sep=""))


      autoDist <- readSelectedOMX(dailySkim, autoDistance_omxName)
      dimnames(autoDist) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(autoDist,paste0("unimodel/emmemat/mf",autoDistance_matNum,".zmx"),path7zipexe)
      #Set diagonal values
      diag(autoDist) <- apply(autoDist, 1, function(x) mean(sort(x[x>0&!is.na(x)])[1:4])) * IZFactA
      autoDist[autoDist>9999] <- NA
      #Save
      setwd(basedir)
      save(list=c("autoDist"),file=paste("inputs/RData/","autoDist",".RData",sep=""))

      ##/
      #TRANSIT TRIP DISTANCE
      #Added to incorporate Ivan mode choice model
      ##/

      #setwd(paste(basedir, "\\", bankFolder, sep=""))

      tranDist <- readSelectedOMX(dailySkim, tranDistance_omxName)
      dimnames(tranDist) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(tranDist,paste0("unimodel/emmemat/mf",tranDistance_matNum,".zmx"),path7zipexe)
      #Set diagonal values and Transit Availabilty
      diag(tranDist) <- NA
      tranDist[!tAvailoffPeak,] <- tranDist[,!tAvailoffPeak] <- NA
      tranDist[tranDist>9999] <- NA
      #Save
      setwd(basedir)
      save(list=c("tranDist"),file=paste("inputs/RData/","tranDist",".RData",sep=""))

      ## BIKE / WALK
      ## Move Bike/Walk distance skims from createTripDist

      # Skims from VISUM
      bikeDist <- readSelectedOMX(bikeskim, bikeDistance_omxName)
      walkDist <- readSelectedOMX(walkskim, walkDistance_omxName)
      dimnames(bikeDist) <- list(matZones$Lookup,matZones$Lookup)
      dimnames(walkDist) <- list(matZones$Lookup,matZones$Lookup)
      #Set diagonal values
      diag(bikeDist) <- apply(bikeDist, 1, function(x) mean(sort(x[x>0&!is.na(x)])[1:4])) * IZFactB
      diag(walkDist) <- apply(walkDist, 1, function(x) mean(sort(x[x>0&!is.na(x)])[1:4])) * IZFactW

      #output result
      writeZipMat(bikeDist,paste0("unimodel/emmemat/mf",bikeDistance_matNum,".zmx"),path7zipexe)
      writeZipMat(walkDist,paste0("unimodel/emmemat/mf",walkDistance_matNum,".zmx"),path7zipexe)

      save(list=c("bikeDist"),file="inputs/RData/bikeDist.RData")
      save(list=c("walkDist"),file="inputs/RData/walkDist.RData")

      ##/
      #LOG OF DIST TRIP
      #Base 10 log of the shortest path network distance
      #Added to incorporate Ivan mode choice model
      ##/

      #AUTO
      load("inputs/RData/autoDist.RData")
      lnAutoDist <- log(autoDist)
      save(lnAutoDist, file="inputs/RData/lnAutoDist.RData")

      #TRANSIT
      load("inputs/RData/tranDist.RData")
      lnTranDist <- log(tranDist)
      save(lnTranDist, file="inputs/RData/lnTranDist.RData")

      #WALK
      load("inputs/RData/walkDist.RData")
      lnWalkDist <- log(walkDist)
      save(lnWalkDist, file="inputs/RData/lnWalkDist.RData")

      #BIKE
      load("inputs/RData/bikeDist.RData")
      lnBikeDist <- log(bikeDist)
      save(lnBikeDist, file="inputs/RData/lnBikeDist.RData")

      ##/
      #WALK TRAVEL TIME MATRIX
      #Walk time is distance over network at 3mph
      #Read in distance matrix, divide by 3 and multiple by 60.
      #@param: walk Distance matrix
      #@return walkTimewalk
      #Replace times with NA for distances over 5 miles
      #Replace >9999 and !tAvail with NA (OD pair not available)
      ##/

      walkTimewalk <- walkDist
      walkTimewalk[walkTimewalk > maxWalkDistance] <- NA
      walkTimewalk <- walkTimewalk / walkSpeed * 60
      walkTimewalk[walkTimewalk > 9999] <- NA
      save(walkTimewalk, file="inputs/RData/walkTimewalk.RData")

      ##/
      #BIKE TRAVEL TIME MATRIX
      #bike time is bicycle travel time (network distance at 10 mph)
      #@param: walk Distance matrix
      #@return bikeTime
      #Replace times with NA for distances over 10 miles
      #Replace >9999 and !tAvail with NA (OD pair not available)
      ##/

      bikeTime <- bikeDist
      bikeTime[bikeTime > maxBikeDistance] <- NA
      bikeTime <- bikeTime / bikeSpeed * 60
      bikeTime[bikeTime > 9999] <- NA
      save(bikeTime, file="inputs/RData/bikeTime.RData")
      rm(bikeTime,walkTimewalk, tripDist,tripDistII,tripDistIII,lnTripDist,lnTripDistII,lnTripDistIII)

      ##/
      #WALK TO AUTO TRAVEL TIME MATRIX
      #A vector of auto walk time for each zone from taz file
      #FOR drivePass and pass modes we added 1 minute to walk time
      #@param externalStationAutoWalkTime - external station auto out-of-vehicle time
      #@param autoOVTime - auto out-of-vehicle time
      #@return walkTimedriveAlone - walk time for drive alone choice
      #@return walkTimedrivePass - walk time for drive with passenger choice
      #@return walkTimepass - walk time for passenger choice
      ##/

      autoOVTime <- c(rep(externalStationAutoWalkTime, length(externalZones)), taz$autoWalkTime)
      walkTimedriveAlone <- matrix(autoOVTime, length(autoOVTime), length(autoOVTime), byrow=T)
      walkTimedrivePass <- walkTimedriveAlone + drivePass_pass_extra_auto_walk_time
      walkTimepass <- walkTimedriveAlone + drivePass_pass_extra_auto_walk_time
      save(walkTimedriveAlone, file="inputs/RData/walkTimedriveAlone.RData")
      save(walkTimedrivePass, file="inputs/RData/walkTimedrivePass.RData")
      save(walkTimepass, file="inputs/RData/walkTimepass.RData")
      rm(autoOVTime, walkTimedriveAlone, walkTimedrivePass)

      ##/
      #AUTO TRAVEL TIME MATRIX
      #Read in peak and offPeak auto travel times and create inputs by auto modes
      #Depending on the names of the auto modes, as specified in modes, the autModes
      #vector used here might need to be edited.  By default the auto modes are
      #"driveAlone", "drivePass", and "pass".  Since this code section creates files
      #named ivTimepeak<autoMode> and ivTimeoffPeak<autoMode>, the autoMode name
      #must the same as the names in the modes object.
      #@param autoTimepeak - AM 2 hour auto travel times
      #@param autoTimeoffPeak - midday 1 hour auto travel times
      #@return ivTimepeak<autoMode> - peak in-vehicle time for <autoMode>
      #@return ivTimeoffPeak<autoMode> - offPeak in-vehicle time for <autoMode>
      #Calculate diagonal of matrix to 50% as the mean of the closest four values to the zone
      ##/

      autoModes <- c("driveAlone", "drivePass", "pass")
      autoTimeoffPeak <- readSelectedOMX(dailySkim, autoTimeoffPeak_omxName)
      dimnames(autoTimeoffPeak) <- list(matZones$Lookup,matZones$Lookup)
      autoTimepeak <- readSelectedOMX(peakSkim, autoTimepeak_omxName)
      dimnames(autoTimepeak) <- list(matZones$Lookup,matZones$Lookup)
      writeZipMat(autoTimeoffPeak,paste0("unimodel/emmemat/mf",autoTimeoffPeak_matNum,".zmx"),path7zipexe)
      writeZipMat(autoTimepeak,paste0("unimodel/emmemat/mf",autoTimepeak_matNum,".zmx"),path7zipexe)
      diag(autoTimepeak) <- apply(autoTimepeak, 1, function(x) mean(sort(x[x>0])[1:4])) * IZFactA
      diag(autoTimeoffPeak) <- apply(autoTimeoffPeak, 1, function(x) mean(sort(x[x>0])[1:4])) * IZFactA

      for(autoMode in autoModes) {
          #Peak
   	      assign(paste("ivTimepeak", autoMode, sep=""), autoTimepeak)
          save(list=paste("ivTimepeak", autoMode, sep=""), file=paste("inputs/RData/ivTimepeak", autoMode, ".RData", sep=""))
          rm(list=paste("ivTimepeak", autoMode, sep=""))

          #OffPeak
   	      assign(paste("ivTimeoffPeak", autoMode, sep=""), autoTimeoffPeak)
          save(list=paste("ivTimeoffPeak", autoMode, sep=""), file=paste("inputs/RData/ivTimeoffPeak", autoMode, ".RData", sep=""))
          rm(list=paste("ivTimeoffPeak", autoMode, sep=""))
      }
      rm(autoTimepeak, autoTimeoffPeak, autoMode, autoModes)

      ##/
      #AUTO TRAVEL TIME SQUARED
      #ivTimeoffPeakdriveAlone squared for hbsch distribution
      #@param ivTimeoffPeakdriveAlone - drive alone off peak time
      #@return ivTimeoffPeakdriveAloneSqrd - drive alone off peak time squared
      ##/

      load("inputs/RData/ivTimeoffPeakdriveAlone.RData")
      ivTimeoffPeakdriveAloneSqrd <- ivTimeoffPeakdriveAlone^2
      save(ivTimeoffPeakdriveAloneSqrd, file="inputs/RData/ivTimeoffPeakdriveAloneSqrd.RData")
      rm(ivTimeoffPeakdriveAlone, ivTimeoffPeakdriveAloneSqrd)

########################## AUTO OPERATING COSTS ##############################################

      ##/
      #SHORT AND LONG TERM PARKING COSTS
      #Long-term parking costs and short-term parking costs from the taz file
      #@param longTermParkCost - long term parking costs
      #@param shortTermParkCost - short term parking costs
      #@param autoOperCost - auto operating costs per mile
      #@param tripDist - auto trip distance matrix
      #@return opCostdriveAlonehbw - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonehbs - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonehbr - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonehbo - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonenhbw - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonenhbnw - out-of-pocket cost for drive alone choice
      #@return opCostdriveAlonehbcoll - out-of-pocket cost for drive alone choice
      #@return opCostdrivePasshbw - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePasshbs - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePasshbr - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePasshbo - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePassnhbw - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePassnhbnw - out-of-pocket cost for drive with passenger choice
      #@return opCostdrivePasshbcoll - out-of-pocket cost for drive with passenger choice
      #@return opCostpasshbw - out-of-pocket cost for passenger choice
      #@return opCostpasshbs - out-of-pocket cost for passenger choice
      #@return opCostpasshbr - out-of-pocket cost for passenger choice
      #@return opCostpasshbo - out-of-pocket cost for passenger choice
      #@return opCostpassnhbw - out-of-pocket cost for passenger choice
      #@return opCostpassnhbnw - out-of-pocket cost for passenger choice
      #@return opCostpasshbcoll - out-of-pocket cost for passenger choice
      ##/

      load("inputs/RData/autoDist.RData")
      longTermParkCost <- c(rep(0, length(externalZones)), taz$longTermParkingCost)
      shortTermParkCost <- c(rep(0, length(externalZones)), taz$shortTermParkingCost)

      #Define trip purposes
      tPurposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll")
      #Clean-up coefficient matrices
      for(purpose in tPurposes) {
          print(purpose)

          #LONG TERM PARKING COSTS and AUTO_OPERCOST
          #hbw and hbcoll are a function of long-term parking costs
          if(purpose == "hbw" | purpose == "hbcoll") {

              #driveAlone
              assign(paste("opCostdriveAlone", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * longTermParkCost))))
              save(list=paste("opCostdriveAlone", purpose, sep=""), file=paste("inputs/RData/opCostdriveAlone", purpose, ".RData", sep=""))
              rm(list=paste("opCostdriveAlone", purpose, sep=""))

              #drivePass
              assign(paste("opCostdrivePass", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * longTermParkCost))) * 0.5)
              save(list=paste("opCostdrivePass", purpose, sep=""), file=paste("inputs/RData/opCostdrivePass", purpose, ".RData", sep=""))
              rm(list=paste("opCostdrivePass", purpose, sep=""))

              #pass
              assign(paste("opCostpass", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * longTermParkCost))) * 0.5)
              save(list=paste("opCostpass", purpose, sep=""), file=paste("inputs/RData/opCostpass", purpose, ".RData", sep=""))
              rm(list=paste("opCostpass", purpose, sep=""))

          }

          #SHORT TERM PARKING COSTS and AUTO_OPERCOST
          #nhbw, nhbnw, hbr, hbs, and hbo are a function of short-term parking costs
          if(purpose%in%c("nhbw","nhbnw","hbr","hbs","hbo")) {

              #driveAlone
              assign(paste("opCostdriveAlone", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * shortTermParkCost))))
              save(list=paste("opCostdriveAlone", purpose, sep=""), file=paste("inputs/RData/opCostdriveAlone", purpose, ".RData", sep=""))
              rm(list=paste("opCostdriveAlone", purpose, sep=""))

              #drivePass
              assign(paste("opCostdrivePass", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * shortTermParkCost))) * 0.5)
              save(list=paste("opCostdrivePass", purpose, sep=""), file=paste("inputs/RData/opCostdrivePass", purpose, ".RData", sep=""))
              rm(list=paste("opCostdrivePass", purpose, sep=""))

              #pass
              assign(paste("opCostpass", purpose, sep=""),
              t(apply(autoOperCost * autoDist, 1, function(x) x+(0.5 * shortTermParkCost))) * 0.5)
              save(list=paste("opCostpass", purpose, sep=""), file=paste("inputs/RData/opCostpass", purpose, ".RData", sep=""))
              rm(list=paste("opCostpass", purpose, sep=""))
          }
      }
      rm(longTermParkCost, shortTermParkCost, tripDist, purpose, tPurposes)

########################## TRANSIT OPERATING COSTS ##############################################

      ##/
      #BUSWALK OPERATING COSTS
      #@param transitFare - transit fare for BusWalk choice
      #@return opCostbusWalk - Bus cost for BusWalk choice
      #Costs are for areas of trasits coverage for off peak
      ##/

      load("inputs/RData/districts.RData")
      transitFares = read.csv("inputs/transitFares.csv")
      opCostbusWalk <- matrix(0, numZones, numZones)
      for(i in 1:nrow(transitFares)) {
        	fromZones = districts$transitDistricts == transitFares$FromDistrict[i]
        	toZones = districts$transitDistricts == transitFares$ToDistrict[i]
        	opCostbusWalk[fromZones,toZones] <- transitFares$Fare[i]
      }
      opCostbusWalk[!tAvailoffPeak,] <- opCostbusWalk[,!tAvailoffPeak] <- NA
      save(opCostbusWalk, file="inputs/RData/opCostbusWalk.RData")
      rm(opCostbusWalk)

########################## PARK AND RIDE ######################################################

      ##/
      #PARK AND RIDE ZONE VECTOR
      #Park and Ride Lot Assignment vector from the taz file
      #Each zone is assigned a park and ride lot via closest spatial lot
      #@return parkAndRideLot - park and ride lot for each zone
      #@return parkAndRideLotIndex - park and ride lot for each zone
      #Calculate parkAndRideLotIndex from parkAndRideLot by converting from named zones to zone indexes for an unnamed matrix using districts
      ##/

      parkAndRideLot <- c(rep(0, length(externalZones)), taz$LOTZONE)
      parkAndRideLotIndex <- match(parkAndRideLot,districts$zone)
      save(parkAndRideLot, file="inputs/RData/parkAndRideLot.RData")
      rm(parkAndRideLot)

      ##/
      #PARK AND RIDE LOT OPERATING COSTS
      #Park And Ride out-of-pocket costs are bus operating cost + $0.091/mile * 0.25
      #@param opCostbusWalk - Bus cost for BusWalk choice
      #@param tripDist - - Auto trip distance
      #@return opCostparkAndRideBus - Auto and Bus cost for Park and Ride Bus choice
      ##/

      load("inputs/RData/autoDist.RData")
      load("inputs/RData/opCostbusWalk.RData")
      opCostparkAndRideBus <-  opCostbusWalk[parkAndRideLotIndex,] + (percentDriveForParkAndRideCost * autoOperCost * autoDist[,parkAndRideLotIndex])
      dimnames(opCostparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(opCostparkAndRideBus, file="inputs/RData/opCostparkAndRideBus.RData")
      rm(opCostparkAndRideBus, tripDist, opCostbusWalk)

      #SHADOW PRICING BY ZONE
      #price to limit park and ride demand to capacity
      #Applied in hbwAccess and hbwMode utilites from IVAN
      #@param shadow - shadow price by zone
      #@return shadow - shadow price by zone
      ##/

      shadow <- rep(0, length(zones))
      shadow <- matrix(shadow, length(shadow), length(shadow))
      save(shadow, file="inputs/RData/shadow.RData")
      rm(shadow)

########################## PEAK PARK AND RIDE TRAVEL TIME MATRICES ###########################

      ##/
      #AM PEAK PARK AND RIDE BUS FOR IN-VEHICLE TIME
      #@param ivTimepeakbusWalk - bus vehicle travel time
      #@param ivTimepeakdriveAlone - auto vehicle travel time
      #@param parkAndRideLotIndex - Travel time matrix row and column index for zones with a park and ride lot
      #@param tAvailpeak - Transit available zones
      #@return ivTimepeakparkAndRideBus - in-vehicle time for Auto and Bus for Park and Ride choice
      #If value > 9999 or !tAvail is TRUE, set all to NA (OD pair not available)
      #Changing the dimnames to be consistent with the origin and destination zones: Sriram Narayanamoorthy
      ##/

      load("inputs/RData/ivTimepeakbusWalk.RData")
      load("inputs/RData/ivTimepeakdriveAlone.RData")
      ivTimepeakparkAndRideBus <- ivTimepeakdriveAlone[,parkAndRideLotIndex] + ivTimepeakbusWalk[parkAndRideLotIndex,]
      ivTimepeakparkAndRideBus[!tAvailpeak,] <- ivTimepeakparkAndRideBus[,!tAvailpeak] <- NA
      ivTimepeakparkAndRideBus[ivTimepeakparkAndRideBus > 9999] <- NA
      dimnames(ivTimepeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(ivTimepeakparkAndRideBus, file="inputs/RData/ivTimepeakparkAndRideBus.RData")
      rm(ivTimepeakparkAndRideBus,ivTimepeakbusWalk,ivTimepeakdriveAlone)

      ##/
      #AM PEAK PARK AND RIDE BUS FOR WALK TO TRANSIT TIME
      #@param walkTimepeakbusWalk - walk to bus travel time
      #@param walkTimedriveAlone - walk to auto travel time
      #@param parkAndRideLotIndex - Travel time matrix row and column index for zones with a park and ride lot
      #@param tAvailpeak - Transit available zones
      #@param walkTimepeakparkAndRideBusCap - max wait
      #@return walkTimepeakparkAndRideBus - walk time for Auto and Bus for Park and Ride choice
      #If value > 9999 or !tAvail is TRUE, set all to NA (OD pair not available)
      #Adjust times to cap at 30 minutes
      ##/

      load("inputs/RData/walkTimepeakbusWalk.RData")
      load("inputs/RData/walkTimedriveAlone.RData")
      walkTimepeakparkAndRideBus <- walkTimedriveAlone[,parkAndRideLotIndex] + walkTimepeakbusWalk[parkAndRideLotIndex,]
      walkTimepeakparkAndRideBus[walkTimepeakparkAndRideBus > walkTimepeakparkAndRideBusCap & walkTimepeakparkAndRideBus < 9999] <- walkTimepeakparkAndRideBusCap
      walkTimepeakparkAndRideBus[walkTimepeakparkAndRideBus > 9999] <- NA
      walkTimepeakparkAndRideBus[!tAvailpeak,] <- walkTimepeakparkAndRideBus[,!tAvailpeak] <- NA
      dimnames(walkTimepeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(walkTimepeakparkAndRideBus, file="inputs/RData/walkTimepeakparkAndRideBus.RData")
      rm(walkTimepeakparkAndRideBus)


      ##/
      #AM PEAK PARK AND RIDE BUS WAIT TIME
      #@param waitTimeApeakparkAndRideBus - initial wait time for Park and Ride Bus choice
      #@param waitTimeBpeakbusWalk - initial wait time for Park and Ride Bus choice
      #@param pkWaitTimeACap - max wait for wait one
      #@param pkWaitTimeBCap - max wait for wait two
      #@param parkAndRideLotIndex - Travel time matrix row and column index for zones with a park and ride lot
      #@param tAvailpeak - Transit available zones
      #@return waitTimeApeakparkAndRideBus - initial wait time for Park and Ride Bus choice
      #@return waitTimeBpeakparkAndRideBus - wait time 2 for Park and Ride Bus choice
      #Adjust times to cap at 30 minutes
      #Replace >9999 and !tAvail with NA (OD pair not available)
      ##/

      load("inputs/RData/waitTimeApeakbusWalk.RData")
      load("inputs/RData/waitTimeBpeakbusWalk.RData")

      #FIRST WAIT
      waitTimeApeakparkAndRideBus <- waitTimeApeakbusWalk[parkAndRideLotIndex,]
      waitTimeApeakparkAndRideBus[waitTimeApeakparkAndRideBus > pkWaitTimeACap & waitTimeApeakparkAndRideBus < 9999] <- pkWaitTimeACap
      waitTimeApeakparkAndRideBus[waitTimeApeakparkAndRideBus > 9999] <- NA
      waitTimeApeakparkAndRideBus[!tAvailpeak,] <- waitTimeApeakparkAndRideBus[,!tAvailpeak] <- NA
      dimnames(waitTimeApeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(waitTimeApeakparkAndRideBus, file="inputs/RData/waitTimeApeakparkAndRideBus.RData")

      #SECOND WAIT
      waitTimeBpeakparkAndRideBus <- waitTimeBpeakbusWalk[parkAndRideLotIndex,]
      waitTimeBpeakparkAndRideBus[waitTimeBpeakparkAndRideBus > pkWaitTimeBCap & waitTimeBpeakparkAndRideBus < 9999] <- pkWaitTimeBCap
      waitTimeBpeakparkAndRideBus[waitTimeBpeakparkAndRideBus > 9999] <- NA
      waitTimeBpeakparkAndRideBus[!tAvailpeak,] <- waitTimeBpeakparkAndRideBus[,!tAvailpeak] <- NA
      dimnames(waitTimeBpeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(waitTimeBpeakparkAndRideBus, file="inputs/RData/waitTimeBpeakparkAndRideBus.RData")

      rm(waitTimeApeakbusWalk,waitTimeBpeakbusWalk,waitTimeApeakparkAndRideBus,waitTimeBpeakparkAndRideBus)

      ##/
      #AM peak Park and Ride Bus boardings
      #@param boardingspeakparkAndRideBus - number of boardings for trip for Park and Ride Bus choice
      #@return boardingspeakparkAndRideBus - number of boardings for trip for Park and Ride Bus choice
      #Replace >9999 and !tAvail with NA (OD pair not available)
      #Subtract one from number of boardings
      ##/

      load("inputs/RData/boardingspeakbusWalk.RData")
      boardingspeakparkAndRideBus <-  boardingspeakbusWalk[parkAndRideLotIndex,]
      boardingspeakparkAndRideBus[boardingspeakparkAndRideBus > 9999] <- NA
      boardingspeakparkAndRideBus[!tAvailpeak,] <- boardingspeakparkAndRideBus[,!tAvailpeak] <- NA
      dimnames(boardingspeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(boardingspeakparkAndRideBus, file="inputs/RData/boardingspeakparkAndRideBus.RData")
      rm(boardingspeakbusWalk, boardingspeakparkAndRideBus)


######################## OFFPEAK PARK AND RIDE TRAVEL TIME MATRICES ###########################

      #OFF PEAK PARK AND RIDE IS JUST A COPY OF PEAK RARK AND RIDE

      ##/
      #MIDDAY OFFPEAK PARK AND RIDE BUS FOR IN-VEHICLE TIME
      #@param ivTimepeakparkAndRideBus - bus vehicle travel time
      #@return ivTimeoffPeakparkAndRideBus - in-vehicle time for Auto and Bus for Park and Ride choice
      ##/

      load("inputs/RData/ivTimepeakparkAndRideBus.RData")
      ivTimeoffPeakparkAndRideBus <-  ivTimepeakparkAndRideBus
      dimnames(ivTimeoffPeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(ivTimeoffPeakparkAndRideBus, file="inputs/RData/ivTimeoffPeakparkAndRideBus.RData")
      rm(ivTimeoffPeakparkAndRideBus, ivTimepeakparkAndRideBus)

      ##/
      #MIDDAY OFFPEAK PARK AND RIDE BUS FOR WALK TO TRANSIT TIME
      #@param walkTimeoffPeakparkAndRideBus - walk to bus travel time
      #@return walkTimeoffPeakparkAndRideBus - walk time for Auto and Bus for Park and Ride choice
      ##/

      load("inputs/RData/walkTimepeakparkAndRideBus.RData")
      walkTimeoffPeakparkAndRideBus <-  walkTimepeakparkAndRideBus
      dimnames(walkTimeoffPeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(walkTimeoffPeakparkAndRideBus, file="inputs/RData/walkTimeoffPeakparkAndRideBus.RData")
      rm(walkTimeoffPeakparkAndRideBus, walkTimepeakparkAndRideBus)

      ##/
      #MIDDAY MIDDAY PARK AND RIDE BUS WAIT TIME
      #@param waitTimeApeakparkAndRideBus - initial wait time for Park and Ride Bus choice
      #@param waitTimeBpeakparkAndRideBus - initial wait time for Park and Ride Bus choice
      #@return waitTimeAoffPeakparkAndRideBus - initial wait time for Park and Ride Bus choice
      #@return waitTimeBoffPeakparkAndRideBus - wait time 2 for Park and Ride Bus choice
      ##/

      #FIRST WAIT
      load("inputs/RData/waitTimeApeakparkAndRideBus.RData")
      waitTimeAoffPeakparkAndRideBus <- waitTimeApeakparkAndRideBus
      dimnames(waitTimeAoffPeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(waitTimeAoffPeakparkAndRideBus, file="inputs/RData/waitTimeAoffPeakparkAndRideBus.RData")
      rm(waitTimeAoffPeakparkAndRideBus, waitTimeApeakparkAndRideBus)

      #SECOND WAIT
      load("inputs/RData/waitTimeBpeakparkAndRideBus.RData")
      waitTimeBoffPeakparkAndRideBus <-  waitTimeBpeakparkAndRideBus
      dimnames(waitTimeBoffPeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(waitTimeBoffPeakparkAndRideBus, file="inputs/RData/waitTimeBoffPeakparkAndRideBus.RData")
      rm(waitTimeBoffPeakparkAndRideBus, waitTimeBpeakparkAndRideBus)

      ##/
      #AM peak Park and Ride Bus boardings
      #@param boardingspeakparkAndRideBus - number of boardings for trip for Park and Ride Bus choice
      #@return boardingsoffPeakparkAndRideBus - number of boardings for trip for Park and Ride Bus choice
      ##/

      load("inputs/RData/boardingspeakparkAndRideBus.RData")
      boardingsoffPeakparkAndRideBus <-  boardingspeakparkAndRideBus
      dimnames(boardingsoffPeakparkAndRideBus) <- list(matZones$Lookup,matZones$Lookup)
      save(boardingsoffPeakparkAndRideBus, file="inputs/RData/boardingsoffPeakparkAndRideBus.RData")
      rm(boardingsoffPeakparkAndRideBus, boardingspeakparkAndRideBus)


########################################## END ################################################################################
