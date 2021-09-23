      #/
      #@filename inputs.R
      #@author Ben Stabler, benjamin.stabler@odot.state.or.us
      #@version 1.4
      #@date 8/12/04
      #
      #Preprocess all model inputs that are not saved but stored in memory.
      #Inputs stored in memory include simple objects such as titles, 
      #scalars, vectors, and coefficient matrices.
      #
      #Changed peakFactorHbw to hbwPeakFactor for easier coding when
      #pasting purpose and peakFactor for all purposes.
      #3/30/04 Ben Stabler, revised to read in utility defintion files and market segment coefficient files for mode choice.
      #8/12/04 Ben Stabler, added external model inputs, special generators, 
      #Trip generation calibration factors and school model inputs
      #10/31/05 Brian Gregor, added hbsroVisitorCalibrationFactor and nhbnwVisitorCalibrationFactor variables. These adjust the rated of trip making by visitors in these categories compared to the standard rate for resident households.
      #03/16/06 Brian Gregor, added trip distribution scaling factors
      #02/04/08 Alex Bettinardi, removed trip distribution scaling factors
      #02/04/08 Alex Bettinardi, adding in Metro's trip production calibration factors
      # BTS 03/14/12 - multiple chages to update JEMnR to work with a settings.csv file
      #Added variable dictionary and utility definition file variable
      #name checks via verifyVarName function.
      # 5/14/2014 Yegor Malinovskiy - added TOD_periods creation for commercial vehicle model  
      # 10/21/2014 Martin Mann:
      # 1 Moved vector of names for census taz distributions from inputsSave.r
      # 2 Removed the code provided by PB to process the JEMNR TAZ file with code that modifies the TAZ file and PUMA file using the results of the Population Synthesizer.  It makes adjustments for students if running the University model.
      # 3 Added reading of Trip Generation boosting file to allow selective TripProdCalibrationFactors  by district.  Removed the TripProdCalibrationFactors  from settings file
      # 4 Fully implemented reading and saving of TAZ and Districts files from inputsSave.r  to inputs.r
      # 11/10/2014 - Martin Mann - revised Population Processing for the Univeristy model - handeled in seperate process    
      # 01/13/15 - Martin Mann - Code reformatting and removing of old code no longer used 
      # 03/30/16 - Martin Mann - Removed code to create percenatages in distributions if found to be HH
      #1/17/17 - Martin Mann - Added a read to file taz.census.csv if exists for university model runs
#NOTES:
########################## PREGENERATION INPUTS ####################################
      
      ##/DEFINE SOME VECTORS
      
      #MODE DEFINITIONS
      #These are the modes available for calculating accessibility and mode choice.  
      #Although walk access transit and park and ride access transit can have up to
      #three (bus, lrt, transit (both)) choices, only one of each is available in 
      #mode choice
      #hbsch (home-based school) mode must be enter in modes.csv as well.
      #
      #This input file has three columns and a row for each mode in the model.
      #The first column is the mode name - it is used to make sure that an input 
      #utility definition for a mode is a mode that is in the model (check typos)
      #The second column is the mode type - these are the types of modes that are 
      #allowed (vehicle, transit, pandr, other).  The values in this column must 
      #be from that list.  These are used in the mode choice when building choice 
      #sets for the applicable trips between zones accounting for transit and 
      #park and ride coverage factors.  The third column is the class - it is 
      #the class that this mode is collapsed to and that is written out for 
      #assignment.  The names is this column can be anything and will be the 
      #name of the output demand matrix - the one exception is "other" which 
      #is reserved for modes that are not assigned.  Separate directional factors 
      #by purpose are input for each class.
      
      ##/PREGENERATION INPUTS
      
      #MARKET SEGMENTS 
      #Used to create segmentsDf, which is used for indexing 
      #specific segments throughout the model.
      #All class numbers are category values not actual values so w1 is class one of worker, which is actually 0 workers
      #@return segmentsH - household size market segments
      #@return segmentsI - household income market segments
      #@return segmentsA - age of head of household market segments
      #@return segmentsW - number of workers in household market segments
      #@return segmentsH - household size market segments
      #@return segmentsI - household income market segments
      #@return segmentsA - age of head of household market segments
      #@return segmentsW - number of workers in household market segments
      #@return numH - number of household size market segments
      #@return numI - number of household income market segments
      #@return numA - number of age of head of household market segments
      #@return numW - number of number of workers in household market segments
      ##/
      
      #WORKER  COEFFICIENT MATRIX
      #The worker coefficients matrix is a spreadsheet of
      #rows representing worker classes by columns representing 
      #coefficients for each of the classes of the variables that
      #the worker generation model is a function of.
      #@param workerCoeffMtx - Worker model coefficient matrix
      #@return workerCoeffMtx - Worker model coefficient matrix
      ##/

      ##/
      #CAR OWNERSHIP COEFFICIENT MATRIX
      #The car ownership coefficients matrix is a spreadsheet of
      #rows representing car ownership classes by columns representing 
      #coefficients for each of the classes of the variables that
      #the car generation model is a function of.
      #@param carCoeffMtx - Car Ownership Coefficient Matrix
      #@return carCoeffMtx - Car Ownership Coefficient Matrix
      ##/

      ##/
      #CHILD COEFFICIENT MATRIX
      #The kid coefficients matrix is a spreadsheet of
      #rows representing kid classes by columns representing 
      #coefficients for each of the classes of the variables that
      #the kid generation model is a function of.
      #@param kidCoeffMtx - child Coefficient Matrix
      #@return kidCoeffMtx - child Coefficient Matrix
      ##/
     
      ##/TRIP GENERATION INPUTS
      
      #PREPROCESS GENERATION RATE MATRICES BY TRIP PURPOSE
      #Read in a trip generation rate matrix for each purpose.
      #Format the input and save to the workspace. 
      #Each input file is a spreadsheet of rows representing the classes of one of the 
      #dependent variables and columns representing the other dependent variable if applicable.

      ##/ MULTIMODAL INPUTS

      #PREPROCESS ACCESSIBILITY UTILTIY DEFINTION CSV FILES BY TRIP PURPOSE
      #Read in a multimodal accessibility utility definitions
      #for each purpose.  Format the result and save to the workspace.
      #Also checks that variables are defined in the variable dictionary and
      #modes are defined in the modes vector.
      #@param <purpose>AccessUtils.csv - accessibility utility definition for <purpose>
      #@return <purpose>AccessUtils.csv - accessibility utility definition for <purpose>
      ##/

      ##/ TRIP DISTRIBUTION INPUTS

      #DISTRIBUTION COEFFICIENT MATRIX
      #distUtils.csv is read in
      #The first column (purpose) is the purpose and 
      #the second column (utility) is the utility definition
      #Scaling factors for the impedance (logsum) are appended to the equations
      #Note that the remaining employment variable must be
      #labeled remainingEmp and all employment categories must
      #follow the employment names convention.
      #Checks to verify variable names in variable dictionary as well
      #The utilities are written out in same for to inputs/distUtils.csv
      #@param distUtils - Reference Distribution Utility Definitions
      #@param scalingFactors - scaling factors
      #@return distUtils - Distribution Utility Definitions
      ##/
     
      #NHB(WORK AND NON-WORK) PRODUCTION UTILITY DEFINITIONS
      #@param nhbProdUtils - nhb Production Utility Definitions
      #@return nhbProdUtils - nhb Production Utility Definitions
      #The first column (purpose) is the purpose and 
      #the second column (utility) is the utility definition
      #There should be two rows (nhbw and nhbnw)
      #Checks to verify variable names in variable dictionary as well 
   
      ##/ MODE CHOICE INPUTS

      #PREPROCESS MODE CHOICE UTILTIY DEFINTION CSV FILES BY TRIP PURPOSE
      #Read in a mode choice utility definitions for each purpose.
      #Format the result and save to the workspace.
      #Also checks that variables are defined in the variable dictionary and
      #modes are defined in the modes vector.
      #nhbw and nhbnw are not by market segment but must still input a matrix of zero coefficients.
      #@param <purpose>ModeUtils.csv - mode choice utility definition for <purpose>
      #@return <purpose>ModeUtils.csv - mode choice utility definition for <purpose>
      #@param <purpose>MarketSegments.csv - mode choice market segment coefficients for <purpose>
      #@return <purpose>MarketSegments.csv - mode choice market segment coefficients for <purpose>
      ##/

      ##/
      #HBSCH MODE CHOICE PERCENTS
      #Read in a mode choice percents for hbsch. 
      #hbsch's mode choice percents is actually percent of trips by mode
      #for each input district for each school type. The input matrix is 
      #a spreadsheet of rows representing districts and columns representing modes.
      #The column names or modes must be input to the modes input table.
      #The rows should sum to one and a zero should be entered for 
      #modes not available for a given district.  The district names must be 
      #the same as the district codes in the districts csv file.
      #The first column is the district code and the scond column is the
      #school code (type - 1 = elem, 2 = middle, 3 = high)
      #@param hbschModeChoicePercents - hbsch Mode Choice Percents
      #@return hbschModeChoicePercents - hbsch Mode Choice Percents
      ##/
      
      #PEAKING INPUTS
      #These input factor csv files represent the assignment period
      #factors by purpose for each assignment class identified in modes.csv
      #There must be an input <class>PeriodFactors.csv file for each
      #class in modes.csv.  The class portion name of the <class>PeriodFactors
      #filename must the same as the class name identified in modes.csv. So
      #if a class is pandrBus then the file must be pandrBusPeriodFactors and
      #the resulting R object must be pandrBusPeriodFactors.
      #/
    
      ##/
      #VEHICLE PEAKING FACTOR
      #vehicle peaking factors are P->A and A->P period factors
      #for each trip purpose.  These factors represent the percent of
      #daily trips accounted for by that purpose and direction.  So an
      #input value of 0.05 for hbwpa for period "amPeak" means that
      #hbw P->A for period "amPeak" accounts for 5% of the daily hbw trips.
      #The input format is as follows:
      #The columns represent assignment periods and the rows are trip 
      #purposes AND and "ap" or "pa" attached to the end of the name to
      #represent the direction.
      #@param vehiclePeriodFactors - vehicle period factors
      #@return vehiclePeriodFactors - vehicle period factors
      ##/
    
      ##/
      #PARK AND RIDE VEHICLE PEAKING FACTOR
      #vehicle peaking factors are P->A and A->P period factors
      #for each trip purpose.  These factors represent the percent of
      #daily drive leg of transot trips accounted for by that purpose and direction.
      #The input format is as follows:
      #The columns represent assignment periods and the rows are trip 
      #purposes AND and "ap" or "pa" attached to the end of the name to
      #represent the direction.
      #@param pandrVehiclePeriodFactors - park and ride drive leg vehicle period factors
      #@param pandrVehiclePeriodFactors - park and ride drive leg vehicle period factors
      ##/

      ##/
      #BUS PEAKING FACTOR
      #bus peaking factors are P->A and A->P period factors
      #for each trip purpose.  These factors represent the percent of
      #daily trips accounted for by that purpose and direction.  So an
      #input value of 0.05 for hbwpa for period "amPeak" means that
      #hbw P->A for period "amPeak" accounts for 5% of the daily hbw trips.
      #The input format is as follows:
      #The columns represent assignment periods and the rows are trip 
      #purposes AND and "ap" or "pa" attached to the end of the name to
      #represent the direction.
      #@param busPeriodFactors - bus period factors
      #@return busPeriodFactors - bus period factors
      ##/
 
      ##/
      #PARK AND RIDE BUS PEAKING FACTOR
      #park and ride bus peaking factors are P->A and A->P period factors
      #for each trip purpose.  These factors represent the percent of
      #daily trips accounted for by that purpose and direction.  So an
      #input value of 0.05 for hbwpa for period "amPeak" means that
      #hbw P->A for period "amPeak" accounts for 5% of the daily hbw trips.
      #The input format is as follows:
      #The columns represent assignment periods and the rows are trip 
      #purposes AND and "ap" or "pa" attached to the end of the name to
      #represent the direction.
      #@param pandrBusPeriodFactors - park and ride bus period factors
      #@return pandrBusPeriodFactors - park and ride bus period factors
      ##/     
########################## SET-UP FOLDERS ####################################
     
      cat("Read in common and basic inputs\n\n")
      
      #/
      # Remove all previous Rdata to ensure that this model is run with
      # the correct data
      #/
    
      createdByCode <- c("access","modec", "peaking", "pregen", "tripdist", "tripgen")
    
      for(Dir in createdByCode){
          if(file.exists(Dir)) {
              allinDir <- list.files(paste(Dir, "/",sep=""))
              if(length(allinDir) > 0) {
                  for(File in allinDir) {
                      if(!file.remove(paste(Dir,File,sep="/"))) {
                          file.remove(paste(Dir,File,list.files(paste(Dir,File,sep="/")),sep="/"))
                          allinDir <- list.files(paste(Dir, File, sep="/"))
                          if(length(allinDir) > 0) {
                              for(File2 in allinDir) {
                                  if(!file.remove(paste(Dir,File,File2,sep="/"))) {
                                      #file.remove(paste(Dir,File,File2,list.files(paste(Dir,File,File2,sep="/")),sep="/"))
                                  } # end of if
                              } # end of dir
                          } # end of if
                      } # end of if
                  } # end of dir
              } # end of if
          }   # end of if
      }  # end of for

########################## DEFINE SOME VECTORS ####################################

      ##/
      #Trip Purposes
      #trip purposes except hbsch since it is generally different than the others
      ##/
      
      purposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll")
    
      ##/
      #Mode Definitions
      ##/
     
      modes <- read.csv("inputs/utilities/modes.csv")
    
      ##/
      #Variable Dictionary
      #This file contains all the variables that are used in the utility definitions
      #It also allows for descriptions to be input for each variable
      ##/
      
      varDictionary <- read.csv("inputs/variableDictionary.csv")
      
      ##/
      #Mode Calibration Constants Dictionary
      #This file contains all the constants that are used in the Accessibitily and mode choice utilities.
      #The table is by purpose and mode
      ##/
            
      modeCalib <- read.csv("inputs/modeCalib.csv",row.names=1)
    
########################## PREGENERATION INPUTS ####################################

      #/
      #Market Segments
      #Used to create segmentsDf, which is used for indexing 
      ##/
    
      hs <- c("HHS1BASE","HHS2BASE","HHS3BASE","HHS4BASE")
      hi <- c("HHI1BASE","HHI2BASE","HHI3BASE","HHI4BASE")
      ha <- c("AGE1BASE","AGE2BASE","AGE3BASE","AGE4BASE")
    
      segmentsH <- c("h1","h2","h3","h4")
      segmentsI <- c("i1","i2","i3","i4")
      segmentsA <- c("a1","a2","a3","a4")
      segmentsW <- c("w1","w2","w3","w4")
    
      numH <- length(segmentsH)
      numI <- length(segmentsI)
      numA <- length(segmentsA)
      numW <- length(segmentsW)
    
      ##/
      #Child Model Classes
      ##/
      
      segmentsK <- c("k1","k2","k3","k4")
      numK <- length(segmentsK)
    
      ##/
      #Car Ownership Classes
      ##/
      
      segmentsC <- c("c1","c2","c3","c4")
      numC <- length(segmentsC)    
    
      ##/
      #Worker model coefficient matrix
      ##/
      
      workerCoeffMtx <- read.csv("inputs/utilities/workerCoeffMtx.csv")
   
      ##/
      #Car Ownership Coefficient Matrix
      ##/
      
      carCoeffMtx <- read.csv("inputs/utilities/carCoeffMtx.csv")

      ##/
      #Child Coefficient Matrix
      ##/
      
      kidCoeffMtx <- read.csv("inputs/utilities/kidCoeffMtx.csv")
    
########################## TRIP GENERATION INPUTS #######################################

      ##/
      #Preprocess Generation Rate Matrices by Trip Purpose
      ##/    
      
      tPurposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll","hbsch")
      #Clean-up coefficient matrices
      for(purpose in tPurposes) {
          gMat <- read.csv(paste("inputs/utilities/", purpose, "GenRates.csv", sep=""),row.names=1)
          assign(paste(purpose, "GenRatesMtx", sep=""), gMat)
      }
      rm(gMat, purpose, tPurposes)

########################## MULTIMODAL INPUTS #######################################

      ##/
      #Preprocess Accessibility Utiltiy Defintion CSV files by Trip Purpose
      ##/
      
      #Define trip purposes
      tPurposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll")
      #Clean-up coefficient matrices
      for(purpose in tPurposes) {
          #Add Constants
          cMat <- readUtils(paste("inputs/utilities/", purpose, "AccessUtils.csv", sep=""))      
          for(cName in names(cMat)) cMat[cName] <- paste(modeCalib[cName,purpose],cMat[cName], sep=" +")
          assign(paste(purpose, "AccessUtils", sep=""), cMat) 
          verifyVarName(paste(purpose, "AccessUtils", sep=""), varDictionary)
          verifyModeName(paste(purpose, "AccessUtils", sep=""), modes)
      }
      rm(cMat, purpose, tPurposes)    

########################## TRIP DISTRIBUTION INPUTS ##################################
    
      ##/
      #Distribution Coefficient Matrix
      ##/
      
      distUtils <- readUtils("inputs/utilities/distUtils.csv")
      verifyVarName("distUtils",varDictionary)
      #Define factors for scaling the impedence terms of the utilities
      scalingFactors <- c("hbwlowInc" = as.vector(scalingFactors_hbwlowInc),
                        "hbwmidInc" = as.vector(scalingFactors_hbwmidInc),
                        "hbwhighInc" = as.vector(scalingFactors_hbwhighInc),
                        "hbs" = as.vector(scalingFactors_hbs),
                        "hbr" = as.vector(scalingFactors_hbr),
                        "hbo" = as.vector(scalingFactors_hbo),
                        "nhbw" = as.vector(scalingFactors_nhbw),
                        "nhbnw" = as.vector(scalingFactors_nhbnw))
      #Add (by pasting) the scaling factor to the utilities
      distUtilsNames <- names(distUtils)
      distUtils <- paste(scalingFactors, distUtils, sep=" * ")
      names(distUtils) <- distUtilsNames
      rm(distUtilsNames)
    
      ##/
      #nhb(work and non-work) Production Utility Definitions
      ##/
      
      nhbProdUtils <- readUtils("inputs/utilities/nhbProdUtils.csv")
      verifyVarName("nhbProdUtils", varDictionary)
    
      ##/
      #home-based school distribution utility definition
      ##/
      
      hbschDistUtils <- readUtils("inputs/utilities/hbschDistUtils.csv")
      
      ##/
      #Add a Trip Generation Boosting by districts
      #Districts taken from "ga" field in districts.csv
      ##/
        
      #tripBoost <- read.csv("inputs/tripGenBoosting.csv")
      #rownames(tripBoost) <-  tripBoost$Dist
       
########################## MODE CHOICE INPUTS #######################################

      ##/
      #Preprocess Mode Choice Utiltiy Defintion CSV files by Trip Purpose
      ##/
      
      #Define trip purposes
      tPurposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll")
      #Loop through each purpose and add the appropriate constant by mode
      for(purpose in tPurposes) {
          #Add Constants
          cMat <- readUtils(paste("inputs/utilities/", purpose, "ModeUtils.csv", sep=""))
          for(cName in names(cMat)) cMat[cName] <- paste(modeCalib[cName,purpose],cMat[cName], sep=" +")
          assign(paste(purpose, "ModeUtils", sep=""), cMat) 
          
          #Verify variables
          verifyVarName(paste(purpose, "ModeUtils", sep=""), varDictionary)
          verifyModeName(paste(purpose, "ModeUtils", sep=""), modes)
           
          #Market Segment Coefficients
          cMat <- readCoeffs(paste("inputs/utilities/", purpose, "MarketSegments.csv", sep=""))
          assign(paste(purpose, "MarketSegments", sep=""), cMat)
      }
      rm(cMat, purpose, tPurposes, scalingFactors)
    
      ##/
      #hbsch Mode Choice Percents
      ##/
      
      hbschModeChoicePercents <- read.csv("inputs/hbschModeChoicePercents.csv")
    
      ##/
      #vehicle peaking factor
      ##/
      
      vehiclePeriodFactors <- read.csv("inputs/vehiclePeriodFactors.csv")
      rownames(vehiclePeriodFactors) <- vehiclePeriodFactors$purpose
      vehiclePeriodFactors <- vehiclePeriodFactors[,-1]
    
      ##/
      #park and ride vehicle peaking factor
      ##/
      
      pandrVehiclePeriodFactors <- read.csv("inputs/pandrVehiclePeriodFactors.csv")
      rownames(pandrVehiclePeriodFactors) <- pandrVehiclePeriodFactors$purpose
      pandrVehiclePeriodFactors <- pandrVehiclePeriodFactors[,-1]
    
      ##/
      #bus peaking factor
      ##/      
      
      busPeriodFactors <- read.csv("inputs/busPeriodFactors.csv")
      rownames(busPeriodFactors) <- busPeriodFactors$purpose
      busPeriodFactors <- busPeriodFactors[,-1]
    
      ##/
      #park and ride bus peaking factor
      ##/      
      
      pandrBusPeriodFactors <- read.csv("inputs/pandrBusPeriodFactors.csv")
      rownames(pandrBusPeriodFactors) <- pandrBusPeriodFactors$purpose
      pandrBusPeriodFactors <- pandrBusPeriodFactors[,-1]
      
      
########################## COMMERCIAL VEHICLE MODEL INPUTS ###########################
      
      #Time of Day Periods for Commercial Vehicle Model
      if(as.logical(runCommercialVehicleModel)) {
   	      TOD_periods  <- read.csv("inputs/TOD_Periods.csv", header=T, as.is=T)
      }      
      
########################## EXTERNAL MODEL INPUTS #######################################
      
      #Differing set of inputs depending on which external model you are running
      
      #SWIM EXTERNALS      
      if(as.logical(externalModelFromSWIM)) {           
          ##/
          #SWIM time-of-day periods for external model          
          ##/
          
          TOD_periods  <- read.csv("inputs/TOD_Periods.csv", header=T, as.is=T)
          #ensure that daily is a period
          if(sum("daily" %in% TOD_periods$Period) == 0) TOD_periods <- rbind(TOD_periods, list("daily", 0, 2359, "all times of the day"))
        
          ##/
          #External model input data
          #For SWIM subarea model columns are STATIONNUMBER, AutoAADT, TruckAADT, AADT_YEAR, GrowthRate, and all applicable period factor columns
          #The stations should be sorted from lowest to highest
          #This input is used to adjust SWIM external model trends to known or predicted station volumes
          #@param externals - selectLinks.csv input table
          #@return externals - externals input table
          ##/
          
          externals <- read.csv("inputs/externalModel/selectLinks.csv")
          #Match the externals object to the TOD periods available       
          pCols <- unlist(sapply(TOD_periods$Period, function(x) grep(paste("^",x,sep=""), colnames(externals))))
          colnames(externals)[pCols] <- names(pCols)
          externals <- externals[,c("STATIONNUMBER", "DIRECTION", "AutoAWDT", "TruckAWDT", "AWDT_YEAR", names(pCols), "GrowthRate")]
          externals <- externals[order(externals$STATIONNUMBER,externals$DIRECTION),]
          colnames(externals)[1] <- "station"        

          ##/
          #SWIM zones to JEMnR model zones crosswalk
          #@param Crosswalk - SWIM zones to JEMnR model zones crosswalk
          #@return Crosswalk - SWIM zones to JEMnR model zones crosswalk
          ##/
          
          Crosswalk <- read.csv("inputs/externalModel/SWIM_JEMnR_TAZ_CW.csv")
        
          ##/
          #SWIM zones that define the JEMnR subarea
          #@param SWIM_subarea_zones - SWIM zones that define the JEMnR subarea
          #@return SWIM_subarea_zones - SWIM zones that define the JEMnR subarea
          ##/
          
          SWIM_subarea_zones  <- read.csv("inputs/externalModel/InternalZones.txt", header=F)        
      }  
      
      #LEGACY EXTERNALS
      if(!as.logical(externalModelFromSWIM)) {
          ##/
          #External model period defintions and factors
          #The external model adds an external OD matrix to the vehicle class
          #identified in modes.  The first column is the external assignment period
          #name.  The second is the source input external matrix to build the
          #period external matrix from.  This is the name of the matrix saved to
          #an RData file in inputsSave.R.  The third column is the factor to apply 
          #to the input OD matrix to get the assignment period matrix.  Note that
          #the external trips are added in OD form - so they are not transposed
          #and added together
          #The assignment periods should correspond to the assignment periods in 
          #the vehicle period factors input table since the external vehicle trips
          #are added to the vehicle matrices.
          #@param externalPeriodFactors - externalPeriodFactors.csv input table
          #@return externalPeriodFactors - externalPeriodFactors.csv input table
          ##/
          
          externalPeriodFactors <- read.csv("inputs/externalPeriodFactors.csv")
        
          ##/
          #External model input data
          #Columns are station, ADT, ei.pct, and ie.pct
          #The stations must be sorted from lowest to highest
          #This input is used to calculate EI and IE trip generation and distribution
          #@param externals - externals.csv input table
          #@return externals - externals input table
          ##/
          
          externals <- read.csv("inputs/externals.csv")
        
          ##/
          #EE distribution matrix
          #@param eeDist - ee distribution matrix
          #@return eeDist - ee distribution matrix
          ##/
         
          eeDist <- read.csv("inputs/eeDist.csv")
          colnames(eeDist) <- gsub("X","",colnames(eeDist))
          eeDist <- as.matrix(eeDist)
          if(!file.exists("inputs/RData")) dir.create("inputs/RData") #AB
          save(eeDist, file="inputs/RData/eeDist.RData")        
      } 
    
########################## LAND USE INPUTS ####################################
      
      ##/
      #DISTRICTS.CSV
      #@return zones - vector from 1 to number of zones
      #@return numZones - number of zones in model
      #@return externalZones - names of the external zones in model
      #@return zoneNames - names of the al zones in model
      ##/
      
      districts <- read.csv("inputs/districts.csv")
      districts <- districts[order(districts$zone),]
      zones <- 1:nrow(districts)
      numZones <- length(zones)
      externalZones <- districts$zone[districts$zone <= max(externals$station)]
      rownames(districts) <- zoneNames <- districts$zone  
      save(districts, file="inputs/RData/districts.RData")
      
      ##/
      #TAZ.CSV
      #Land Use information from census, employment and other internal zonal values
      ##/
      
      taz <- read.csv("inputs/taz.csv")
      if(file.exists("inputs/taz_census.csv"))   taz <- read.csv("inputs/taz_census.csv") 
      taz <- taz[order(taz$TAZ),]
      rownames(taz) <- taz$TAZ    
      tazCols <- colnames(taz) 
              	    
      ##/
      #SYNTHETIC POPULATION 
      #Replaces taz and pums file information with synthetic population
      #Note: Though synthetic population is used with the University model, it is not implemented in this module
      #@return taz_census.csv - for record purposes only
      #@return taz.csv
      #@return taz.RData      
      #@return pumaXXX.csv
      #@return pumsHHSyn.RData - for record purposes only    
      ##/
      
      if(as.logical(addPopSyn)) {       
          #SYNPOP INPUTS
          personsDF <- read.csv("inputs/PopSyn/persons.csv", header=T, as.is=T)       
          housesDF <- read.csv("inputs/PopSyn/households.csv", header=T, as.is=T)
          rownames(housesDF) <- housesDF$HHID 
                
          #Replace maz index value with corresponding TAZ number in housesDF
          xWlkDf <- read.csv("inputs/PopSyn/CALMpopSynIDxWalk.csv")      
          rownames(xWlkDf) <- xWlkDf$MAZ
          housesDF$maz <- xWlkDf[as.character(housesDF$maz),"CALMTAZID"]
        
          housesDF$tract <- housesDF$taz
          housesDF$taz <- housesDF$maz 
          housesDF$maz <- NULL
        
          agepDF <- personsDF[personsDF$relp%in%0,c("agep","OSUTag","HHID")]
          housesDF[as.character(agepDF$HHID),"ageRelp"] <- agepDF$agep
        
          #Get number of persons per HH and the HH Wgt	  
          housesDF$AdjNP <- housesDF$np
          housesDF$HHWgt <- rep(1,nrow(housesDF))

          #PROCESS TAZ FILE 
          out <- SynPopLUToTAZ(housesDF,taz)
                   
          #OUTPUT RESULTS
          #Save census taz file
          write.csv(out[["taz_census"]],"inputs/taz_census.csv",row.names=F)
        
          #Make SynPop taz the new taz file
          taz <- out[["modTaz"]]
          taz <- taz[order(taz$TAZ),]
          rownames(taz) <- taz$TAZ  
          write.csv(taz,"inputs/taz.csv",row.names=F)           
		      save(taz, file="inputs/RData/taz.RData")                       
        
          #output SynPop puma and SynPop pums file and save census version
          file.rename(paste("inputs/",pumaFileName,".csv",sep=""),paste("inputs/",pumaFileName,"_census.csv",sep="")) 
          write.csv(out[["puma"]],paste("inputs/",pumaFileName,".csv",sep=""),row.names=F)
          pumsHHSyn <-out[["pumsHHSyn"]] 
          save(pumsHHSyn, file="inputs/RData/pumsHHSyn.RData")
        
          rm(pumsHHSyn,personsDF,housesDF,agepDF,out)
        
          #Set addPopSynOnly to FALSE so modifications are no longer made in further iterations
          addPopSynOnly <- FALSE        
      }
   
########################################## END ################################################################################    