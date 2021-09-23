    #/
    #@filename runModelMedford.R
    #@author Ben Stabler
    #@version 4
    #@date 2/05/13
    #
    #This script is used to run the JEM-n-R model.
    #Ben Stabler 10/31/03 benjamin.stabler@odot.state.or.us
    #Revised 10/28/05 brian.j.gregor@odot.state.or.us
    #If running RGui turn off buffered output (under Misc menu)
    #
    #Copyright (C) 2004  Oregon Department of Transportation
    #
    #This program is free software; you can redistribute it and/or
    #modify it under the terms of the GNU General Public License
    #as published by the Free Software Foundation; either version 2
    #of the License, or (at your option) any later version.
    #
    #This program is distributed in the hope that it will be useful,
    #but WITHOUT ANY WARRANTY; without even the implied warranty of
    #MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    #GNU General Public License for more details.
    #
    #You should have received a copy of the GNU General Public License
    #along with this program; if not, write to the Free Software
    #Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    # Revisions
    # 10/27/05: Turned off Special Generators
    # 10/28/05: Reads in a table of scripts of functions to be loaded (functiontable.csv). The table is in the main JEMnR directory. This allows the various scripts being called to be named other than the function names as was previously the case. The script names can be used to keep track of versions.
    # 03/16/06 Brian Gregor, added trip distribution scaling factors, using "inputsMedford_bjg031606.R"
    # 04/02/08 Alex Bettinardi, Changed Code to call updated Inputs and Inputs Save script
    # 10/21/2014 Martin Mann, Added call to run a recalibration of the car and worker submodel  constants if so indicated in settings file #Now removed 06/06/15 -MM
    # 11/10/2014 Martin Mann, 1.	Added sourcing of createTwoPopsFromUnivOutputs.R  within the run University control structure.  This script creates two populations, one without OSU students and the other without OSU students and faculty
                             #2.	Set control structure for PreGeneration to run if no University model
    # 4/11/16 AB - Edited inputCheck.r call to inputCheck_V4.r, (old JEMnR is inputCheck.R)

      #/
      #INPUTS SETUP
      #/
      library(doParallel)

      #Turn on option to save RData files in compressed form
      options(save.defaults=list(compress=TRUE))

      #Process inputs and save workspace
      source("rcode/inputs.R")

      #Save workspace (basic inputs and functions)
      if(iter==1) save.image()

      #Create inputs for Rdata folder
      source("rcode/inputsSave.R")

      #Input checker for first iteration only
      if(iter==1) {
          #serverLocalSource("\\\\s6000e\\6420only\\Tools\\ModelingTools\\JEMnR\\inputCheck_V4.r", "rcode\\inputCheck_V4.r")
          source("rcode/inputCheck_V4.r")
      }

      #/
      #UNIVERSITY MODEL
      #This is for the University Model and adjust inputs accordingly
      #/

        	if(as.logical(runUniversityModel)) {
            cat("UNIVERSITY TOUR MODEL",sep="\n")
            source("rcode/university/university.R")
            source("rcode/university/createTwoPopsFromUnivOutputs.R")
        	}

      #/
      #COMMERCIAL VEHICLE MODEL
      #Run the Commerical Vehicle Model if true
      #/

        	if(as.logical(runCommercialVehicleModel)) {
   	          cat("COMMERCIAL VEHICLE MODEL",sep="\n")
        		  source("rcode/comVeh/cvm.R")
        	}

      #/
      #PRE-GENERATION
      #This is run for non-University Model runs only
      #/

          if(!as.logical(runUniversityModel)){
              cat("PreGen for non-University runs",sep="\n")
              access() #Urban accessibility measures
              whia() #Worker sub-model
              chwi() #Auto ownership sub-model
              khia() #Child sub-model
              visitors() #Visitor sub-model
          }

      #/
      #PARALLEL PROCESSOR SETUP
          indx <- getEnvirPos("hbGen")[[1]]
          lsvec <- unique(c(ls(all.names=T),ls(indx)))
          cl <- makeCluster(7)
          registerDoParallel(cl)
          clusterExport(cl,lsvec )
     #/

      #/
      #TRIP GENERATION
      #/
          cat("TRIP GENERATION",sep="\n")
          purpLst <- list(c("hbwGen"),c("hbsroGen","hbs"),c("hbsroGen","hbr"),c("hbsroGen","hbo"),c("nhbGen","nhbw"),c("nhbGen","nhbnw"),c("hbcollGen"))
          TGC <- function(curPurpLst){
                    curPurpFun <- curPurpLst[[1]]
                    if(curPurpFun%in%c("hbwGen","hbcollGen")) get(curPurpFun)()
                    if(curPurpFun%in%c("hbsroGen","nhbGen")) get(curPurpFun)(curPurpLst[[2]])
                    curPurpLst
                  }
          parLapply(cl,purpLst, TGC)
          hbschGen()

      #/
      #MULTIMODAL ACCESSIBILITY
      #/
          cat("MULTIMODAL ACCESSIBILITY",sep="\n")
          MCPurpMKT <- c("hbw","hbo","hbr","hbs","hbcoll","nhbw","nhbnw")
          parLapply(cl,as.list(MCPurpMKT), accessUtilities)
          parLapply(cl,as.list(MCPurpMKT), accessLogSum)

    #/
    #DESTINATION CHOICE
    #/

          cat("DESTINATION CHOICE",sep="\n")
          purpLst <- list(c("hbwlowInc"),c("hbwmidInc"),c("hbwhighInc"),("hbo"),c("hbs"),c("hbr","nhbw"),c("hbcoll","nhbnw"))
          DTC <- function(curPurpLst){
                        for(purp in curPurpLst) tripDistribution(purp, "ugb")
                  }
          parLapply(cl,purpLst,DTC)
          parLapply(cl,list("hbw","hbcoll"),balanceDist)

          hbschDistByType() #Distribute home-based school by school type

    #/
    #MODE CHOICE
    #/

        cat("MODE CHOICE COMMON",sep="\n")
        #MODE CHOICE COMMON
        parLapply(cl,as.list(MCPurpMKT), modeChoiceCommon)

        #PREPROCESS MARKET SEGMENT SPECIFIC UTILITIES
        cat("PREPROCESS MARKET SEGMENT SPECIFIC UTILITIES",sep="\n")
        processSegmentUtils(purposes)

        #MODE CHOICE BY MARKET SEGMENT
        cat("MODE CHOICE BY MARKET SEGMENT",sep="\n")
        MCC <- function(curPurpLst){
                      for(perd in c("peak","offPeak")) calcTripsByMode(curPurpLst, perd, outputReport=FALSE)
                }
        parLapply(cl,as.list(MCPurpMKT), MCC)

        #MODE CHOICE FOR SHCOOLS
        hbschMcByType()

    #/
    #PEAKING AND GENERATION OF ASSIGNMENT PERIOD DEMAND MATRICES
    #/
         cat("COLLAPSE TABLES AND ALLOCATION",sep="\n")
         parLapply(cl,as.list(MCPurpMKT), collapseTables)

         stopCluster(cl)
         rm(cl)
         gc()

         collapseTables("hbsch") #collapse hbsch mode choice to daily by purpose by class
         allocateParkAndRide() #split pandr trips for all purposes to vehicle and transit class
         peaking() #apply directional factors and create assignment period matrices
         pandrPeaking() #apply directional factors and create park and ride assignment inputs

         #EXTERNAL MODEL
         cat("EXTERNAL MODEL",sep="\n")
         if(as.logical(externalModelFromSWIM)) {
            if(iter==1) extModelSWIM() #create external daily pa and od matrix based on SWIM subarea process
            addExternals() #add external model results to assignment matrices
         }else{
            externalModel() #create external daily pa and od matrix
            addExternals_old(, "peaking") #add external model results to assignment matrices
         }

########################################## END #################################################################
