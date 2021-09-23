    # JEMnR Model Runner Script

    # This script takes command line arguments passed from a batch file to run
    # components of the JEMnR model

    # 03/03/2021 by Michael McCarthy, michael.mccarthy@rsginc.com
    ################################################################################

    options(digits=16,scipen=16,stringsAsFactors=F)

    #get working directory
    basedir <- getwd()

    #CodeLoc is the location of the JEM-n-R code
    codeLoc <- "rcode"

    # libraries
    library(doParallel)

    library(rhdf5)
    source("./rcode/OMX/omxapi.r")
    source("./rcode/OMX/ZMX.R")

    #LOAD NEEDED FUNCTIONS
    functionTable <- read.csv("functiontable.csv")
    #Source files listed in functionTable to load the functions
    for(i in 1:nrow(functionTable)){
          sourceFilePath <- paste(codeLoc, functionTable[i,"file"], sep="/")
          if(!file.exists(sourceFilePath)) stop(paste(sourceFilePath, "does not exist!"))
          source(sourceFilePath)
          rm(sourceFilePath)
    }
    #Attach the utility functions to the workspace
    attach(fun)

    #READ IN MODEL SETTINGS
    settings <- read.csv("inputs/settings.csv")
    createSettings(settings)

    ############################################################################

    validArgs = c("cvm","jemnr","buildmat","external","convergence","report")
    args = commandArgs(trailingOnly = TRUE)

    # 2 arguments: (1) module name and (2) current iteration
    if(args[1] %in% validArgs){
            iter <- as.numeric(args[2])
            cat("Running ", args[1],args[2],"\n")

            # jemnr main
            if(args[1] == "jemnr") {

              # Only run on first iteration
              if(iter == 1){
                if(file.exists("inputs/Rdata"))  {
                    allinDir <- list.files("inputs/Rdata")
                    for(File in allinDir) file.remove(paste("inputs/Rdata",File,sep="/"))
                }else{
                    dir.create("inputs/RData",showWarnings=F)
                }
              }

              # Clear ZMX matrices
              if(file.exists("unimodel/emmemat"))  {
                  allinDir <- list.files("unimodel/emmemat")
                  for(File in allinDir) file.remove(paste("unimodel/emmemat",File,sep="/"))
              }else{
                  dir.create("unimodel/emmemat",showWarnings=F)
              }


              #DETERMINE TRANSIT ASSIGNMENT TYPE
              if(as.logical(runPeakOffPeakTransitAssignment)) first_transit_assign_mat <- opadbus_matNum
              if(!as.logical(runPeakOffPeakTransitAssignment)) first_transit_assign_mat <- dailybus_matNum

              ################################################################################

              #feedback loop
              aChange <- 1
              RMSE <- 3

              source("rcode/ModelModules.r")
            }
            ## Build Matrices
            if(args[1] == "buildmat") {
              source("rcode/writeToOMX.R")
            }

            # Check Convergence
            if(args[1] == "convergence") {
              iter <- as.numeric(args[2])

              if(iter > 1 & file.exists("outputs/convergence.RData")){
                load("outputs/convergence.RData")
              }

              # Old skims
              load("inputs/RData/ivTimepeakdriveAlone.RData")
              if(!exists("pkTime.old")) {
                pkTime.old <- rowSums(ivTimepeakdriveAlone) * pkTimeOldPercent
                RMSE <- c()
                aChange <- c()
              }
              rm(ivTimepeakdriveAlone)

              # Update RData skims
              #source("rcode/inputsSave.R")
              matNumsPrT <- if(iter == 1){ c(tt = 2, dist = 3) }else{ c(tt = 4, dist = 5) } # Off-peak + peak travel time, distances
              prtPkSkim <- "outputs/matrices/prt_peak_skim.omx"
              matZones <- readLookupOMX("outputs/matrices/prt_peak_skim.omx","NO")
              ivTimepeakdriveAlone <- readSelectedOMX(prtPkSkim, as.character(matNumsPrT["tt"]))
              dimnames(ivTimepeakdriveAlone) <- list(matZones$Lookup,matZones$Lookup)


              RMSE <- c(RMSE, calcRmse(pkTime.old, rowSums(ivTimepeakdriveAlone)))
              cat(paste("iter=", iter, "RMSE=", RMSE[iter],"\n"))

              aChange <- c(aChange, max(abs(rowSums(ivTimepeakdriveAlone)-pkTime.old)[pkTime.old !=0]/pkTime.old[pkTime.old != 0]))
              pkTime.old <- rowSums(ivTimepeakdriveAlone)
              rm(ivTimepeakdriveAlone)

              save(pkTime.old, RMSE, aChange, file = "outputs/convergence.RData")

              # if converged, return status code to batch file
              if(!is.na(RMSE) & any(c(RMSE[length(RMSE)] > maxRMSE, aChange[length(aChange)] > absTTChange)) & iter < maxIter){
                cat("Assignment results converged, exiting loop\n")
                quit(save = "no", status = 10) # signals to batch file convergence reached
              }else{
                quit(save = "no", status = 0)
              }
            }

            # Reporting
            if(!is.null(referenceRun) & args[1] == "report") {
                source("rcode/modelReport.R")
            }
            # End
            cat("Module completed","\n")
            quit(save = "no", status = 0) # check if process compete and quit with status 0
    }else{
      if(length(args) == 0 ){
          err = "No model component specified. Supply an argument to run the model."
      }else if(length(args) > 0 | !args[1] %in% validArgs){
          err = paste("Invalid model component specified. Valid arguments are: ", paste(validArgs, collapse = ", "))
      }

      if(length(err) > 0){
        cat(err)
        quit(save = "no", status = 1)
      }
    }


########################################## END ################################################################################
