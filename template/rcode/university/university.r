#UNIVERSITY MODEL RUNNER
#:Author: Yegor Malinovskiy
#:Contact: malinovskiyy@pbworld.com
#:Date: 05/19/2014
#:Revised: Martin Mann 10/17/14 - 11/10/14
#  1_Added code to set university iterations from value in settings file
#  2_Rewrote code to process trips.csv from the university run to create OD matrices used in writeToBank.r
#    Reduced runtime from 9  minutes to 10 seconds and added OD outputs for unassigned modes and periods
#  3_Added code to create person and household inputs csv from synpop outputs
#:Revised: Martin Mann 06/19/16 Added balancing in and out for daily matrices to make consistent with all other matrices (JEMNR, External, Commercial)
#:Revised: Martin Mann 10/25/17 Corrected code that computed the Time of Day. This is used to designate records as peak hour

      ################### READ IN AND OUT POPSYN OUTPUTS ###########################

      #Replace maz index value with corresponding TAZ number in housesDF
      personsDF <- read.csv("inputs/PopSyn/persons.csv", header=T, as.is=T)
      housesDF <- read.csv("inputs/PopSyn/households.csv", header=T, as.is=T)
      xWlkDf <- read.csv("inputs/PopSyn/CALMpopSynIDxWalk.csv")[,c("CALMTAZID","MAZ")]
      rownames(xWlkDf) <- xWlkDf$MAZ

      #Modify HH
      housesDF$tract <- housesDF$taz
      housesDF$taz <- xWlkDf[as.character(housesDF$maz),"CALMTAZID"]
      housesDF <- housesDF[order(housesDF$HHID,decreasing=F),]

      #Modify Persons
      personsDF$tract <- personsDF$taz
      personsDF$taz <- xWlkDf[as.character(personsDF$maz),"CALMTAZID"]
      colnames(personsDF)[colnames(personsDF)%in%"OSUTag"] <- "UofOTag"
      personsDF <- personsDF[order(personsDF$HHID,personsDF$sporder,decreasing=F),]

      write.csv(personsDF,"unimodel/persons.csv", row.names=F)
      write.csv(housesDF,"unimodel/households.csv",  row.names=F)

      ################### RUN UNIVERSITY MODEL ####################################

      setwd(paste(basedir,"\\unimodel",sep=''))

      tpau_prop <- readLines("ctlfiles\\tpau_tbm.properties")
      line <- grep("^databank.FileName = %project.folder%/emmemat/", tpau_prop)
      dFNameArr <- strsplit(tpau_prop[line], "/")
      dFNameArr <- dFNameArr[[1]]
      dFNameArr[length(dFNameArr)] = paste("mf",autoDistance_matNum,".zmx",sep="")
      tpau_prop[line] <- paste(dFNameArr, sep="/", collapse="/")
      tpau_prop <- writeLines(text=tpau_prop, "ctlfiles\\tpau_tbm.properties")

      emmeMats <- apropos("*matNum")
      tempList = vector()
      for(i in 1:length(emmeMats)){tempList <- c(tempList, get(emmeMats[i]))}
      tempList  <- unique(tempList)
      emme_matrices = ""
      for(i in 1:length(tempList)){emme_matrices <- paste(emme_matrices, paste("mf", tempList[i], sep=""))}

      #Run University Model (CT-RAMP)
      shell(paste("runUniversityModel.cmd", getwd(), universitySampleRate, universityIterations)) #, EMME_File, peak_scen_num, EMME_Python, Java, GNU_Tools, paste('"', emme_matrices, '"', sep="")))
      setwd(basedir)

      rm(emmeMats, tempList, emme_matrices, tpau_prop, dFNameArr, line)

    ####################### UNIVERSITY DEMAND MATRICES #####################################################

    #tripMode:
    #1 sov free
    #2 sov pay
    #3 hov2 free
    #4 hov2 pay
    #5 hov3 free
    #6 hov3 pay
    #7 = walk
    #8 = bike
    #9+ = transit
        #CODE TO CREATE OD MATRICES FROM TRIP TABLE
        #Added by Martin Mann July 2014, replaced original code for performance gains and expanded output
        if(!file.exists("peaking")) dir.create("peaking")
        trips_uni <- read.csv("unimodel/outputs/trips.csv", header=T, as.is=T)
        trips_uni$trips <- 1
        trips_uni[trips_uni$tripMode %in% c(3,4),"trips"] <- trips_uni[trips_uni$tripMode %in% c(3,4),"trips"]*(1/2)
        trips_uni[trips_uni$tripMode %in% c(5,6),"trips"] <- trips_uni[trips_uni$tripMode %in% c(5,6),"trips"]*(1/3.5)
        tripType <- c("vehicle","vehicle","vehicle","vehicle","vehicle","vehicle","walk","bike","bus")
        names(tripType) <- 1:9
        for(i in 1:nrow(TOD_periods)) {
            if(!TOD_periods[i,"Period"]%in%"daily"){
                  timeVec <- c(seq(300,2400,50),seq(50,250,50))
                  names(timeVec) <- 1:48
                  trip_period <- timeVec[as.character(trips_uni$period)]
                  startPeriod <- as.numeric(TOD_periods[i,"StartTime"])
                  endPeriod <- as.numeric(TOD_periods[i,"EndTime"])
                  curPeriodIndx <- trip_period >= startPeriod & trip_period <= endPeriod
                  curPerTrips <- trips_uni[curPeriodIndx,]
            }
            if(TOD_periods[i,"Period"]%in%"daily") curPerTrips <- trips_uni

            #Process Trips
            splitindx <-  tripType[as.character(curPerTrips$tripMode)]
            tripOD <- as.data.frame(paste(curPerTrips$originTaz,curPerTrips$destinationTaz,sep="_"),stringsAsFactors=F)
            colnames(tripOD) <- "tripOD"
            tripList <- split(cbind(curPerTrips[,c("originTaz","destinationTaz","trips")],tripOD) ,splitindx)
            tripListTotals <- lapply(tripList,function(x) tapply(x$trips,x$tripOD,sum))
            FlatMats <- lapply(names(tripList),function(x){
                        curTotals <- tripListTotals[[x]]
                        curOD <- tripList[[x]]
                        curOD <- curOD[!duplicated(curOD$tripOD),]
                        curOD <- curOD[order(curOD$originTaz,curOD$destinationTaz),]
                        temp <- cbind(curOD[c("originTaz","destinationTaz")],as.data.frame(as.vector(curTotals[curOD$tripOD])))
                        colnames(temp) <- c("originTaz","destinationTaz","tripOD")
                        temp
                        })
            names(FlatMats) <-  names(tripList)
            tazNames <- taz$TAZ
            Mats <- lapply(names(FlatMats),function(x) {
                          curMatLst <- as.list(FlatMats[[x]])
                          from <- rep(tazNames,each=length(tazNames))
                          to <- rep(tazNames,length(tazNames))
                          values <- rep(0,length(tazNames)^2)
                          vec1 <- paste(from,to,sep="_")
                          vec2 <- paste(curMatLst[[1]],curMatLst[[2]],sep="_")
                          indx1 <- which(vec1%in%vec2)
                          values[indx1] <- curMatLst[[3]]
                          outMat <- matrix(values,ncol=length(tazNames),byrow=T)
                          dimnames(outMat) <- list(tazNames,tazNames)
                          if(TOD_periods[i,"Period"]%in%"daily") outMat <- (outMat + t(outMat)) / 2  #added MM 061916
                          outMat
                    })
            names(Mats) <-  names(FlatMats)
            assign(paste(TOD_periods[i,"Period"],"vehicleuniversity",sep=""),Mats[["vehicle"]])
            save(list=(paste(TOD_periods[i,"Period"],"vehicleuniversity",sep="")),file=paste("peaking/",TOD_periods[i,"Period"],"vehicleuniversity.rData",sep=""))
            assign(paste(TOD_periods[i,"Period"],"busuniversity",sep=""),Mats[["bus"]])
            save(list=(paste(TOD_periods[i,"Period"],"busuniversity",sep="")),file=paste("peaking/",TOD_periods[i,"Period"],"busuniversity.rData",sep=""))
            assign(paste(TOD_periods[i,"Period"],"walkuniversity",sep=""),Mats[["walk"]])
            save(list=(paste(TOD_periods[i,"Period"],"walkuniversity",sep="")),file=paste("peaking/",TOD_periods[i,"Period"],"walkuniversity.rData",sep=""))
            assign(paste(TOD_periods[i,"Period"],"bikeuniversity",sep=""),Mats[["bike"]])
            save(list=(paste(TOD_periods[i,"Period"],"bikeuniversity",sep="")),file=paste("peaking/",TOD_periods[i,"Period"],"bikeuniversity.rData",sep=""))
          }

    ######################################### END #####################################################################
