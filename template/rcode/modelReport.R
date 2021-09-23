 # modelReport.R
 # Code Base (C) unknown: 7-20-16 (AB)
 #============
 #**Author:** Yegor Malinovskiy, malinovskiyy@pbworld.com
 #**Based on code by:
 #**Alex Bettinardi (AB) (alexander.o.bettinardi@odot.state.or.us) and
 #**Sriram Narayanamoorthy (narayanamoorthys@pbworld.com)
 #**Contact:** alexander.o.bettinardi@odot.state.or.us
 #**Date:** 6/25/14
 #**License:** GPL2
 #
 # Creates standard Reports for JEMnR
 #
 # Change / Revision History
 # 7/20/2016 AB - updated to add:
 #	- Auto ownership results for the model-wide area
 #	- Worker distribution results for the model-wide area
 #  -	Auto Sufficiency distribution results for the model-wide area
 #  Also removed the use of "purposes" from the JEMnR work space.
 #  Now uses "tripPurpose" consistently throughout, which is defined within this script / reporting function.
 # 8/2/16 AB - updated to add:
 #  - Mode shares
 #  - Transit trips by district
 #  - Daily and Peak Vehicle Demand
 # 8/18/16 AB - formalized script to begin using in all JEMnR analysis [major edits], up until this point it could be considered a draft concept
 # 9/12-15/16 AB - incorporated edits from modeling meeting on 9-7-16
 # 9/27/16 AB - found that a x factor scaling of 0.9 did not work for all JEMnR models - removed that scaling in the "distributionPlotter" function
 # 11/07/16 JXR - added the totals by purpose and by modes on the tables only
 # 11/16/16 Tpau - Clean up figures
 # 03/13/17 AB - added values to plots based on request from Jin and Sam

     # First load opitional libraries for html tables:
      options(warn=-1)
      htmlTableFlag <- library("xtable", logical.return=T)
      options(warn=0)

      # For whatever reason png files do not update the timestamp when they are overwritten
      # So, to show the proper "Date modified" date in window explorer, all png files are being
      # removed from teh "resultsAnalysis" Folder
      file.remove(list.files(path="resultsAnalysis", pattern="\\.png$", full.names=T))

      ################################################################################################
      # SETTINGS
      ################################################################################################

      # create place to update resoltion of plots
      Png <- list(Width=1000, Height=600, Res = 150)

      # Create a directory for storing the analysis results
      if(!file.exists("resultsAnalysis")) dir.create("resultsAnalysis")

      ################################################################################################
      # Functions
      ################################################################################################

 #================================================

 #Function to tabulate trips by time intervals to produce travel time distribution plots
 #--------------------------------------------

 #This function takes a zone to zone matrix travel time matrix and a zone to zone matrix of trips
 #and sums up the number of trips by a specified time interval. This function correctly accounts for fractional trips.
 #Other methods of doing this by repeating the travel times by the number of trips give the wrong
 #result because "rep" drops fractional trips. Trip fractions can add up to a sizable proportion of trips.
 #This was resolved in some code by multiplying the repeats by some number (e.g. 40) and then dividing the
 #binned result by the number. This however creates very long vectors and memory problems.
 #The following function bins the time data and then sums the number of trips by time bin. There are no memory problems.

 #The function returns a list that contains vectors identifying the breaks for the time bins (Breaks),
 #the number of trips in each time bin (Trips) and the midpoint values of each time bin (Mids).
 #The results can then be graphed directly using the midpoints and number of trips.
 #If it is desired to plot a smooth density distribution, then set the interval to sum small value (e.g. 0.25)
 #and then send to the density function a vector created by repeating the Mids by the Trips.
 # Brian Gregor 4/2006
 #(Brian's original code was designed to produce a historgram and used TimeInterval=1)

 #Example:
 #TimeTab_ <- tabulateTripTimes(OffPkTime.ZnZn, HbsTrips.ZnZn, TimeInterval=0.25)
 #plot(TimeTab_$Mids, TimeTab_$Trips, type="l")
 #plot(density(rep(TimeTab_$Mids, TimeTab_$Trips)))

     tabulateTripTimes <- function(Times.ZnZn, Trips.ZnZn, TimeInterval=.1){

        #Check that the Times.ZnZn and Trips.ZnZn are conforming matrices
        if(!is.matrix(Times.ZnZn)) stop("Times.ZnZn must be a matrix.")
        if(!is.matrix(Trips.ZnZn)) stop("Trips.ZnZn must be a matrix.")
        if(any(dim(Times.ZnZn) != dim(Trips.ZnZn))){
           stop("Times.ZnZn and Trips.ZnZn don't have the same dimensions")
           }
        if(any(rownames(Times.ZnZn) != rownames(Trips.ZnZn))){
           warning("rownames(Times.ZnZn) != rownames(Trips.ZnZn)")
           }
        if(any(colnames(Times.ZnZn) != colnames(Trips.ZnZn))){
           warning("colnames(Times.ZnZn) != colnames(Trips.ZnZn)")
           }

        #Unravel Times.ZnZn and Trips.ZnZn into vectors
        Times.X <- as.vector(Times.ZnZn)
        Trips.X <- as.vector(Trips.ZnZn)

        #Cut times into time intervals
        MaxTime <- max(Times.ZnZn)
        MaxTimeInterval <- TimeInterval * ceiling(MaxTime / TimeInterval)
        TimeIntervals.Tm <- seq(0, MaxTimeInterval, TimeInterval)
        NumIntervals <- length(TimeIntervals.Tm)
        TimeCut.X <- cut(Times.X, breaks=TimeIntervals.Tm, include.lowest=TRUE)

        #Tabulate trips by time interval
        Trips.Tm <- tapply(Trips.X, TimeCut.X, sum)
        Trips.Tm[is.na(Trips.Tm)] <- 0

        Time.Tm <- tapply(Times.X, TimeCut.X, mean)
        rep(Time.Tm,Trips.Tm)
        #removed Brian's histogram code to make way for plot

     }

#Define function to summarize mode shares
#========================================

#:Name: summarizeModeShares
#:Description: This function sequentially loads the trips by mode for each trip purpose and time of day. Each input file is an array of person trips by production zone, attraction zone and mode. The function sums all trips across zones to yield a vector of trips by mode. These are combined into peak and off-peak matrices of trips by trip purpose and mode. A matrix of total trips by mode is computed by summing the peak and off-peak matrices. Percentages of trips by mode are computed from these.
#:Argument: Dir - The path to the directory where the trip tables by mode are stored. The default assumes that the current directory is the top level JEMnR model directory
#:Return: ModeSummary - A list composed of two three dimensional arrays. The first array contains the number of trips by trip purpose, mode and time of day. The second array contains the percentage of trips by mode for each purpose and time of day.

#::

     summarizeModeShares <- function(Dir="modec"){

          # Define vector of trip purposes exclusive of school
          Pr <- c("hbcoll", "hbo", "hbr", "hbs", "hbw", "nhbnw", "nhbw")
          # Define vector of income groups
          Ic <- c("lowInc", "midInc", "highInc")
          # Define a vector that combines purposes and income
          Pi <- as.vector(t(outer(Pr, Ic, FUN="paste", sep="_")))
          # Define a vector of modes
          Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
          # Define a vector of peak and off-peak trip types
          Ty <- c("peakTrips", "offPeakTrips", "totalTrips")
          noPandR <- c("nhbnw", "nhbw", "hbcoll")

          # Initialize a matrix to hold the peak trip results
          Trips.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))

          # add in distance matrices for PMT cals
          load(gsub("modec","inputs/RData/autoDist.RData",Dir))
          load(gsub("modec","inputs/RData/tranDist.RData",Dir))
          load(gsub("modec","inputs/RData/bikeDist.RData",Dir))
          load(gsub("modec","inputs/RData/walkDist.RData",Dir))

          Dist.ZnZnMd <- array(0, dim=c(dim(autoDist)[1:2],length(Md)), dimnames=list(rownames(autoDist),colnames(autoDist),Md))
          Dist.ZnZnMd[,,1:3] <- autoDist
          Dist.ZnZnMd[,,4:5] <- tranDist
          Dist.ZnZnMd[,,6] <- bikeDist
          Dist.ZnZnMd[,,7] <- walkDist
          Dist.ZnZnMd[is.na(Dist.ZnZnMd)] <- 0

          # Initialize a matrix to hold the pmt results
          pmt.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))

          # Initialize a matrix to hold the pmt results
          intrazonal.PiMdTy <- array(0, dim=c(length(Pi), length(Md), length(Ty)), dimnames=list(Pi, Md, Ty))


          # Iterate through each purpose and income group and add data to matrix
          for(ty in Ty[1:2]){
               for(pr in Pr){
                    for(ic in Ic){
                         DataDir <- paste(Dir, "/", pr, "/", ic, "/", sep="")
                         File <- paste(ty, ".RData", sep="")
                         Path <- paste(DataDir, File, sep="")
                         load(Path)
                         Trips.Md <- apply(get(ty), 3, sum)
                         intzon <- apply(get(ty), 3, function(x) sum(diag(x)))
                         PMT <- apply(get(ty)*Dist.ZnZnMd[,,dimnames(get(ty))[[3]]], 3, sum)
                         RowName <- paste(pr, ic, sep="_")
                         if(pr %in% noPandR){
                              Trips.Md <- c(Trips.Md[1:4], parkAndRideBus=0, Trips.Md[5:6])
                              PMT <- c(PMT[1:4], parkAndRideBus=0, PMT[5:6])
                              intzon <- c(intzon[1:4], parkAndRideBus=0, intzon[5:6])
                              }
                         Trips.PiMdTy[RowName,,ty] <- Trips.Md
                         pmt.PiMdTy[RowName,,ty] <- PMT
                         intrazonal.PiMdTy[RowName,,ty] <- intzon

                         }
                    }
               }

          # Add the totals
          Trips.PiMdTy[,,3] <- Trips.PiMdTy[,,1] + Trips.PiMdTy[,,2]
          pmt.PiMdTy[,,3] <- pmt.PiMdTy[,,1] + pmt.PiMdTy[,,2]
          intrazonal.PiMdTy[,,3] <- intrazonal.PiMdTy[,,1] + intrazonal.PiMdTy[,,2]

          # Calculate percentages
          RowTotals <- apply(Trips.PiMdTy, c(1,3), sum)
          TripPcts.PiMdTy <- round(100 * sweep(Trips.PiMdTy, c(1,3), RowTotals, "/"), 1)

          # Return the result
          ModeSummary <- list(trips=Trips.PiMdTy, percents=TripPcts.PiMdTy, PMT = pmt.PiMdTy, intrazonal=intrazonal.PiMdTy)
          ModeSummary

          }

#Define function to plot travel time and distance distribution curves
#========================================

#:Name: distributionPlotter
#:Description: This function uses tabulateTripTimes to plot distribution curves
#:Argument: provide the x and y data to be plotted for both current and reference scenarios
#:Return: Very pretty graphs

#::
     distributionPlotter <- function(curSkim.ZnZn, curTrips.ZnZn, refSkim.ZnZn, refTrips.ZnZn, Title, legendCorner="topright",meanPointSize=2,plotLeg=T,meanTextXAdj=0,meanTextYAdj=0,TitleAdj=0){

        # parameter for density plot bandwidth - bw
        BW <- 1
        # parameter for max ylim
        Ylim <- 0.2

        distribution <- tabulateTripTimes(curSkim.ZnZn,curTrips.ZnZn)
        Hist <- hist(distribution,plot=F, breaks=0:round(max(curSkim.ZnZn)))
        refDist <- tabulateTripTimes(refSkim.ZnZn, refTrips.ZnZn)
        refHist <- hist(refDist,plot=F, breaks=0:round(max(curSkim.ZnZn)))

        plot(density(distribution, bw=BW, from=0, to=round(max(curSkim.ZnZn)*0.9)), ylim=c(0,Ylim), xlab="", ylab="", main="")
        for(i in 1:length(Hist$density)) polygon(c(Hist$mids[i]-.3,Hist$mids[i],Hist$mids[i],Hist$mids[i]-0.3, Hist$mids[i]-0.3),c(0,0,Hist$density[i],Hist$density[i],0))
        lines(density(refDist, bw=BW, from=0, to=round(max(refSkim.ZnZn)*0.9)),lty=2, col="red")
        for(i in 1:length(refHist$density)) polygon(c(refHist$mids[i],refHist$mids[i]+0.3,refHist$mids[i]+0.3,refHist$mids[i], refHist$mids[i]),c(0,0,refHist$density[i],refHist$density[i],0),col="red")

        curMean <- weighted.mean(curSkim.ZnZn,curTrips.ZnZn)
        points(curMean,Hist$density[abs(Hist$mids-curMean)==min(abs(Hist$mids-curMean))],cex=meanPointSize)
        refMean <- weighted.mean(refSkim.ZnZn,refTrips.ZnZn)
        points(refMean,refHist$density[abs(refHist$mids-refMean)==min(abs(refHist$mids-refMean))],cex=meanPointSize,col="red",pch=16)

        text(max(curSkim.ZnZn)*(0.3+TitleAdj), 0.9*Ylim, Title, cex=1.2)
        meantime <- round(curMean,1)
        #text(meantime, 0.04, paste("mean =", meantime),cex=0.6)
        text(max(curSkim.ZnZn)*(0.5+meanTextXAdj),(0.5+meanTextYAdj)*Ylim,paste("Current Mean=",meantime))          # remember expression(mu) as a way to plot the greek symbol for mean
        meantime <- round(refMean,1)
        #text(meantime, 0.02, paste("mean =", meantime), col="grey",cex=0.6)
        text(max(refSkim.ZnZn)*(0.5+meanTextXAdj),(0.4+meanTextYAdj)*Ylim,paste("Reference Mean=",meantime))
        if(plotLeg) legend(legendCorner, legend=c("Current", " - bar"," - mean","Reference"," - bar"," - mean"), lty=c(1,NA,NA,2,NA,NA), col=c(rep("black",3),rep("red",3)), bty="n",pch=c(NA,0,1,NA,15,16))

     }

      ################################################################################################
      # PLOTS
      ################################################################################################

      #######################################
      #  trip distance distribution
      #######################################

      # set up plotting device
     png(paste("resultsAnalysis", "TripDist.png", sep="/"), width=Png$Width*1.5, height=Png$Width, res=Png$Res)

     # Set up 3 X 3 plot layout
     layout(matrix(1:9, byrow=TRUE, ncol=3))
     # Change inner and outer margins
     par(mar=c(2,2,2,1), oma=c(4,2,2,2))

 #-------------------------------------------------------------------------------
  # Read in "autoDist.RData"
      curautoDist<- loadFile("inputs/RData/autoDist.RData")[[1]]

      refautoDist <- loadFile(paste(referenceRun,"inputs/RData/autoDist.RData",sep="/")) [[1]]

      #Create placeholders to hold trip distribution matrices
      Allpur<-0
      refAllpur<-0
      #start for loop to plot distance distribution by purpose
      purposes <- c("hbw","hbs","hbr","hbo","nhbw","nhbnw","hbcoll") # Define here for standalone run
     for(pu in purposes) {
                          load(paste("tripdist/", pu, "lowIncDist.RData", sep=""))
            		          load(paste("tripdist/", pu, "midIncDist.RData", sep=""))
                  		    load(paste("tripdist/", pu, "highIncDist.RData", sep=""))
                  		    total <- get(paste(pu, "lowIncDist", sep="")) +
      			              get(paste(pu, "midIncDist", sep="")) +
                 			    get(paste(pu, "highIncDist", sep=""))

                          load(paste(referenceRun,"/tripdist/", pu, "lowIncDist.RData", sep=""))
            		          load(paste(referenceRun,"/tripdist/", pu, "midIncDist.RData", sep=""))
                  		    load(paste(referenceRun,"/tripdist/", pu, "highIncDist.RData", sep=""))
                  		    reftotal <- get(paste(pu, "lowIncDist", sep="")) +
      			              get(paste(pu, "midIncDist", sep="")) +
                 			    get(paste(pu, "highIncDist", sep=""))
                 			    rm(list=c(paste(pu, "lowIncDist", sep=""),paste(pu, "midIncDist", sep=""),paste(pu, "highIncDist", sep="")))

       if(pu == "hbr"){
              distributionPlotter(curautoDist, total, refautoDist, reftotal, pu,meanTextXAdj=0.1,meanTextYAdj=-0.2)
       } else {
              distributionPlotter(curautoDist, total, refautoDist, reftotal, pu, plotLeg=F)
       }
       Allpur<-Allpur+total
       refAllpur<-refAllpur+reftotal

     }

     # plot total distribution:
        distributionPlotter(curautoDist, Allpur, refautoDist, refAllpur, "Total Internal\nDemand", plotLeg=F,,TitleAdj=0.1)

     # plot total distribution, full daily vehicle matrix with externals:
           curdayveh<- loadFile("peaking/dailyvehicle.RData")[[1]]
           refdayveh <- loadFile(paste(referenceRun,"peaking/dailyvehicle.RData",sep="/")) [[1]]
           distributionPlotter(curautoDist, curdayveh, refautoDist, refdayveh, "Total Vehicle\nDemand w External", plotLeg=F,TitleAdj=0.2)

     mtext("Distance Frequency Distributions Comparison", side=3, line=0, outer=TRUE, cex=1.25)
     mtext("Unit = Miles", side=1, line=2, outer=TRUE)
     dev.off()


      #######################################
      #  trip travel time distribution
      #######################################

      # set up plotting device
     png(paste("resultsAnalysis", "TripTT.png", sep="/"), width=Png$Width, height=Png$Width, res=Png$Res)

    # Read in "ivTimepeakdriveAlone.RData"
      curautoTT<- loadFile("inputs/RData/ivTimepeakdriveAlone.RData")[[1]]
      refautoTT <- loadFile(paste(referenceRun,"inputs/RData/ivTimepeakdriveAlone.RData",sep="/")) [[1]]

      # read in peak demand
      curveh<- loadFile("peaking/pm1vehicle.RData")[[1]]
      refveh <- loadFile(paste(referenceRun,"peaking/pm1vehicle.RData",sep="/")) [[1]]
      distributionPlotter(curautoTT, curveh, refautoTT, refveh, "",,meanPointSize=3)
      mtext("Peak Travel Time Frequency Distributions Comparison",side=3,cex=1.25, line=1)
      mtext("Unit = Minutes", side=1, line=2)
     dev.off()


     # clean work space
     rm(pu, tabulateTripTimes,Allpur, refAllpur,total,reftotal,curautoDist,refautoDist,curautoTT,refautoTT, distributionPlotter)

      ####################################
      # plotting daily and peak auto trips
      ####################################

      vehDmd <- rbind(Current=c(Daily=sum(curdayveh),Peak=sum(curveh)),Reference=c(Daily=sum(refdayveh),Peak=sum(refveh)))
      # plotting Tranist Trips by District
      png(paste("resultsAnalysis/dailyPeakVehTrips.png", sep="/"), width=Png$Width, height=Png$Width*0.8, res=Png$Res)
      options(scipen=7)
      barplot(vehDmd,beside=T,col=c("white", "grey"), legend.text=T, ylab="Number of Trips", xlab="", main="Vehicle Trips by Daily and PM Peak")
      # text(c(4.5,5.5), c(1.75)*vehDmd[,"Peak"], paste(round(100*vehDmd[,"Peak"]/vehDmd[,"Daily"],1),"%",sep="") )
      # text(5,2.5*max(vehDmd[,"Peak"]),"Peak Percentage of Daily")
      text(1.5,0.9*vehDmd[1,1],paste("Percentage\nof Reference\n",round(100*vehDmd[1,1]/vehDmd[2,1],1),"%",sep=""))
      text(c(1.5,2.5, 4.5,5.5), 0.5*vehDmd, format(round(vehDmd/1000)*1000,big.mark=",",scientific=FALSE) )
      dev.off()

      rm(curdayveh, curveh, refdayveh, refveh, vehDmd)

      #######################################
      #  Plotting pre-gen
      #######################################

      # Read in "whiazcAry.RData"
      load("pregen/whiazcAry.RData")
      curWHIAZC <- whiazcAry

      load(paste(referenceRun,"/pregen/whiazcAry.RData",sep=""))
      refWHIAZC <- whiazcAry

      # auto ownership
      carOwnership <- rbind(Current=apply(curWHIAZC,6,sum),Reference=apply(refWHIAZC,6,sum))
      colnames(carOwnership) <- c("No car", "1 car", "2 cars", "3+ cars")
      png(paste("resultsAnalysis/AutoOwnership.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
      plotX <- barplot(carOwnership,beside=T,col=c("white", "grey"), legend.text=T, ylab="Count of Households", xlab="Car Ownership", main="Model-Wide Auto Ownership Distribution Comparison")
      plotY <- carOwnership
      plotZ <-  round(100*plotY/rowSums(plotY),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(round(plotY), ":  ", plotZ,"%",sep=""), pos=1, srt=270)
      dev.off()
      rm(carOwnership)

      # total workers
      workers <- rbind(Current=apply(curWHIAZC,1,sum),Reference=apply(refWHIAZC,1,sum))
      colnames(workers) <- c("No workers", "1 worker", "2 workers", "3+ workers")
      png(paste("resultsAnalysis/WorkerModel.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
      plotX <- barplot(workers,beside=T,col=c("white", "grey"), legend.text=T, ylab="Count of Households", xlab="Number of Workers", main="Model-Wide Worker Distribution Comparison")
      plotY <- workers
      plotZ <-  round(100*plotY/rowSums(plotY),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(round(plotY), ":  ", plotZ,"%",sep=""), pos=1, srt=270)
      dev.off()
      rm(workers)

      # auto sufficiency
      cValVec <- c(rep("cVal0",4),"cVal3",rep("cVal2",1),rep("cVal1",2),rep("cVal3",2),"cVal2",rep("cVal1",1),rep("cVal3",3),"cVal2")
      carSuf <- rbind(Current=tapply(as.vector(apply(curWHIAZC,c(1,6),sum)),cValVec,sum), Reference=tapply(as.vector(apply(refWHIAZC,c(1,6),sum)),cValVec,sum))
      colnames(carSuf) <- c("0 Vehicles", "Cars < Workers", "Cars = Workers", "Cars > Workers")
      png(paste("resultsAnalysis/AutoSufficiency.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
      plotX <- barplot(carSuf,beside=T,col=c("white", "grey"), legend.text=T,args.legend=list(x="topleft"), ylab="Count of Households", xlab="Car Sufficiency", main="Model-Wide Auto Sufficiency Distribution Comparison")
      plotY <- carSuf
      plotZ <-  round(100*plotY/rowSums(plotY),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(round(plotY), ":  ", plotZ,"%",sep=""), pos=1, srt=270)
      dev.off()
      rm(carSuf, cValVec, plotX, plotY, plotZ)

      #

      ######################################
      # Plotting Mode Choice
      #########################################

      curModeSummary_ <- summarizeModeShares()

      cur.PuMd <- curModeSummary_[["trips"]][,,"totalTrips"]

      refModeSummary_ <- summarizeModeShares(Dir=paste(referenceRun,"modec",sep="/"))

      ref.PuMd <- refModeSummary_[["trips"]][,,"totalTrips"]

      purpNames <- unlist(lapply(strsplit(rownames(cur.PuMd), "_"), function(x) x[1]))

      cur.PuMd <- tapply(as.vector(cur.PuMd), as.list(expand.grid(purpNames, colnames(cur.PuMd))), sum)

      ref.PuMd <- tapply(as.vector(ref.PuMd), as.list(expand.grid(purpNames, colnames(ref.PuMd))), sum)

      modeShare <- rbind(Current=colSums(cur.PuMd), Reference = colSums(ref.PuMd))
      modeShare <- modeShare[,names(sort(colSums(modeShare),decreasing=T))]
      png(paste("resultsAnalysis/TripsByMode.png", sep="/"), width=Png$Width, height=Png$Height*1.5, res=100)
      layout(matrix(1:2, byrow=TRUE, ncol=1))
      # Change inner and outer margins
      par(mar=c(1,4,2,1), oma=c(2,2,2,2))
      plotX <- barplot(modeShare,beside=T,col=c("white", "grey"), legend.text=T, ylab="Number of Trips")
      plotY <- modeShare
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),round(plotY), pos=1, srt=270)
      plotX <- barplot(100*modeShare/rowSums(modeShare),beside=T,col=c("white", "grey"),  ylab="Percent of Trips")
      plotY <- round(100*modeShare/rowSums(modeShare),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(plotY,"%",sep=""), pos=1, srt=270)
      mtext("Model-Wide Trips by Mode", side=3, line=0, outer=TRUE, cex=1.25)
      dev.off()

      modeShare <- rbind(Current=rowSums(cur.PuMd), Reference = rowSums(ref.PuMd))
      modeShare <- modeShare[,names(sort(colSums(modeShare),decreasing=T))]
      png(paste("resultsAnalysis/TripsByPurpose.png", sep="/"), width=Png$Width, height=Png$Height*1.5, res=100)
      layout(matrix(1:2, byrow=TRUE, ncol=1))
      # Change inner and outer margins
      par(mar=c(1,4,2,1), oma=c(2,2,2,2))
      plotX <- barplot(modeShare,beside=T,col=c("white", "grey"), legend.text=T, ylab="Number of Trips")
      plotY <- modeShare
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),round(plotY), pos=1, srt=270)
      plotX <- barplot(100*modeShare/rowSums(modeShare),beside=T,col=c("white", "grey"), ylab="Percent of Trips")
      plotY <- round(100*modeShare/rowSums(modeShare),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(plotY,"%",sep=""), pos=1, srt=270)
      mtext("Model-Wide Trips by Purpose", side=3, line=0, outer=TRUE, cex=1.25)
      dev.off()


      #######################################
      #  PMT plot
      #######################################

      curPMT.PuMd <- curModeSummary_[["PMT"]][,,"totalTrips"]

      refPMT.PuMd <- refModeSummary_[["PMT"]][,,"totalTrips"]

      curPMT.PuMd <- tapply(as.vector(curPMT.PuMd), as.list(expand.grid(purpNames, colnames(curPMT.PuMd))), sum)

      refPMT.PuMd <- tapply(as.vector(refPMT.PuMd), as.list(expand.grid(purpNames, colnames(refPMT.PuMd))), sum)

      modeShare <- rbind(Current=colSums(curPMT.PuMd), Reference = colSums(refPMT.PuMd))
      modeShare <- modeShare[,names(sort(colSums(modeShare),decreasing=T))]
      png(paste("resultsAnalysis/PMT.png", sep="/"), width=Png$Width, height=Png$Height*2, res=100)
      layout(matrix(1:3, byrow=TRUE, ncol=1))
      # Change inner and outer margins
      par(mar=c(1,4,2,1), oma=c(2,2,2,2))
      plotX <- barplot(modeShare,beside=T,col=c("white", "grey"), legend.text=T, ylab="Person Miles Traveled (PMT)")
      plotY <- modeShare
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),round(plotY), pos=1, srt=270)
      plotX <- barplot(100*modeShare/rowSums(modeShare),beside=T,col=c("white", "grey"),  ylab="Percent of PMT")
      plotY <- round(100*modeShare/rowSums(modeShare),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(plotY,"%",sep=""), pos=1, srt=270)

      curPMT.PuMd <- curModeSummary_[["intrazonal"]][,,"totalTrips"]

      refPMT.PuMd <- refModeSummary_[["intrazonal"]][,,"totalTrips"]

      curPMT.PuMd <- tapply(as.vector(curPMT.PuMd), as.list(expand.grid(purpNames, colnames(curPMT.PuMd))), sum)

      refPMT.PuMd <- tapply(as.vector(refPMT.PuMd), as.list(expand.grid(purpNames, colnames(refPMT.PuMd))), sum)

      modeShare2 <- rbind(Current=colSums(curPMT.PuMd)/colSums(cur.PuMd), Reference = colSums(refPMT.PuMd)/colSums(ref.PuMd))
      modeShare2 <- 100*modeShare2[,colnames(modeShare)]
      plotX <- barplot(modeShare2,beside=T,col=c("white", "grey"), ylab="Percent Intrazonal Trips")
      plotY <- round(modeShare2,2)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(plotY,"%",sep=""), pos=1, srt=270)
      mtext("Model-Wide PMT by Mode", side=3, line=0, outer=TRUE, cex=1.25)
      dev.off()

      rm( modeShare, purpNames,modeShare2,  curPMT.PuMd,  refPMT.PuMd)

      ######################################
      # Plotting Transit trips by transit district
      #########################################

      # read current bus matrix
      cur.Bus <- loadFile("peaking/dailybus.RData")
      # read the current district file
      curDist <- read.csv("inputs/districts.csv")

      # read reference bus matrix
      ref.Bus <- loadFile(paste(referenceRun,"/peaking/dailybus.RData",sep=""))
      # read the current district file
      refDist <- read.csv(paste(referenceRun,"/inputs/districts.csv",sep=""))

      # tabulating bus trips by tranist district
      curBus.Di <- tapply(rowSums(cur.Bus[[1]])[as.character(curDist$zone)],curDist$transitDistricts,sum)
      refBus.Di <- tapply(rowSums(ref.Bus[[1]])[as.character(refDist$zone)],refDist$transitDistricts,sum)

      # plotting Tranist Trips by District
      png(paste("resultsAnalysis/TransitTripsByDistrict.png", sep="/"), width=Png$Width, height=Png$Height, res=Png$Res)
      plotX <- barplot(rbind(Current=curBus.Di, Reference=refBus.Di),beside=T,col=c("white", "grey"), legend.text=T, ylab="Number of Trips", xlab="Districts", main="Transit Daily Trips by Origin District")
      plotY <- rbind(Current=curBus.Di, Reference=refBus.Di)
      plotZ <-  round(100*plotY/rowSums(plotY),1)
      text(plotX,ifelse(plotY<max(plotY)*.5,plotY+(max(plotY)*.2),plotY-(max(plotY)*.2)),paste(round(plotY), ":  ", plotZ,"%",sep=""), pos=1, srt=270)
      dev.off()
      rm(cur.Bus, curDist, ref.Bus,refDist, curBus.Di, refBus.Di, plotX, plotY, plotZ)


      ################################################################################################
      # TABLES
      ################################################################################################

      # to obtain model daily totals by purpose and by mode
      DailyMode <- colSums(cur.PuMd)
      cur.PuMd <- rbind(cur.PuMd, DailyMode)
      DailyTotal <- rowSums(cur.PuMd)
      cur.PuMd <- cbind(cur.PuMd, DailyTotal)

      # to obtain reference daily totals by purpose and by mode
      DailyMode <- colSums(ref.PuMd)
      ref.PuMd <- rbind(ref.PuMd, DailyMode)
      DailyTotal <- rowSums(ref.PuMd)
      ref.PuMd <- cbind(ref.PuMd, DailyTotal)

      print("Current Scenario Trips by Mode by Purpose")
      print(round(cur.PuMd))
      print("Reference Scenario Trips by Mode by Purpose")
      print(round(ref.PuMd))
      print("Difference between Trips by Mode")
      print(round(cur.PuMd-ref.PuMd))
      print("Percent Difference between Current and Reference Scenario")
      print(round(100*(cur.PuMd-ref.PuMd)/ref.PuMd))

      write.csv(cur.PuMd, "resultsAnalysis/currentScenarioTripsbyMode.csv")
      write.csv(ref.PuMd, "resultsAnalysis/referenceScenarioTripsbyMode.csv")

            # Write html to store everything
            #===============================
            writeLines(c("<html>", "<head>",
                       "<title>JEMnR Run Comparison</title>",
                       "</head>","<body>",
                       " <p>The following are comparisons of this model run vs. the Reference file:",
                       paste(referenceRun,"</p>",sep=""),
                       " <p>Pre-gen Analysis:</p>",
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/workerModel.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/AutoOwnership.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/AutoSufficiency.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       " <p>Trip Generation by Purpose:</p>",
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/TripsByPurpose.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       " <p>Trip Length Distribution Analysis by Purpose:</p>",
                        paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/TripDist.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                        " <p>Peak Travel Time Distribution:</p>",
                        paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/TripTT.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                        " <p>Transit Trips by Fare District:</p>",
                        paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/TransitTripsByDistrict.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                        " <p>Daily and Peak Vehicle Demand:</p>",
                        paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/dailyPeakVehTrips.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       " <p>Person Miles Traveled by Mode (and IntraZonal Percenatages):</p>",
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/PMT.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" ),
                       " <p>Trips by Mode:</p>",
                       paste("<div class=\"chunk\" id=\"reporting\"><div class=\"rimage default\"><img src=\"resultsAnalysis/TripsByMode.png",
                              "\" title=\"plot of chunk reporting\" alt=\"plot of summary\" class=\"plot\" /></div>" )
                          ),"modelReport.html")

      if(htmlTableFlag){
            # now build htlm tables if flag is true and append them to the html
            #-------------------------------------------------------------------
            sink("temp.html")
            curPrint.PuMd <- xtable( round(cur.PuMd), align=rep( "c", ncol(cur.PuMd)+1 ), digits=0,
            		         caption="Current Trips by Mode and Purpose")

            cat(" <p>Current Scenario Trips by Mode by Purpose:</p>\n")
            print(curPrint.PuMd, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=TRUE)

            refPrint.PuMd <- xtable( round(ref.PuMd), align=rep( "c", ncol(ref.PuMd)+1 ), digits=0,
                         caption="Reference Trips by Mode and Purpose")
            cat(" <p>Reference Scenario Trips by Mode by Purpose:</p>\n")
            print(refPrint.PuMd, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=TRUE)

            cat(" <p>Difference between Trips by Mode:</p>\n")
            difPrint.PuMd <- xtable(round(cur.PuMd-ref.PuMd), align=rep( "c", ncol(ref.PuMd)+1 ), digits=0,
                         caption="Difference in Trips by Mode and Purpose")
            print(difPrint.PuMd, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=TRUE)

            cat(" <p>Percent Difference between Current and Reference Scenario:</p>\n")
            perPrint.PuMd <- xtable(round(100*(cur.PuMd-ref.PuMd)/ref.PuMd), align=rep( "c", ncol(ref.PuMd)+1 ), digits=0,
                         caption="Percent Difference by Mode and Purpose")
            print(perPrint.PuMd, type="html", caption.placement="top", include.colnames=TRUE, include.rownames=TRUE)

            rm(curPrint.PuMd, refPrint.PuMd, difPrint.PuMd, perPrint.PuMd)

            # close the "temp.html" sink
            sink()

            # append tables to html
            temp <- readLines("modelReport.html")
            writeLines(c(temp,readLines("temp.html")),"modelReport.html")
            file.remove("temp.html")
      }
              # Finalize the html
  #------------------
  temp <- readLines("modelReport.html")
  writeLines(c(temp, "</body>","</html>"),"modelReport.html")
  rm(cur.PuMd, ref.PuMd, temp, htmlTableFlag, Png)
########################################################### END ####################################################################################
