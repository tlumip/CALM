  #/
    #@filename access_forUnive.R
    #@author Martin Mann, martin.a.mann@odot.state.or.us
    #@date 11/11/14
    #@Description:
    #   Creates accessibility measures for two populations
    #   Returns the results to calling function
    
########################## START #####################################################################

    access_forUniv <- function(curSynPopData) {

        #Load inputs
        curTotalEmp <- curSynPopData[["totalEmp"]]
        curRetEmp <- curSynPopData[["retEmp"]]
        curHhs <- curSynPopData[["hhs"]]
        load("inputs/RData/autoDist.RData")
        load("inputs/RData/inthm.RData")
    
        #Calculate totals within a half mile
        walkTime.weight <- ((autoDist > .34 & autoDist <=1) * 
            ((.50 - (autoDist/2))/autoDist))
        walkTime.weight[is.na(walkTime.weight)] <- 0
        
        #Calculate tothm: Total emp w/in half mile
        tothm <- apply(autoDist, 1, function(x) sum((x <= 0.34) * curTotalEmp, na.rm=T)) +
                apply(walkTime.weight, 1, function(x) sum(x * curTotalEmp, na.rm=T))
        
        #Calculate rethm: Total retail w/in half mile
        rethm <- apply(autoDist, 1, function(x) sum((x <= 0.34) * curRetEmp, na.rm=T)) +
                apply(walkTime.weight, 1, function(x) sum(x * curRetEmp, na.rm=T))
        
        #Calculate hhhm: Households w/in half-mile
        hhhm <- apply(autoDist, 1, function(x) sum((x <= 0.34) * curHhs, na.rm=T)) +
                apply(walkTime.weight, 1, function(x) sum(x * curHhs, na.rm=T))
        
        #Calculate means
        hhhm.mean <- mean(hhhm, na.rm=T)
        tothm.mean <- mean(tothm, na.rm=T)
        inthm.mean <- mean(inthm, na.rm=T)
        rethm.mean <- mean(rethm, na.rm=T)
        
        #Calculate Mix Accessibility Variables
        mixthm <- (inthm * (tothm * (inthm.mean / tothm.mean)) *
                 (hhhm * (inthm.mean / hhhm.mean))) /
                 (inthm + (tothm * (inthm.mean / tothm.mean)) +
                 (hhhm * (inthm.mean / hhhm.mean)))
        
        mixthm[is.na(mixthm) | mixthm==0] <- 0.0001
        
        #Calculate mixrhm: Mixed Use (retail) w/in half mile
        mixrhm <- (inthm * (rethm * (inthm.mean / rethm.mean)) *
                 (hhhm * (inthm.mean / hhhm.mean))) /
                 (inthm + (rethm * (inthm.mean / rethm.mean)) +
                 (hhhm * (inthm.mean / hhhm.mean)))
        
        mixrhm[is.na(mixrhm) | mixrhm==0] <- 0.0001
    
        
        
        #calculate total employment within 30 minutes of transit
        tModes <- as.character(modes$mode[modes$type == "transit"])         
        for(tMode in tModes) {
              #Load inputs
              load(paste("inputs/RData/waitTimeAoffPeak", tMode, ".RData", sep=""))
              load(paste("inputs/RData/waitTimeBoffPeak", tMode, ".RData", sep=""))
              load(paste("inputs/RData/walkTimeoffPeak", tMode, ".RData", sep=""))
              load(paste("inputs/RData/ivTimeoffPeak", tMode, ".RData", sep=""))
              
              #Calculate total transit time and assign
              tTime <- get(paste("waitTimeAoffPeak", tMode, sep="")) +
                       get(paste("waitTimeBoffPeak", tMode, sep="")) +
                       get(paste("walkTimeoffPeak", tMode, sep="")) +
                       get(paste("ivTimeoffPeak", tMode, sep=""))
              assign(paste(tMode, "TTime", sep=""), tTime)
              
              #Remove inputs
              rm(list=paste(c("waitTimeAoffPeak", "waitTimeBoffPeak",
                       "walkTimeoffPeak", "ivTimeoffPeak"), tMode, sep=""))
              rm(tTime)
         }
         
         #Calculates transit accessibility for transit modes in modes object
         tot30t <- 0         
         for(tMode in tModes) {
              #Calculate mode specific employment within 30 minutes
              cTime <- get(paste(tMode, "TTime", sep=""))
              cTime.weight <- (cTime > 20 & cTime <=60) * ((30 - (cTime/2))/cTime)
                  
              tot30c <- apply(cTime, 1, function(x) sum((x <= 20) * curTotalEmp, na.rm=T)) +
                      apply(cTime.weight, 1, function(x) sum(x * curTotalEmp, na.rm=T))

              #Add mode specific number to running total
              tot30t <- tot30t + tot30c
              rm(cTime.weight, tot30c, cTime)
         }
         mixRetP <- matrix(log(mixrhm),length(mixrhm),length(mixrhm))
         mixRetA <- matrix(log(mixrhm),length(mixrhm),length(mixrhm), byrow=T)
         mixTotA <- matrix(log(mixthm),length(mixthm),length(mixthm), byrow=T)
                #Load inputs
         rm(curTotalEmp,curRetEmp,curHhs,autoDist,inthm)
         gc()  
         
         #Save access object
         return(list("mixthm"=mixthm,"mixrhm"=mixrhm,"tot30t"=tot30t,"mixRetP"=mixRetP,"mixRetA"=mixRetA,"mixTotA"=mixTotA))

      }
