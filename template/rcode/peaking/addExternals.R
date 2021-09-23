#/
#@filename addExternals.R
#@author Ben Stabler
#
# Add external model results to assignment period matrices
# Load external model matrix and add to II model matrix for assignment by time period
# BTS 03/14/12 - Added separate time-of-day factoring of E-E vs. I-E and E-I trips
# BTS 12/4/12 - Revised to use external model results from SWIM
# AB 8/26/2013 - took out hard coded assumption that all zones numbered less than 100 are external zones.
       #ExternalZoneNums <- sort(unique(externals$STATIONNUMBER)) #took out hardcoded <100 limit on 8/26/2013 - AB    
       #Took out hardcoded <100 limit on 8/26/2013 - AB  
       #Vehicle[zoneNames %in% ExternalZoneNums,zoneNames %in% ExternalZoneNums] <- get(paste(p, "vehicle", sep=""))[zoneNames %in% ExternalZoneNums, zoneNames %in% ExternalZoneNums]
# Martin Mann 06_6_16  Added code to allow K-factors to be applied to the daily or peak period auto or truck matrices
# Martin Mann 06_16_16  Added code balance in and out flow on daily external matrices
    
    fun$addExternals <- function() {
          
          ##/
          #ADD EXTERNALS BY PERIOD TO INTERNALS 
          ##/
          
          cat("Add SWIM external model results to assignment period matrices\n\n")
          
          ##/
          #LOAD INPUTS
          ##/
          
              load(paste(storeLoc, "externalOD_ZnZnTdMd.RData", sep=""))
              externals <- read.csv(paste(storeLoc, "selectLinks_Report.csv", sep=""))
                                                                  
          ##/
          #GET INTERNAL MATRICES, COMBINE WITH EXTERNAL
          ##/
          
              for(p in c("daily", names(pCols))){

                  #Save external auto matrices by period
                  assign(paste(p, "external_auto", sep="_"), apply(ext.ZnZnTdMd[,,p,!dimnames(ext.ZnZnTdMd)[[4]] %in% "truck"], 1:2, sum)) 
                  if(file.exists("inputs/externalModel/adjExttoSrvy_auto.RData")) { 
                        load("inputs/externalModel/adjExttoSrvy_auto.RData") 
                        curAuto <- get(paste(p, "external_auto", sep="_"))
                        curAuto <- curAuto * adjExttoSrvy_auto[rownames(curAuto),colnames(curAuto)]
                        curAuto <- extIPF(externals[externals$DIRECTION=="IN", paste(p, "auto", sep="_")], externals[externals$DIRECTION=="OUT",  paste(p, "auto", sep="_")],curAuto, as.character(externalZones),p )         
                        assign(paste(p, "external_auto", sep="_"),curAuto) 
                  } 
                  if(p == "daily") daily_external_auto <-  (daily_external_auto + t(daily_external_auto))/2 #Added MM 061616   
                  save(list=paste(p, "external_auto", sep="_"), file=paste("peaking/", p, "external_auto.RData", sep=""))
                  
                  #Save truck matrices by period
                  assign(paste(p, "external_truck", sep="_"), ext.ZnZnTdMd[,,p,"truck"])
                  if(file.exists("inputs/externalModel/adjExttoSrvy_truck.RData")) {
                        load("inputs/externalModel/adjExttoSrvy_truck.RData") 
                        curTruck <- get(paste(p, "external_truck", sep="_"))
                        curTruck <- curTruck * adjExttoSrvy_truck[rownames(curTruck),colnames(curTruck)]
                        curTruck <- extIPF(externals[externals$DIRECTION=="IN", paste(p, "truck", sep="_")], externals[externals$DIRECTION=="OUT",paste(p, "truck", sep="_")],curTruck, as.character(externalZones),p)         
                        assign(paste(p, "external_truck", sep="_"),curTruck) 
                  }
                  #For daily ensure in == out
                  if(p == "daily") daily_external_truck <-  (daily_external_truck + t(daily_external_truck))/2   #Added MM 061616               
                  save(list=paste(p, "external_truck", sep="_"), file=paste("peaking/", p, "external_truck.RData", sep=""))
                  
                  #Combine auto and total into vehicle
                  vehicle <- get(paste(p, "external_auto", sep="_")) + get(paste(p, "external_truck", sep="_"))   
                  if(p%in%"daily") cat(paste("DailyExternal:",round(sum(vehicle),0)),sep="\n") 
                  rm(list=c(paste(p, "external_auto", sep="_"), paste(p, "external_truck", sep="_")))
                  
                  #Get Internal-Internal matrices
                  load(paste("peaking/", p, "vehicle.RData", sep=""))
                  save(list=paste(p, "vehicle", sep=""), file=paste("peaking/", p, "vehicle_ii.RData", sep="")) 
                  if(p%in%"daily") cat(paste("JEMNRDailyVehicle:",round(sum(dailyvehicle),0)),sep="\n")
                  
                  #Add Internal-Internal data to the external matrix
                  temp <- get(paste(p, "vehicle", sep=""))
                  temp <- temp + vehicle[rownames(temp),colnames(temp)]
                  assign(paste(p, "vehicle", sep=""),temp)   
                  rm(vehicle)
        
                  #SAVE RESULTS
                  save(list=paste(p, "vehicle", sep=""), file=paste("peaking/", p, "vehicle.RData", sep=""))       
                  rm(list=paste(p, "vehicle", sep=""))  
              } 
             
    }
