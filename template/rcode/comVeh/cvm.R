#Oregon Commercial Vehicle 3-Step Model
#Yegor Malinovskiy, malinovskiyy@pbworld.com
#Ben Stabler, stabler@pbworld.com 
#9/23/2013
      
          ###############################################
          #################USER MANAGED##################
          ###############################################
          

          ###################INPUTS######################
          
    local({ 
             
          options(stringsAsFactors=T,scipen=12,digits=12)          
          ##Maximum # of iterations in trip balancing
          balance_iter <- maxIter_Commercial
          
          ##File names
          #TAZ data (employment)
          tazFileName <-"inputs/taz.csv"
          if(as.logical(runUniversityModel))  tazFileName <-"inputs/taz_census.csv"
          
          #Productions
          nonWorkProductionFileName <- "inputs/cvm/nonWorkProd_IntraTrips.csv"
          workProductionFileName <- "inputs/cvm/workProd_IntraTrips.csv"
          
          #Attractions
          nonWorkAttractionFileName <- "inputs/cvm/nonWorkAttr.csv"
          workAttractionFileName <- "inputs/cvm/workAttr.csv"
          
          #Friction factor
          nonWorkFrictionFileName <- "inputs/cvm/nonWorkFriction.csv"
          workFrictionFileName <- "inputs/cvm/workFriction.csv"
          
          #In vehicle time skim
          skimFileName <- "inputs/RData/ivTimepeakdriveAlone.RData"
          
          #Time of day filenames
          carTODFileName <- "inputs/cvm/TOD_Car.csv"
          suTODFileName <- "inputs/cvm/TOD_SUTruck.csv"
          muTODFileName <- "inputs/cvm/TOD_MUTruck.csv"

          ###################OUTPUTS######################
          
          ##Outputs Directory
          #Matrix output filename prefixes (suffixes by TOD period added later)
          
          carOut <- "peaking/cvm/CAR"
          suOut <- "peaking/cvm/SU"
          muOut <- "peaking/cvm/MU"

          ###############################################
          ##################MAIN SCRIPT##################
          ###############################################
          
          
          ####################SETUP#######################
          #TAZ inputs (employment by type)
          tazData <- read.csv(tazFileName, header = TRUE)
          
          #Productions inputs
          nwkProd <- read.csv(nonWorkProductionFileName, header = TRUE, row.names=1)
          wkProd <- read.csv(workProductionFileName, header = TRUE, row.names=1)
          
          #Attractions inputs
          nwkAttr <- read.csv(nonWorkAttractionFileName, header = TRUE, row.names=1)
          wkAttr <- read.csv(workAttractionFileName , header = TRUE, row.names=1)
          
          #Friction factor inputs
          nwkFriction <- read.csv(nonWorkFrictionFileName, header = TRUE, row.names=1)
          wkFriction <- read.csv(workFrictionFileName, header = TRUE, row.names=1)
          
          #Travel time skim inputs
          load(skimFileName)
          
          #Time of day 
          carTOD <- read.csv(carTODFileName, header = TRUE)
          suTOD <- read.csv(suTODFileName, header = TRUE)
          muTOD <- read.csv(muTODFileName, header = TRUE)
          
          
          ##TAZ limits (internal TAZs only)
          allTAZs <- as.integer(unlist(dimnames(ivTimepeakdriveAlone)[1]))
          minTAZ <- which(allTAZs == max( min(allTAZs), min(tazData$TAZ) ))
          maxTAZ <- which(allTAZs == min( max(allTAZs), max(tazData$TAZ) ))      
          
          ###############TRIP GENERATION##################
          
          ##FUNCTION TO CROSS MULTIPLY PRODUCTIONS OR ATTRACTIONS BY EMPLOYMENT
          crossMult <- function(workMat, mode) {
          	return(		tazData$AFREMP*(workMat["Agr_Forest",mode]) +
          				tazData$MINEMP*(workMat["Mining",mode]) +
          				tazData$CONEMP*(workMat["Construction",mode]) +
          				tazData$MFGEMP*(workMat["Manufacturing",mode]) + 
          				tazData$TCPEMP*(workMat["Transp_Comm_PU",mode]) +
          				tazData$WSTEMP*(workMat["Wholesale",mode])+
          				tazData$RETEMP*(workMat["Retail",mode])+
          				tazData$FINEMP*(workMat["Financial",mode])+
          				tazData$SVCEMP*(workMat["Service",mode])+
          				tazData$GVTEMP*(workMat["Government",mode]))
          }         
          
          ##CAR MODE PRODUCTIONS/ATTRACTIONS
          mode <- "Car"
          tazData$ProdCar_Wk <- crossMult(wkProd, mode)
          tazData$AttrCar_Wk <- crossMult(wkAttr, mode)
          
          tazData$ProdCar_NWk <- crossMult(nwkProd, mode)
          tazData$AttrCar_NWk <- crossMult(nwkAttr, mode)
          
          
          ##SINGLE UNIT TRUCK MODE PRODUCTIONS/ATTRACTIONS
          mode <- "SU.Truck"
          tazData$ProdSU_Wk <- crossMult(wkProd, mode)
          tazData$AttrSU_Wk <- crossMult(wkAttr, mode)
          
          tazData$ProdSU_NWk <- crossMult(nwkProd, mode)
          tazData$AttrSU_NWk <- crossMult(nwkAttr, mode)
          
          
          ##MULTI UNIT TRUCK MODE PRODUCTIONS/ATTRACTIONS
          mode <- "MU.Truck"
          tazData$ProdMU_Wk <- crossMult(wkProd, mode)
          tazData$AttrMU_Wk <- crossMult(wkAttr, mode)
          
          tazData$ProdMU_NWk <- crossMult(nwkProd, mode)
          tazData$AttrMU_NWk <- crossMult(nwkAttr, mode)
          
          
          cat(paste("Sum of Productions Work: ", sum(tazData$ProdMU_Wk + tazData$ProdSU_Wk + tazData$ProdCar_Wk)),sep="\n")
          cat(paste("Sum of Productions Non-Work: ", sum(tazData$ProdMU_NWk + tazData$ProdSU_NWk + tazData$ProdCar_NWk)),sep="\n")
          
          ###############TRIP DISTRIBUTION################
          
          ##FRICTION FACTORS
          ##FUNCTION TO GET FRICTION FACTOR MATRIX
          getFrictionMat <- function(friMat, skim, mode) {
          	r <- friMat["r",mode]
          	s <- friMat["s",mode]
          	b <- friMat["b",mode]
          	q <- friMat["q",mode]
          	return(exp((b*skim)+((r + s*skim)/(1+q*skim^2))))
          }
          
          ##TRIP TABLES
          ##FUNCTION TO GET TRIP TABLE
          getTripTable <- function(friMat, attr, prod, mode) {
          	n <- length(attr)
          	tempMat <- matrix(rep(NA, n^2), nrow=n, ncol=n)
          	skim <- ivTimepeakdriveAlone[minTAZ:maxTAZ,minTAZ:maxTAZ]
          	diag(skim)<- 1111111111  #Removes intrazonal - MM
          	f <- getFrictionMat(friMat, skim, mode)     
          	norm <- colSums(t(f)*attr)
          	tripMat <- prod*(t(attr*t(f))/norm)
          	tripMat[is.nan(tripMat)] = 0 #Prevent NaNs
          	#diag(tripMat)
          	return (tripMat)
          }
          
          ##ROOT MEAN SQUARE ERROR
          rmse<-function(v1,v2) {
          	e <- v1-v2;
          	sqrt(sum(e^2)/(length(e)))
          }
          
          ##FUNCTION TO BALANCE TRIP TABLE ON ATTRACTIONS
          balanceTrips <- function(trip_mat, friMat, attr, prod, mode, max_rmse=1e-5, max_iter=balance_iter) {
          	new_rmse<-rmse(attr,colSums(trip_mat))
          	iter=0
          	while( new_rmse>max_rmse && iter<max_iter ) {
          		adj_Attr <- attr/(colSums(trip_mat))
          		adj_Attr[is.nan(adj_Attr)] = 0 #Prevent NaNs
          		adj_Attr[is.infinite(adj_Attr)] = 0 #Prevent InFs
          		if(iter == 0){next_iter_attr <- attr} else {next_iter_attr <- next_iter_attr*adj_Attr}
          		trip_mat <- getTripTable(friMat, next_iter_attr, prod, mode)
          		trip_mat[is.nan(trip_mat)] = 0 #Prevent NaNs
          		new_rmse<-rmse(attr,colSums(trip_mat)) 
          		iter<-iter+1
          		#cat("Iteration",iter,"RMSE:",new_rmse,"\n")
          	}
          	return(trip_mat)
          }
          
          
          mode <- "Car"
          WORK_CAR_TRIPS <- balanceTrips(getTripTable(wkFriction, tazData$AttrCar_Wk, tazData$ProdCar_Wk, mode), wkFriction, tazData$AttrCar_Wk, tazData$ProdCar_Wk, mode)
          NWORK_CAR_TRIPS <- balanceTrips(getTripTable(nwkFriction, tazData$AttrCar_NWk, tazData$ProdCar_NWk, mode), nwkFriction, tazData$AttrCar_NWk, tazData$ProdCar_NWk, mode)
          
          mode <- "SU.Truck"
          WORK_SU_TRIPS <- balanceTrips(getTripTable(wkFriction, tazData$AttrSU_Wk, tazData$ProdSU_Wk, mode), wkFriction, tazData$AttrSU_Wk, tazData$ProdSU_Wk, mode)
          NWORK_SU_TRIPS <- balanceTrips(getTripTable(nwkFriction, tazData$AttrSU_NWk, tazData$ProdSU_NWk, mode), nwkFriction, tazData$AttrSU_NWk, tazData$ProdSU_NWk, mode)
          
          mode <- "MU.Truck"
          WORK_MU_TRIPS <- balanceTrips(getTripTable(wkFriction, tazData$AttrMU_Wk, tazData$ProdMU_Wk, mode), wkFriction, tazData$AttrMU_Wk, tazData$ProdMU_Wk, mode)
          NWORK_MU_TRIPS <- balanceTrips(getTripTable(nwkFriction, tazData$AttrMU_NWk, tazData$ProdMU_NWk, mode), nwkFriction, tazData$AttrMU_NWk, tazData$ProdMU_NWk, mode)
          
          
          cat("Sum of Matrix Work: ", sum(WORK_CAR_TRIPS + WORK_SU_TRIPS + WORK_MU_TRIPS))
          cat("Sum of Matrix Non-Work: ", sum(NWORK_CAR_TRIPS + NWORK_SU_TRIPS + NWORK_MU_TRIPS))
          
          ##############TIME OF DAY CHOICE################
          
          #Generate OD Matrix by multiplying by appropriate TOD factors
          generateODMatrix <- function(tod, period, w_trips, nw_trips) {
          	startTime <- as.integer(TOD_periods[TOD_periods["Period"] == period][2])
          	endTime <- as.integer(TOD_periods[TOD_periods["Period"] == period][3])
           	return(	sum( tod$From.Work.to.Visit	[ tod$Time[!is.na(tod$Time)] >= startTime & tod$Time[!is.na(tod$Time)] < endTime ])*(w_trips) +
          			sum( tod$From.Visit.to.Work	[ tod$Time[!is.na(tod$Time)] >= startTime & tod$Time[!is.na(tod$Time)] < endTime ])*t(w_trips) +
          			sum( tod$Visit.to.Visit		[ tod$Time[!is.na(tod$Time)] >= startTime & tod$Time[!is.na(tod$Time)] < endTime ])*(nw_trips)+
          			sum( tod$Visit.to.Visit		[ tod$Time[!is.na(tod$Time)] >= startTime & tod$Time[!is.na(tod$Time)] < endTime ])*t(nw_trips) )
          }
          
          #Generate OD Matrix by multiplying by appropriate TOD factors
          generateODMatrixTotal <- function(tod, w_trips, nw_trips) {
           	return(	sum( tod$From.Work.to.Visit )*(w_trips) +
          			sum( tod$From.Visit.to.Work )*t(w_trips) +
          			sum( tod$Visit.to.Visit	    )*(nw_trips)+
          			sum( tod$Visit.to.Visit     )*t(nw_trips) )
          }      
          
          #Write tables to folder
          n <- sqrt(length(WORK_CAR_TRIPS))
          dailycommercialvehicle <- matrix(rep(0, n^2), nrow=n, ncol=n)
          
          #assign the total to dailycommercialvehicle
          car <- generateODMatrixTotal(carTOD, WORK_CAR_TRIPS, NWORK_CAR_TRIPS)
          su <- generateODMatrixTotal(suTOD, WORK_SU_TRIPS, NWORK_SU_TRIPS)
          mu <- generateODMatrixTotal(muTOD, WORK_MU_TRIPS, NWORK_MU_TRIPS)
          save(su, file=paste("peaking/sglUnitTruck.RData", sep=""))  
          save(mu, file=paste("peaking/multiUnitTruck.RData", sep=""))
          dailycommercialvehicle <- dailycommercialvehicle + car + su + mu
          dailycommercialvehicle <-  (dailycommercialvehicle +t(dailycommercialvehicle)) /2   #MM 06/19/16 Cleanup balancing daily 
          
          #Boost Commercial Vehicle - MM
          taz_forCVM <- read.csv(tazFileName)          
          taz_forCVM <- taz_forCVM[,c("TAZ","HHBASE","RETEMP","MFGEMP","SVCEMP")]
          attach(taz_forCVM)
          CVMTrips <- "0.388 * HHBASE + 1.208 * RETEMP + 1.36 * MFGEMP + 0.514 * SVCEMP"
          CVMVec <- eval(parse(text=CVMTrips))
          names(CVMVec) <- TAZ
          detach(taz_forCVM)
              
          dailycommercialvehicle <- (dailycommercialvehicle*(sum(CVMVec)/sum(dailycommercialvehicle)))*CVMFactor  #- MM
          
          #save daily results to add to total dailyvehicle
          if(!file.exists("peaking")) dir.create("peaking") 
          if(!file.exists("peaking/cvm")) dir.create("peaking/cvm") 	
          save(dailycommercialvehicle, file=paste("peaking/dailycommercialvehicle.RData", sep=""))
                
          #Periods used in JEMnR
          #periods <- colnames(get(paste("vehicle", "PeriodFactors", sep="")))
          periods <- TOD_periods["Period"][,1]
          periodcommercialvehicle <- matrix(rep(0, n^2), nrow=n, ncol=n)      
          
          #generate CSVs by time period
          for(i in 1:length(periods))
          {
          	if(periods[i] != "daily")
          	{
          		car <- generateODMatrix(carTOD, periods[i], WORK_CAR_TRIPS, NWORK_CAR_TRIPS)
          		print(paste("Car trips ", periods[i], ": ", sum(car)))
          		su <- generateODMatrix(suTOD, periods[i], WORK_SU_TRIPS, NWORK_SU_TRIPS)
          		print(paste("Single-unit truck trips ", periods[i], ": ", sum(su)))
          		mu <- generateODMatrix(muTOD, periods[i], WORK_MU_TRIPS, NWORK_MU_TRIPS)
          		print(paste("Multi-unit truck trips ", periods[i], ": ", sum(mu)))
          	
          		write.table(car, paste(carOut,"_",periods[i],".csv",sep = ""), sep=",", row.names=TRUE, col.names=NA)
          		write.table(su, paste(suOut,"_",periods[i],".csv",sep = ""), sep=",", row.names=TRUE, col.names=NA)
          		write.table(mu, paste(muOut,"_",periods[i],".csv",sep = ""), sep=",", row.names=TRUE, col.names=NA)
          	
          		periodcommercialvehicle <- car + su + mu
          		periodcommercialvehicle <- periodcommercialvehicle*(sum(CVMVec)/sum(dailycommercialvehicle))  #Boost PM CV Trips
          		save(periodcommercialvehicle, file=paste("peaking/",periods[i],"commercialvehicle.RData", sep=""))
          	}
          }
          
          cat("Sum of Daily Commercial: ", round(sum(dailycommercialvehicle),0))
          cat("Sum of PMPeak Commercial: ", round(sum(periodcommercialvehicle),0))
    })


############################################################## END ##########################################################################