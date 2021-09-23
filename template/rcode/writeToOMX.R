    #/
    #@filename writeToBank.R
    #@author Ben Stabler
    #@date 7/2/04
    #
    #This script writes the auto and bus demand to the databank
    # BTS 03/14/12 - updated to write matrices to either batchin format or directly to the databank
    # BTS - updated to work with EMME 4
    # Revised by Martin Mann 10/09/14 - Rewrote the method of adding the university and commercial OD matrices to the JEMNR OD matrices as there was an error in the result.
    # Also shifted the location in the script of the dailybus write to EMME as it did not include the university output.
    #Revised by Martin Mann 11/10/14 - minor syntax formatting change - no impact to code.
    # Michael McCarthy, 4/12/21 - Write to OMX instead of Emme matrix

    ####### DAILY AUTO AND BUS ##########################################################
    # Supress warnings when writing matrix files
    options(warn=-1)

    load("peaking/dailyvehicle.RData")
	  load("peaking/dailybus.RData")

    cat(paste("JEMNRDailyBus:",round(sum(dailybus),0)),sep="\n")

	  #Add commercial vehicles if they are to be considered
    if(as.logical(runCommercialVehicleModel)) {
		    load("peaking/dailycommercialvehicle.RData")
        cat(paste("DailyCommercialVehicle:",sum(dailycommercialvehicle)),sep="\n")
        dailyvehicle[rownames(dailycommercialvehicle),colnames(dailycommercialvehicle)] <- 	dailyvehicle[rownames(dailycommercialvehicle),colnames(dailycommercialvehicle)] +	dailycommercialvehicle
    }

    #Add university bus and vehicle trips if the University Model was run
    if(as.logical(runUniversityModel)) {
		    load("peaking/dailyvehicleuniversity.RData")
        sum(dailyvehicleuniversity)
	      cat(paste("DailyVehicleUniversity:",sum(dailyvehicleuniversity)),sep="\n")
        dailyvehicle[rownames(dailyvehicleuniversity),colnames(dailyvehicleuniversity)] <- 	dailyvehicle[rownames(dailyvehicleuniversity),colnames(dailyvehicleuniversity)] +	dailyvehicleuniversity
		    load("peaking/dailybusuniversity.RData")
        cat(paste("DailyBusUniversity:",sum(dailybusuniversity)),sep="\n")
        dailybus[rownames(dailybusuniversity),colnames(dailybusuniversity)] <- 	dailybus[rownames(dailybusuniversity),colnames(dailybusuniversity)] +	dailybusuniversity
    }

    # Write daily demand to OMX
    dailybus[is.na(dailybus)] <- 0
    dailyvehicle[is.na(dailyvehicle)] <- 0

    if(file.exists("outputs/matrices/demand_daily.omx")){
      unlink("outputs/matrices/demand_daily.omx") # Easier to delete + replace than overwrite
    }
    createFileOMX("outputs/matrices/demand_daily.omx", Numrows = nrow(dailyvehicle), Numcols = ncol(dailyvehicle))
    zoneLookup = dimnames(dailyvehicle)[[1]]
    writeLookupOMX("outputs/matrices/demand_daily.omx", zoneLookup, "NO")

    writeMatrixOMX("outputs/matrices/demand_daily.omx", dailybus, "dailybus")
    writeMatrixOMX("outputs/matrices/demand_daily.omx", dailyvehicle, "dailyvehicle")


    ####### PM1 AUTO ################################################################
    load("peaking/pm1vehicle.RData")

	  #Add commercial vehicles if they are to be considered
    if(as.logical(runCommercialVehicleModel)) {
		    load("peaking/pm1commercialvehicle.RData")
        pm1vehicle[rownames(periodcommercialvehicle),colnames(periodcommercialvehicle)] <- 	pm1vehicle[rownames(periodcommercialvehicle),colnames(periodcommercialvehicle)] +	periodcommercialvehicle
    }

	  #Add university bus and vehicle trips if the University Model was run
    if(as.logical(runUniversityModel)) {
		    load("peaking/pm1vehicleuniversity.RData")
	      pm1vehicle[rownames(pm1vehicleuniversity),colnames(pm1vehicleuniversity)] <- 	pm1vehicle[rownames(pm1vehicleuniversity),colnames(pm1vehicleuniversity)] +	pm1vehicleuniversity
    }

    #Write to EMME4
    pm1vehicle[is.na(pm1vehicle)] <- 0

    if(file.exists("outputs/matrices/demand_peak.omx")){
      unlink("outputs/matrices/demand_peak.omx")
    }
    createFileOMX("outputs/matrices/demand_peak.omx", Numrows = nrow(pm1vehicle), Numcols = ncol(pm1vehicle))
    zoneLookup = dimnames(pm1vehicle)[[1]]
    writeLookupOMX("outputs/matrices/demand_peak.omx", zoneLookup, "NO")

    writeMatrixOMX("outputs/matrices/demand_peak.omx", pm1vehicle, "pm1vehicle")

    ####### SAVE MATRICES AS RDATA ###################################################

    save(dailyvehicle, file=paste("peaking/dailyvehicle.RData", sep=""))
    save(dailybus, file=paste("peaking/dailybus.RData", sep=""))
    save(pm1vehicle , file=paste("peaking/pm1vehicle.RData", sep=""))

    ####### PEAK AND OFFPEAK BUS ASSIGNMENT ############################################

    if(runPeakOffPeakTransitAssignment){
        load("peaking/pkadbus.RData")
        pkadbus[is.na(pkadbus)] <- 0

        load("peaking/opadbus.RData")
        opadbus[is.na(opadbus)] <- 0

        writeMatrixOMX("outputs/matrices/demand_peak.omx", pkadbus, "pkadbus")
        writeMatrixOMX("outputs/matrices/demand_peak.omx", opadbus, "opadbus")
    }

    # Close any open OMX/HDF5 files
    H5close()

    setwd(basedir)

################################################ END ######################################################
