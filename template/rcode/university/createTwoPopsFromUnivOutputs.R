  #/
    #@filename createTwoPopsFromUnivOutputs.R
    #@author Martin Mann, martin.a.mann@odot.state.or.us
    #@date 11/10/14
    #@Description:
    #   This script creates two populations from the householdsOut.csv, and personsOut.csv file from the University model outputs and the 
    #   households.csv and persons.csv of the University model inputs (SYNPOP outputs).  One population ("SynPop_StdLst") removes the OSU students from 
    #   the Synthesized population and the other population ("SynPop_StdFacLst") removes the OSU students and OSU Faculity.  The former is applied with the 
    #   standard rData outputs found in inputs/RData.  These outputs are written out from this script.  The latter is applied on HBW trips only and the modified
    #   landuse representing this population is contained in a list which is applied in the following scripts: hbwGen.R, accessUtilities.R, tripDistribution.R , modeChoiceCommon.R.
    
########################## START #####################################################################

    local({
            
            runReCalib <- FALSE
            
            ## OTHER NEEDED FUNCTIONS #################
            source("rcode/university/calledFunctions/inputsSave_tazData.R")
            source("rcode/university/calledFunctions/access_forUniv.R") 
            source("rcode/university/calledFunctions/pregen.R")          
            
            if(file.exists("inputs/SynPop_StdLst.rData")) {
                SynPopLandUseLst <- list()
                for(curPop in c("SynPop_StdLst","SynPop_StdFacLst")){
                    prevSynPop <- unlist(loadFile(paste("inputs/",curPop,".rData",sep="")),recursive=F)
                    if(curPop == "SynPop_StdLst") taz <- prevSynPop$taz_census  #Reset TAZ to Original Census TAZ
                    index <- which(names(prevSynPop)%in%c("workerCoeffMtx","carCoeffMtx","kidCoeffMtx"))
                    SynPopLandUseLst[[curPop]] <- prevSynPop[index]
                    rm(prevSynPop)
                }
            }else{
                SynPopLandUseLst <- list("SynPop_StdLst"=list("workerCoeffMtx"=workerCoeffMtx,"carCoeffMtx"=carCoeffMtx,"kidCoeffMtx"=kidCoeffMtx),
                                         "SynPop_StdFacLst"=list("workerCoeffMtx"=workerCoeffMtx,"carCoeffMtx"=carCoeffMtx,"kidCoeffMtx"=kidCoeffMtx))
            }
            print(SynPopLandUseLst)
            
            ## PROCESS UNIVERSITY OUTPUTS ################
            uniIndx <- read.csv("unimodel/UniTAZs.csv")$TAZ
            #sum(taz[taz$TAZ%in%uniIndx,"EMPBASE"])
            #Read TAZ input file
            curTaz <- taz
            rownames(curTaz) <- curTaz$TAZ 
                
            #Read in outputs of SynPop Model          
            personsDF <- read.csv("unimodel/persons.csv", header=T, as.is=T)[,c("HHID","agep","relp")]
            housesDF <- read.csv("unimodel/households.csv", header=T, as.is=T)[,c("HHID","taz","tract","np","hhincAdj","nwrkrs_esr","bld","veh")]
            rownames(housesDF) <- housesDF$HHID 
            
            #Add age of householder
            agepDF <- personsDF[personsDF$relp%in%0,c("agep","HHID")]
            housesDF[as.character(agepDF$HHID),"ageRelp"] <- agepDF$agep     
            rm(personsDF)          
                                                     
            #Read in outputs of University Model to get students and workers in households
            personsOutDF <- read.csv("unimodel/outputs/personsOut.csv", header=T, as.is=T)
                                    
            #Use housesDF to remove grpQtrs persons
            personsOutDF <- personsOutDF[personsOutDF$hh_id%in%housesDF$HHID,]          
             
            #Create Household Weights by removing students and students and faculty        
            UniWkrHH <- tapply(personsOutDF$majorUniversityWorker,personsOutDF$hh_id,sum)
            housesDF$UniWkr <-  as.vector(UniWkrHH[rownames(housesDF)])           
            UniStdHH <- tapply(personsOutDF$majorUniversityStudent,personsOutDF$hh_id,sum)
            housesDF$UniStd <-  as.vector(UniStdHH[rownames(housesDF)])
            rm(personsOutDF)
            
            #Add students and faculty to housesDF
            housesDF$AdjNP_St <- housesDF$np - housesDF$UniStd  #adjust np per HH
            housesDF$HHWt_St <- housesDF$AdjNP_St/housesDF$np  #Calc HH wgt for no students            
            housesDF$AdjNP_StFc <- housesDF$np - (housesDF$UniStd + housesDF$UniWkr)  #adjust np per HH
            housesDF$HHWt_StFc <- housesDF$AdjNP_StFc/housesDF$np  #Calc HH wgt for no students and faculty            
            
            ## PROCESS TAZ FILE ######################                        
            SynPopLandUseLst[["SynPop_StdLst"]] <- c(SynPopLandUseLst[["SynPop_StdLst"]],SynPopLUToTAZ(housesDF,curTaz,"SynPop_StdLst"))
            SynPopLandUseLst[["SynPop_StdFacLst"]] <- c(SynPopLandUseLst[["SynPop_StdFacLst"]],SynPopLUToTAZ(housesDF,curTaz,"SynPop_StdFacLst"))
            print("Created Two Populations")
            
            ## RUN INPUTSSAVE and ACCESS ############## 
             
            #Create empFactor
            empFactor <- sum(housesDF$HHWt_St)/nrow(housesDF) #Percent houses without students
            for(curPop in  c("SynPop_StdLst","SynPop_StdFacLst")){

                #inputsSave_tazData() adds a modified modTaz dataframe, Remove the previous modified taz dataframe              
                SynPopLandUseLst[[curPop]] <- c(SynPopLandUseLst[[curPop]],inputsSave_tazData(SynPopLandUseLst[[curPop]],curPop,uniIndx,empFactor,UnivJEMNREmpFact))      
                SynPopLandUseLst[[curPop]] <- SynPopLandUseLst[[curPop]][-(which(names(SynPopLandUseLst[[curPop]])%in%"modTaz")[1])]                
                cat("Modified Univ Emp and Saved Two Populations rData")
                
                #Accessiblilty Measures
                SynPopLandUseLst[[curPop]] <- c(SynPopLandUseLst[[curPop]],access_forUniv(SynPopLandUseLst[[curPop]]))
                cat("Created Accessibilty Measures for Two Populations")
            }  
                        
            ## RUN A RECALIB OF SUBMODELS ##############
            # This may occur with a new synthetic population.  Since the syn pop is the new pums for the model a new puma, workerCoeffMtx, and carCoeffMtx are created
            if(runReCalib) {
                  file.rename("inputs/utilities/workerCoeffMtx.csv","inputs/utilities/workerCoeffMtx_prior.csv")
                  file.rename("inputs/utilities/carCoeffMtx.csv","inputs/utilities/carCoeffMtx_prior.csv")               
                  for(curPop in  c("SynPop_StdLst","SynPop_StdFacLst")){
                        print(paste("Run Recalibration of",curPop,"Car and Worker submodels"))
                        source("rcode/calib/calibSubModels.r")
                        CalibOut <- calibrateSub(SynPopLandUseLst[[curPop]],curPop)                    
                        SynPopLandUseLst[[curPop]][c("workerCoeffMtx","carCoeffMtx")] <- CalibOut[c("workerCoeffMtx","carCoeffMtx")]                   
                   }
                   print(paste("Finished Recalibration of submodels"))   
                   if(runReCalib) file.rename(paste("inputs/",pumaFileName,".csv",sep=""),paste("inputs/",pumaFileName,"_prior.csv",sep=""))     
                   runReCalib <- F            
            }         
            
            ## PREGEN ################################## 
            
            for(curPop in  c("SynPop_StdLst","SynPop_StdFacLst")) SynPopLandUseLst[[curPop]] <- c(SynPopLandUseLst[[curPop]],pregen(SynPopLandUseLst[[curPop]])) 
            print("Created Two Populations Pregen Measures")       
           
            ############################# OUTPUTS ################################# 

            SynPop_StdLst <- SynPopLandUseLst[["SynPop_StdLst"]]
            SynPop_StdFacLst <- SynPopLandUseLst[["SynPop_StdFacLst"]] 
            rm(SynPopLandUseLst)
            gc()

            # WRITE OUT SYNPOP WITHOUT STUDENTS AS GENERAL OUTPUTS ################           
            #Rename some old inputs as census based (if first previous student population doesn't exist)          
            
            #OUTPUT TO CSV            
            #PUMA
            write.csv(SynPop_StdLst[["puma"]],paste("inputs/",pumaFileName,".csv",sep=""),row.names=F)
            #TAZ_CENSUS
            write.csv(SynPop_StdLst[["taz_census"]],"inputs/taz_census.csv",row.names=F)             
            #TAZ
            write.csv(SynPop_StdLst[["modTaz"]],"inputs/taz.csv",row.names=F)                   
            #workerCoeffMtx
            write.csv( SynPop_StdLst[["workerCoeffMtx"]] ,"inputs/utilities/workerCoeffMtx.csv",row.names=F)       
            #carCoeffMtx
            write.csv(SynPop_StdLst[["carCoeffMtx"]] ,"inputs/utilities/carCoeffMtx.csv",row.names=F)              
            
            #OUTPUT TO WORKSPACE
            #Reestablish values in memory created in inputs.r using the outpus in SynPop_StdLst (Populaton without Students)
            taz <<- SynPop_StdLst[["modTaz"]]  
            workerCoeffMtx <<- SynPop_StdLst[["workerCoeffMtx"]]
            carCoeffMtx <<- SynPop_StdLst[["carCoeffMtx"]]       
            
            #OUTPUT TO RDATA
            rDataFldr <- c("puma","pumsHHSyn","colveh","totalEmp","afrEmp","minEmp","conEmp","finEmp","gvtEmp","mfgEmp","retEmp","svcEmp","tcpEmp","wstEmp","percentSingleFamily","hhs","hiazAry")
            pregenFldr <- c("mixthm","mixrhm","tot30t","mixRetP","mixRetA","mixTotA","whiazAry","whiazcAry","cvalAry","cvalIndexAry","khiazAry","visitorWhiazcAry")
            #Create rData sets for NoStudent Population 
            if(!file.exists("pregen")) dir.create("pregen")
            sapply(c(rDataFldr,pregenFldr),function(x) {
                 assign(x,SynPop_StdLst[[x]])
                 if(x %in% pregenFldr) save(list=c(x),file=paste("pregen/",x,".RData",sep=""))
                 if(x %in% rDataFldr)save(list=c(x),file=paste("inputs/RData/",x,".RData",sep=""))
            })
            
            ###### SAVE LIST OF SYNPOP WITHOUT STUDENTS AND FACULITY FOR HBW TRIPS ######
            
            save(SynPop_StdLst,file="inputs/SynPop_StdLst.RData")                        
            save(SynPop_StdFacLst,file="inputs/SynPop_StdFacLst.RData")
            rm(SynPop_StdLst,SynPop_StdFacLst)
       })
            
###################################### END #########################################################################################






