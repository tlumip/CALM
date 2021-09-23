  #/
    #@filename inputsSave_tazData.R
    #@author Martin Mann, martin.a.mann@odot.state.or.us
    #@date 10/04/14
    #@Description:
    #   Modifies employment, creates hiazAry, pulls hhsize and percent single family for two populations
    #   Returns the results to calling function 

########################## START #####################################################################
   
    inputsSave_tazData <- function(curSynPopData,curPop,uniIndx,empFactor,UnivJEMNREmpFact) {    
               
                curPuma <- curSynPopData[["puma"]]
                curTaz <- curSynPopData[["modTaz"]]

                #Adjust College
                curTaz[curTaz$TAZ%in%uniIndx,"COLEBASE"] <- 0
                colveh <- c(rep(0,length(externalZones)), curTaz$COLEBASE)
                names(colveh) <-  zoneNames
                               
                #Employment By Zone
                empCats <- c("EMPBASE","AFREMP","MINEMP","CONEMP","FINEMP","GVTEMP","MFGEMP","RETEMP","SVCEMP","TCPEMP","WSTEMP")
                if(curPop%in%"SynPop_StdFacLst") curTaz[curTaz$TAZ%in%uniIndx,empCats] <- 0
                if(curPop%in%"SynPop_StdLst") curTaz[curTaz$TAZ%in%uniIndx,empCats] <- curTaz[curTaz$TAZ%in%uniIndx,empCats] * empFactor * UnivJEMNREmpFact
                
                #Agriculture and forestry employment by zone                 
                afrEmp <- c(rep(0, length(externalZones)), curTaz$AFREMP)
                names(afrEmp) <- zoneNames 

                #Mining employment by zone
                minEmp <- c(rep(0, length(externalZones)), curTaz$MINEMP)
                names(minEmp) <- zoneNames 
                
                #Construction employment by zone
                conEmp <- c(rep(0, length(externalZones)), curTaz$CONEMP)
                names(conEmp) <- zoneNames 
               
                #Financial, insurance, and real estate employment by zone
                finEmp <- c(rep(0, length(externalZones)), curTaz$FINEMP)
                names(finEmp) <- zoneNames 
               
                #Government employment by zone
                gvtEmp <- c(rep(0, length(externalZones)), curTaz$GVTEMP)
                names(gvtEmp) <- zoneNames 
                
                #Manufacturing employment by zone
                mfgEmp <- c(rep(0, length(externalZones)), curTaz$MFGEMP)
                names(mfgEmp) <- zoneNames 
               
                #Retail employment by zone
                retEmp <- c(rep(0, length(externalZones)), curTaz$RETEMP)    
                names(retEmp) <- zoneNames 
                
                #Services employment by zone
                svcEmp <- c(rep(0, length(externalZones)), curTaz$SVCEMP)
                names(svcEmp) <- zoneNames     
                
                #Transportation, communication, and public utilities employment by zone
                tcpEmp <- c(rep(0, length(externalZones)), curTaz$TCPEMP)
                names(tcpEmp) <- zoneNames     
                
                #Wholesale trade employment by zone
                wstEmp <- c(rep(0, length(externalZones)), curTaz$WSTEMP)
                names(wstEmp) <- zoneNames     
                
                #Total Employment by Zone
                totalEmp <- afrEmp + minEmp + conEmp + finEmp + gvtEmp + mfgEmp + retEmp + svcEmp + tcpEmp + wstEmp
                names(totalEmp) <- zoneNames 
                
                #Percent Single Family by zone
                percentSingleFamily <- curTaz$percentSingleFamilyDetached
                percentSingleFamily <- c(rep(0, length(externalZones)), percentSingleFamily)    
                names(percentSingleFamily) <- zoneNames 
                
                #households by zone
                hhs <- c(rep(0, length(externalZones)), curTaz$HHBASE)
                names(hhs) <- zoneNames
                
                ## PROCESS PUMS ##########################

                #h flows fastest, followed by i then a
                hiaAry <- array(curPuma$puma, c(4,4,4))
                dimnames(hiaAry) <- list(hs, hi, ha)
                hiazAry <- array(0, c(4,4,4,nrow(taz)))    
                #Create 3way cross tab for zones
                for(i in 1:nrow(curTaz)) {
                    hiazAry[,,,i] <- ipf(cbind(
                        unlist(curTaz[i,hs])*curTaz[i,"HHBASE"],
                        unlist(curTaz[i,hi])*curTaz[i,"HHBASE"],
                        unlist(curTaz[i,ha])*curTaz[i,"HHBASE"]), hiaAry)
                }          
                extAry <- array(0, c(4,4,4,length(externalZones)))
                hiazAry <- array(c(extAry, hiazAry), c(4,4,4,length(zones)))
                dimnames(hiazAry) <- list(segmentsH,segmentsI,segmentsA,zoneNames)                
                
                curTazLst <- list(curTaz, colveh, totalEmp,  afrEmp, minEmp, conEmp, finEmp, gvtEmp, mfgEmp, retEmp, svcEmp, tcpEmp, wstEmp, percentSingleFamily,hhs,hiazAry)                
                names(curTazLst) <- c("modTaz","colveh","totalEmp","afrEmp","minEmp","conEmp","finEmp","gvtEmp","mfgEmp","retEmp","svcEmp","tcpEmp","wstEmp","percentSingleFamily","hhs","hiazAry")
                return(curTazLst)                                      
            }
    
########################################## END #################################################################


#sumPrior <- round(apply(hiazAryPrior,c(4),sum),0)
#names(sumPrior) <- zoneNames
 #sum(sumPrior[uniIdx])
 
#sumNew<- round(apply(hiazAry,c(4),sum),0)
#names(sumNew) <- zoneNames
# sum(sumNew[uniIdx])
