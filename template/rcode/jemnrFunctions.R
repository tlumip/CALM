#/
#@filename jemnrFunctions.R
#@author Ben Stabler benjamin.stabler@odot.state.or.us
#@date 4/16/04
#@version 1.2
#Functions for JEM-n-R
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
#
#REVISIONS

# Revised 1/26/04 benjamin.stabler@odot.state.or.us

# Revised 10/28/05 brian.j.gregor@odot.state.or.us

# Revised 3/25/04 Added readCoeffs()

# Revised to work with the fun list and to source inall the other module functions defined in other files.

# 10/28/05: Removed the loading of most of the model functions. That was moved to the "runModel" script using a table listing of scripts in (functiontable.csv). Now this script only loads a set of utility functions.

# Revised sometime in '07 or '08, a.bettinardi, 
#   1. corrected balance functionTo close on both sides of the matrix (ipf was only closing on one side).

# 03/14/12 BTS - added new functions: batch_and_read_mf, write_mf_and_batch_in, read.mf.csv, write.mf.csv, write.mf.batchin, calcRmse (moved from runModel.R), readEmme2 (from OSUM)

# 06/25/14 YMM - seed matrices are trimmed of zero cols and rows prior to balancing and are reconstituted after

# 10/9/2014, Martin Mann:
  #1_Added an vector interleaving function
  #2_Removed the first balance function loaded since it is no longer used
  #3_Added a function to talley distributions by multiple groupings
  #4_Modifed currently used matrix balancing function that removed zero values with one that just replaces NA's with zeros.  The previous method created errors with my matrix

# 11/10/2014, Martin Mann:
 #1.	Removed the vector interleaving function and the TalleyDist function added previously
 #2.	Added a function," SynPopLUToTAZ", to process SynPop  data to the TAZ  and puma data.  Used in inputs.R for non university use of SynPop.   Also used in createTwoPopsFromUnivOutputs.R  for same purpose but with SynPop adjusted for OSU students and/or faculty .
 #3.	Added functions to load RData files.  loadFile loads multiple objects from an rData file.  loadListObj loads specific objects from a list in an RData file.  The former  is used in in "createTwoPopsFromUnivOutputs.R " .  The latter is used in the modified JEMNR functions listed above
 
 # 01/12/2015, Martin Mann:
 #1.  Remove flat.to.mat - no longer called as it has been incorporated into readEmme2 function
 #2.  Remove summary.array, rDoc, rDocRecursive, read.mf.csv, write.mf.csv - no longer called
 #3.  Renamed write.mf.batchin to writeEmme2 and write_mf_and_batch_in to write_and_batch_mf to be consistent with read functions
 #4.  Reformatted to make format more uniform and easier to read 

# 3/31/2016, Alex Bettinardi
 #1.  Corrected the ipf function the marginCoeff object was incorrectly being created only for the first dimension, 
 #    the correction now calculates marginCoeff for each dimension that is being ipf.
 #    This actually is not an error if all dimensions have the same length (like the 4x4x4 hias), 
 #    however it could be a problem if the ipf is ever applied to an array that has dimensions of varying length.
 #    There was also an issue here, where the setup incorrectly did not allow arrays to get out of the while loop.
 #    This statement was added - 'marginCoeff[marginTotal==0] <- 1' - so that cells that were not being updated would not 
 #    impact the closure criteria of the loop.  This removed many erroneous "stopped due to iterations errors". 
 #    Additionally, the "margins not equal" warnings was revised so that it only reports if there is really an issue to be concerned about.

  # 04/11/2016, Martin Mann:
 #1.  Updated  - SynPopLUToTAZ function to not round the household base totals (HHBASEDF)
 
 #  06/23/16< Martin Mann
 # Modified IPF output warning to post a table of the number of iterations taken to close

  # 1/12/2017< Martin Mann
  # Modified SynPopLUToTAZ() function at UPDATE TAZ FILE WITH SYNPOP SECTION remove GQ before update, add back after updating

    cat("JEM-n-R Function Definitions\n\n")
    
    #/
    #Function Defintions
    #This section of code defines common functions used in JEM-n-R
    #/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    #Define a fun list to store all model functions.
    #This list is then attached to the workspace so the functions can be references without refering to the list.
    #@param null
    #@return fun - list of functions (currently null)
    ##/
    fun <- list()
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # createSetting - function to read and assign setting.csv
    # @param settings - from settings.csv
    # @return <various parameters> 
    # Called hbwGen.r    
    ##/ 
    
    fun$createSettings <- function(settings) {
                   	    options(warn=-1)
                        for(i in 1:nrow(settings)) { 
                    		    x <- as.numeric(as.character(settings$value[i]))
                    		    if(is.na(x)) x <- as.character(settings$value[i])
                    		    if(x == "NULL") x <- NULL
                    		    assign(as.character(settings$setting[i]), x, pos=1) 
                        }
                        options(warn=0)
                      }
                      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # getDims - function to get the dimension number for an array dimension by name
    # @param object - R object to search for dimension names
    # @param dimensions - string representations of dimensions to return dimension number
    # @return dimNumbers dimension numbers as a vector
    # Called hbwGen.r    
    ##/    
    
    fun$getDims <- function(object, dimensions) {
                        if(!all(dimensions %in% names(dimnames(object)))) { stop("Dimension not found") }
                        match(dimensions, names(dimnames(object)))
                    }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # distsum - function to create district summariy reports
    # adapted from collapse.v2 by Brian Gregor (ODOT)
    # revisions 1/23/04 by Ben Stabler (ODOT)
    #   1) new district input format,
    #   2) system independent (no system commands)
    #   3) R only code (removed awk script)
    #   4) added districts parameter
    # revised 3/10/04 Ben Stabler to work with 3D array
    # Assumes 3rd dimension is dimension to loop over so it collapsed the 1st and 2nd dimension for each 3rd dimension element
    # @param:
         # INPUT    DESCRIPTION          EXAMPLE
         # mat      matrix name          "mf.colldt"
         # desc     matrix description   "College Distribution"
         # ens      ensemble             "ga"
         # digits   max decimal points    3
         # rpt      report file name     "dist"
         # proj     project name*        "R Test"
         # init     user initials*       "wrs"
         # districts district definitions districts
         # example:  
         # distsum ("mf.colldt", "College Distribution", "ga", 3, "dist", project, initials)
    # @return null
    # Called by calcTripsByMode.r, hbschMcByType.r, balanceDist.r, tripDistribution.r    
    ##/
   
    fun$distsum <- function (mat, desc, ens, digits, rpt, proj, init, districts) {  
                       #Only works with vectors, 2D matrices, and 3D arrays
                       if(is.vector(get(mat, envir=sys.parent()))) {
                          type <- "1d"
                       }else if(is.matrix(get(mat, envir=sys.parent()))) {
                          type <- "2d"
                       }else if(length(dim(get(mat, envir=sys.parent()))) == 3) {
                          type <- "3d"
                       }else{
                          stop("ERROR:  unsupported data type (not a one, two or three dimensional object)")
                       }
                     
                       #Set output width
                       oldOptions <- options()
                       options('width'=80) #80 character page width
                     
                       #Print header lines
                       if (file.exists(paste(rpt, ".rpt", sep=""))) {
                          outfile <- file(paste(rpt, ".rpt", sep=""), "a+")
                          writeLines ("\n\f\n", con=outfile, sep="")
                       }else{
                          outfile <- file(paste(rpt, ".rpt", sep=""), "a+")
                       }
                     
                       writeLines(proj, con=outfile, sep="\n")
                       writeLines(paste("JEM-n-R (", toupper(init), ") - ", date(), sep=""),
                         con=outfile, sep="\n")
                       writeLines(paste(mat, desc, sep=" - "), con=outfile, sep="\n")
                     
                       #Load ensemble
                       enssize <- nrow(districts)
                       numdist <- length(unlist(unique(districts[ens])))
                       dist.name <- sort(unlist(unique(districts[ens])))
                     
                       for(x in 1:numdist) {
                         if(x < 10) {
                             dist.name[x] <- paste(ens, "0", dist.name[x], sep="")
                         }else{
                            dist.name[x] <- paste(ens, dist.name[x], sep="")
                         }
                       }
                       dist.name <- append(dist.name, "SUM")
                     
                       #Test for number of zones
                       if(type == "2d") {
                          numzones <- sqrt(length(get(mat, envir=sys.parent())))
                       }else if(type == "3d") {
                          numzones <- dim(get(mat, envir=sys.parent()))[1]
                       }else{
                          numzones <- length(get(mat, envir=sys.parent()))
                       }
                       if(enssize != numzones) {
                          stop("ERROR:  ensemble and matrix sizes do not match")
                       }
                     
                       #Process 3d array of full matrices
                       if(type == "3d") {         
                          #Loop through all elements of 3rd dimension
                          for(i in 1:dim(get(mat, envir=sys.parent()))[3]) {         
                               collapse.row <- apply(get(mat, envir=sys.parent())[,,i], 2,
                                   function(x) tapply(x, districts[ens], sum))
                               dist.sum <- t(apply(t(collapse.row), 2, function(x)
                                   tapply(x, districts[ens], sum)))
                       
                               ma.rowsum <- append(apply(dist.sum, 1, sum), sum(dist.sum))
                               ma.colsum <- apply(dist.sum, 2, sum)
                               dist.sum <- cbind(rbind(dist.sum, ma.colsum), ma.rowsum)
                       
                               dist.sum <- data.frame(dist.sum)
                               names(dist.sum) <- row.names(dist.sum) <- dist.name
                               dist.sum <- round(dist.sum, digits)
                       
                               writeLines(paste("\n", dimnames(get(mat, envir=sys.parent()))[[3]][i],
                                   "              destination groups", sep=""), con=outfile)
                               writeLines("origin", con=outfile)
                               writeLines("groups", con=outfile)
                       
                               sink(outfile, append=T)
                               print(dist.sum)
                               sink()
                          }
                       }
                     
                       #Process 2d full matrix
                       if(type == "2d") {         
                          collapse.row <- apply(get(mat, envir=sys.parent()), 2,
                               function(x) tapply(x, districts[ens], sum))
                           dist.sum <- t(apply(t(collapse.row), 2,
                               function(x) tapply(x, districts[ens], sum)))
                       
                           ma.rowsum <- append(apply(dist.sum, 1, sum), sum(dist.sum))
                           ma.colsum <- apply(dist.sum, 2, sum)
                           dist.sum <- cbind(rbind(dist.sum, ma.colsum), ma.rowsum)
                       
                           dist.sum <- data.frame(dist.sum)
                           names(dist.sum) <- row.names(dist.sum) <- dist.name
                           dist.sum <- round(dist.sum, digits)
                       
                           writeLines("\n              destination groups", con=outfile)
                           writeLines("origin", con=outfile)
                           writeLines("groups", con=outfile)
                       
                           sink(outfile, append=T)
                           print(dist.sum)
                           sink()         
                       }         
                     
                       #Process 1d mo/md (array)
                       if (type == "1d") {         
                           foo <- data.frame(get(mat, envir=sys.parent()))
                           goo <- data.frame(districts[ens])
                           dist.sum <- apply(foo, 2, function(x) tapply (x, goo, sum))
                       
                           ms.totsum <- sum(dist.sum)
                           dist.sum <- rbind(dist.sum, ms.totsum)
                       
                           dist.sum <- data.frame(dist.sum)
                           row.names(dist.sum) <- dist.name
                           names(dist.sum) <- "ALL"
                       
                           dist.sum <- round(dist.sum, digits)
                           dist.sum <- round(dist.sum, digits)
                       
                           writeLines("\nzones", con=outfile)
                           sink(outfile, append=T)
                           print(dist.sum)
                           sink()        
                      } 
                              
                       #Close output file and reset output width
                       close(outfile)
                       options(oldOptions)
                   }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # ipf - function balance a multi dimensional array using the fratar method
    # inputs:
    # 1) marginsMtx - a matrix of margin values with each column equal to a margin
    # 2) seedAry - a multi-dimensional array used as the seed for the IPF
    # 3) iteration counter (default to 100)
    # 4) closure criteria (default to 0.001)
    # Revised 09/20/14 by Martin Mann to handle zero values without replacing them with a small value
    # Revised 04/12/16 by Alex Bettinardi to move location of marginCoeff initialization to inside "for" Loop and to correct marginCheck update so loop closes on iterations
    # Called by inputsSave.r, inputsSave_tazData.R, externalModel_AB20070111.R
    ##/
 
    fun$ipf <- function(marginsMtx, seedAry, maxiter=100, closure=0.001) {
                    
                    if(sum(marginsMtx)>0){
                       if(any(abs(1-colSums(marginsMtx)/mean(colSums(marginsMtx)))>0.01)) cat("margins control totals in the ipf are off by more that 1%\n")
                    }
                    
                    numMargins <- length(dim(seedAry))
                    if(ncol(marginsMtx) != numMargins) {
                        stop("number of margins in marginsMtx not equal to number of margins in seedAry")
                    }

                    resultAry <- seedAry
                    iter <- 0
                    marginChecks <- rep(1, numMargins)
                    margins <- seq(1, numMargins)
                    
                    while((any(marginChecks > closure)) & (iter < maxiter)) { 
                         
                        for(margin in margins) {
                            marginCoeff <- rep(0,(dim(seedAry)[margin]))
                            marginTotal <- apply(resultAry, margin, sum)
                            indx <- marginTotal!=0
                            marginCoeff[indx] <- marginsMtx[,margin][indx]/marginTotal[indx]
                            resultAry <- sweep(resultAry, margin, marginCoeff, "*")
                            marginChecks[margin] <- sum(abs(1 - marginCoeff)[indx])
                        }
                        iter <- iter + 1
                    }
                    attributes(resultAry)$iter <- iter
                    return(resultAry)                                   
                }
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # balance - function balance a two dimensional matrix using the fratar method    
    # Revised 10-9-2014  by Martin Mann to handle zero values without replacing them with a small value or by removing the zero values
    # Called by balanceDist.R, modeChoiceCommon.R, tripDistribution.R
    ##/
    
    fun$balance <- function(seed, rowcontrol, colcontrol, closure, maxiter){   
                      #set initial values       
                      result <- seed
                      rowcheck <- 1
                      colcheck <- 1
                      iter <- 0                       
                      
                      #Successively proportion rows and columns until closure or iteration criteria are met
                      while(((rowcheck > closure) | (colcheck > closure)) & (iter < maxiter)) {                	 
                          rowtotal <- rowSums(result)
                     	    rowfactor <- rowcontrol/rowtotal
                     	    rowfactor[is.nan(rowfactor)]<-0    	                          	 
                     	    result <- sweep(result, 1, rowfactor, "*")
                     	    result[is.nan(result)] <- 0
                
                          coltotal <- colSums(result)
                          colfactor <- colcontrol/coltotal
                          colfactor[is.nan(colfactor)]<-0    	                         
                          result <- sweep(result, 2, colfactor, "*")
                          result[is.nan(result)] <- 0
                                         
                          rowcheck <- sum(abs(1-rowfactor))
                          print(rowcheck)
                      	  colcheck <- sum(abs(1-colfactor))
                      	  print(colcheck)
             	            iter <- iter + 1
                      }
                   return(result)
                   }
                              
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to read utility defintion csv file
    # Ben Stabler, 3/29/04, benjamin.stabler@odot.state.or.us
    # @param inFileName - input csv file
    # @return  inFileName - returns object to workspace with same name as input file
    # The format of the CSV file is as follows:
    # Rows are by purpose and there are two columns:
    #   1. the first is "purpose" and is the trip purpose name
    #   2. the second is "utility" and is the utility definition
    # The variables identified in the utility definitions must be named the same as the inputs read in in inputsSave.R or as those created by the model such as logSum.
    # Called by inputs.r
    ##/
    
    fun$readUtils <- function(inFileName) {
                         utils <- read.csv(inFileName)
                         #Create a vector of named utility definitions
                         result <- as.character(utils[,2])
                         result <- gsub(" +|\t", " ", result)
                         names(result) <- utils[,1]
                         result
                      }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/    
    # Function to parse variable names from a utility definition as an R expression stored as a string
    # Variable names cannot have numbers in them - replace numbers with letter 1 to A, 2 to B, etc.
    # @param rString - R expression stored as a string
    # @return words - variable names in expression
    # Called by accessUtilities.R, modeChoiceCommon.R, tripDistribution.R
    ##/
    
    fun$getVarNames <- function(rString) {
                           #Replace punctuation characters such as +,(,^ with " "
                           rString <- gsub("[+]|[*]|[(]|[)]|[-]|[@^]|[!]", " ", rString)
                           #Remove all numbers including decimals
                           rString <- gsub("[0-9]+.[0-9]+|[0-9]+", "", rString)
                           #Split on white space
                           words <- strsplit(rString, "[[:space:]]")[[1]]
                           words <- words[words != ""]
                           #Check if object exists and if it does check to see if it is a function
                           objExist <- sapply(words, exists)
                           #Test if object is a function so it is not loaded
                           objFunction <- sapply(words[objExist], function(x) is.function(get(x)))
                           objFunction <- objFunction[objFunction == T]
                           #Return variable names
                           words[!(words %in% names(objFunction))]
                        }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to read market segment coefficient matrix
    # Ben Stabler, 3/30/04, benjamin.stabler@odot.state.or.us
    # @param inFileName - input csv file
    # @return  inFileName - returns object to workspace with same name as input file
    # The format of the CSV file is as follows:
    # The first row contains the column names which are:
    # choice,cval0,cval1,cval2,cval3,h1,h2,h3,h4,w1,w2,w3,w4
    # The remaining rows represent the coefficients for each mode or purpose.
    # Called by inputs.r
    ##/
    
    fun$readCoeffs <- function(inFileName) {
                           #read in the data and the first two rows separately
                           coeffs <- read.csv(inFileName)
                           #name the rows with the values of column one and remove column one
                           rownames(coeffs) <- coeffs[,1]
                           coeffs <- coeffs[,2:ncol(coeffs)]
                           coeffs
                      }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to verify variable names
    # Ben Stabler, 4/16/04, benjamin.stabler@odot.state.or.us
    # @param utilDefVectorName - string name of utility defintion object
    # @param varDictionary - variable dinctionary object
    # @return  null
    # Function call example: verifyVarName("hbwModeUtils",varDictionary)
    # The format of the CSV file is as follows:
    # The first row is column names - "variable" and "description"
    # The remaining rows are variable names and descriptions.
    # Called by inputs.r
    ##/
    
    fun$verifyVarName <- function(utilDefVectorName, varDictionary) {
                             varNames <- sapply(get(utilDefVectorName), getVarNames)
                             varNames <- unique(unlist(varNames))                       
                             varDefined <- varNames[varNames %in% varDictionary$variable]
                             varUndefined <- varNames[!(varNames %in% varDictionary$variable)]
                        
                             if(length(varUndefined) > 0) {
                                  cat(paste("\nThe following variables defined in ", utilDefVectorName, " were not found in the variable dictionary\n\n", sep=""))
                                  cat(paste(paste(varUndefined, "\n", collapse="", sep=""), "\n"))
                                  stop("Variables not found",call.=FALSE)   
                              }else{
                                  cat(paste("\nAll variables in ", utilDefVectorName, " found in the variable dictionary\n\n", sep=""))
                              }
                          }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to verify mode names
    # Ben Stabler, 4/16/04, benjamin.stabler@odot.state.or.us
    # @param utilDefVectorName - string name of utility defintion object
    # @param modes - available modes for model
    # @return  null
    # Function call example: verifyModeName("hbwModeUtils",modes)
    # Modes can be specified in the utility files by income - For example driveAlonelowInc.  
    # The function removes lowInc|midInc|highInc from the name before checking mode names thus supporting mode names by income
    # Called by inputs.r
    ##/
    
    fun$verifyModeName <- function(utilDefVectorName, modes) {   
                              modeNames <- names(get(utilDefVectorName))
                              #Drops the income class subtitle
                              modeNames <- gsub("lowInc|midInc|highInc","", modeNames)
                              modeNames <- unique(unlist(modeNames))                         
                              modeDefined <- modeNames[modeNames %in% modes$mode]
                              modeUndefined <- modeNames[!(modeNames %in% modes$mode)]
                          
                              if(length(modeUndefined) > 0) {
                                  cat(paste("\nThe following modes defined in ", utilDefVectorName," were not found in the modes vector\n\n", sep=""))
                                  cat(paste(paste(modeUndefined, "\n", collapse="", sep=""), "\n"))
                                  stop("Modes not found",call.=FALSE)
                          
                              }else{
                                  cat(paste("\nAll modes in ", utilDefVectorName," found in the modes vector\n\n", sep=""))
                              }
                          }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to read in EMME/2 matrix
    #T his reads EMME/2 zone-to-zone batch out file in the following format
    # from to value
    #  x   y:  zzzz
    # After reading the file, a matrix with the from zones as rows and to zones as columns
    # Adapted from OSUM Code, 11/10/03
    # Revised 07-20-2014  by Martin Mann to correct for missing zone pairs and to improve speed
    # @param emme2File - EMME/2 file name
    # @param zoneNames - zone Names (as characters)
    # @return outMat - data in matrix format
    # Called by batch_and_read_mf()
    ##/
    
    fun$readEmme2 <- function(emme2File,zoneNames) {
                          emme2IN <- scan(emme2File, skip=4, what="")
                          emme2IN <- gsub(":", " ", emme2IN)
                          emme2IN <- paste(emme2IN, collapse=" ")
                          emme2IN <- scan(textConnection(emme2IN), what=list(0,0,0))
                  
                          from <- rep(zoneNames,each=length(zoneNames))
                          to <- rep(zoneNames,length(zoneNames))
                          values <- rep(0,length(zoneNames)^2)
                          vec1 <- paste(from,to,sep="_")
                          vec2 <- paste(emme2IN[[1]],emme2IN[[2]],sep="_")
                          indx1 <- which(vec1%in%vec2)
                          values[indx1] <- emme2IN[[3]]
                          outMat <- matrix(values,ncol=length(zoneNames),byrow=T)
                          dimnames(outMat) <- list(zoneNames,zoneNames)
                          outMat
                      }
        
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/    
    # Function to batch out and read an mf matrix
    # @param matNum - EMME/2 mf matrix number
    # @param bankFolder, macrosFolder from settings.csv
    # @return mat - data in matrix format
    # Requires readEmme2()
    # Called by inputsSave.R, inputCheck.R
    ##/
    
    fun$batch_and_read_mf <- function(matNum, bankFolder, macrosFolder,zoneNames) {
                                  setwd(paste(basedir, "\\", bankFolder, sep=""))
                                	absMacro = paste(basedir, "/", macrosFolder, "/", "batchout_mf.mac", sep="")
                                	system2(emmeBatch,c(), input = paste("Emme -P -m", absMacro, matNum),stdout = FALSE)
                                	mat <- readEmme2(paste("mf", matNum, ".out", sep=""),zoneNames)
                                	file.remove(paste("mf", matNum, ".out", sep=""))
                                	setwd(basedir)
                                	return(mat)
                              }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # This function writes an EMME matrix in batchin format.
    # @param mat - the R matrix to write out
    # @param mat.num - EMME matrix number
    # @param mat.name - EMME matrix short name
    # @param zone.names - matrix names for the zones since zone numbers can have gaps
    # @param m_or_a - either "m" for modify existing matrix or "a" for add new matrix to databank
    # @param default - default value for matrix cells
    # @param desc - description of matrix
    # Example: write.mf.batchin(pkadbus, 1, "pkbus", tAvail$taz, "m", 0, "peak am bus")
    # Called by write_mf_and_batch_in()
    ##/
    
    fun$writeEmme2 <- function(mat, matNum, matName, zoneNames, m_or_a="m", default=0, desc=mat.name) {                              	
   	                      #Open a file for writing
                         	out.file <- file(paste("mf", matNum, ".in", sep=""), "w")                             	
                         	#Write header lines
                         	writeLines(paste("c R Module:  writeEmme2  Date:", date(), "User: JEMnR", sep=" "), con=out.file, sep="\n")
                         	writeLines(paste("c Project:", project), con=out.file, sep="\n")
                         	writeLines("t matrices", con=out.file, sep="\n")
                         	writeLines(paste(paste(m_or_a, " matrix=mf", matNum, sep=""), matName, default, desc), con=out.file, sep="\n")                              	
                         	#Write data
                         	from <- rep(zoneNames,each=length(zoneNames))
                         	to <- rep(zoneNames,length(zoneNames))
                         	writeLines(paste("     ", from, "     ", to, ":   ", round(as.vector(t(mat)),3), sep=""), con=out.file, sep="\n")                           	
                         	#Close the file
                         	close(out.file)
                      }
                            
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/    
    # Function to write out a matrix and batch in to EMME
    # @param mat - R matrix
    # @param matNum - EMME matrix number
    # @param matName - EMME short matrix name
    # @param zones - zone names (numbers)
    # @param bankFolder - folder location of databank
    # @param macrosFolder - folder location of macros
    # @param desc - matrix description string
    # Requires writeEmme2()
    # Called by writeToBank.r
    ##/
    
    fun$write_and_batch_mf <- function(mat, matNum, matName, zoneNames, bankFolder, macrosFolder, desc=matName) {    	
   	                              setwd(paste(basedir, "/", bankFolder, sep=""))
                                 	writeEmme2(mat, matNum, matName, zoneNames, "m", 0, desc)
                                 	absMacro = paste(basedir, "/", macrosFolder, "/", "batchin_mf.mac", sep="")
                                 	system2(emmeBatch,c(), input = paste("Emme -P -m", absMacro, matNum),stdout = FALSE)
                                 	file.remove(paste("mf", matNum, ".in", sep=""))
                                 	setwd(basedir)
                              }
      
    ##/

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to create warning messages on inputs    
    # Requires tcltk package    
    # Called by inputCheck.R, hbschDistByType.R
    ##/
    
    if(!any(search()=="package:tcltk")) {
       options(warn=-1)
       wantQ <- library("tcltk", logical.return=T)
       options(warn=0)
    }  else {
       wantQ <- T
    }
    if(wantQ) wantQ <- T
    
    fun$ePopUp <- function(eText, warnName="error") {   
                       #  Only run interactive buttons if wantQ=T
                       if(wantQ){                        
                                eQ <- tktoplevel()
                                e1 <- tklabel(eQ, text=paste(eText,collapse="\n"))
                                NoP <- tkbutton(eQ, text="Close Warning")
                                blank <- tklabel(eQ, text="") 
                                Stop <- tklabel(eQ, text="Press ESC to STOP")
                                blank2 <- tklabel(eQ, text="") 
                                tkpack(e1, blank, Stop, blank2, NoP)
                                #  Function that reacts to user pressing "NoP"   
                                NOP <- function() {
                                       tkdestroy(eQ)                                       
                                    }    
                                tkconfigure(NoP, command = NOP)                       
                        }# end of if wantQ statement
                        
                        if(file.exists(paste(warnName, "Report.txt", sep=""))) {
                           error <- readLines(paste(warnName, "Report.txt", sep=""))
                        } else {
                           error <- c()
                        }
                        error <-c(date(), "",eText, "","", error)
                        writeLines(error, paste(warnName, "Report.txt", sep=""))
                        #end of axisOpt question
                    } 
          
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Name: calcRmse
    # Description: This function calculates the root mean square error of a series of observed and estimated values.
    # @param: obs - a vector of observed values (e.g. traffic counts)
    # @param: est - a vector of estimated values corresponding to the observed values (e.g. model assignments)
    # @param: pct - a logical identifying whether percentages should be returned (TRUE) or decimal values (FALSE)
    # @return: a scalar value of the root mean square error for the vector inputs
    # Called by runModel.R
    ##/
    
    fun$calcRmse <- function(obs, est, pct=TRUE){
                        if(length(obs) != length(est)) stop("Observed and estimated data are unequal length")
                        if(any(is.na(obs)) | any(is.na(est))) stop("Remove NA values from data")
                        SqError <- (obs - est)^2
                        MeanSqError <- sum(SqError) / length(SqError)
                        ObsMean <- sum(obs) / length(obs)
                        Result <- sqrt(MeanSqError) / ObsMean
                        if(pct == TRUE) Result <- Result * 100
                        Result
                    }
        
    ##/

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/    
    # A function to run a server script if it exists, or if it doesn't exist - a local script
    # Called by runModel.R, ModelModules
    ##/ 
    
    fun$serverLocalSource <- function(Server, Local) {
                                 #Code to check Inputs.  Use server version if available, otherwise use traveling copy
                                 if(file.exists(Server)) {
                                    #See if the "traveling copy" (Local) needs to be updated
                                    Old <- paste(readLines(Local), collapse="\n")
                                    New <- paste(readLines(Server), collapse="\n")
                                    if(Old != New) cat(paste("\n\nThe Server script at -", Server, "\nis different than the Local script at -", Local, "\nThe Server Script was used in this run\n\nHowever, if you ever run this model application off the server you will not be\nusing the latest scrip.  It is strongley recomended that you take\na minute or two at this time and update your local copy with the server copy\n\nServer:", Server, "\nLocal:",Local, "\n\n"))
                                    rm(Old, New)
                                    #Source in the Server script if it exists
                                    source(Server)
                                 }else{
                                    #Source in the Local script if the server does not exist
                                    source(Local)
                                 } 
                              }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/ 
    # A function run a slave R process via RScript that uses the master projDir and R image
    # Example - runSlaveR(Rscript, "y = 7; print(y)")
    # Ben Stabler, stabler@pbworld.com
    # Called by ModelModules.R
    ##/
    
    fun$runSlaveR = function(Rscript, RCodeString, wait=F, saveImage=T) {    
                        imageName = gsub("\\\\","/",tempfile())
                      	if(saveImage) {
                      		save.image(imageName)
                      	}
                      	
                      	RCodeString = gsub("\n|\t","",RCodeString)
                      	RCode = paste("setwd('", getwd(), "')", sep="")
                      	RCode = paste(RCode, ";", "load(", "'", imageName, "')", sep="")
                      	RCode = paste(RCode, ";", RCodeString, sep="")
                      	RCode = paste('"', RCode, '"', sep="")
                      	
                      	cmdLine = paste('"', Rscript, '"', " --vanilla -e ", RCode, sep="")
                      	print(paste("runSlaveR:", cmdLine))
                      	system(cmdLine, wait=wait)	    	
                    }
    
    ##/
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/ 
    # Function to load multiple objects in an "RData" file 
    # @param filename
    # @return list of objects in rData set
    # Martin Mann, 11/04/14
    # Called by createTwoPopsFromUnivOutputs.R,R
    ##/
        
    fun$loadFile <- function(fileName) {
                        load(fileName)
                        out <- lapply(ls()[-(which(ls()%in%"fileName"))],function(x) get(x))
                        return(out)
                    }
                      
    ##/

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/ 
    # Function to load selected objects from a single list in an "RData" file
    # @param filename
    # @return selected list of objects in rData set
    # Martin Mann, 11/05/14
    # Called by accessUtilities.R, modeChoieCommon.R, tripDistribution.R, hbwGen.R, nhbGen.R
    ##/ 
     
    fun$loadListObj <- function(fileName,objNames) {
                            load(fileName)
                            rObj <- ls()[-(which(ls()%in%c("fileName","objNames")))]
                            out <- lapply(objNames,function(x) get(rObj)[[x]])       
                            names(out) <-  objNames
                            return(out)
                        }
              
    ##/
    
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/ 
    # Function to load selected objects from a single list in an "RData" file
    # @param filename
    # @return selected list of objects in rData set
    # Martin Mann, 11/05/14
    # Called by accessUtilities.R, modeChoieCommon.R, tripDistribution.R, hbwGen.R, nhbGen.R
    ##/ 
     
    fun$getEnvirPos <- function(objName) {
                            curLs <- sapply(1:length(search()),function(x){
                                          out <- NULL
                                          curEnv <- try(ls(x))
                                          if(class(curEnv)!="try-error") out <- curEnv
                                          out})
                            grep(objName,curLs)
                          }
              
    ##/
                             
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##/
    # Function to create a PUMA, TAZ and a PUMS file from Synpop output
    # @param housesDF -modified output household file from synpop
    # @param taz - taz file from census
    # @return taz,taz_census,puma, pumHHSyn
    # Martin Mann, 10/28/14
    # Called by inputs.R, createTwoPopsFromUnivOutputs.R
    ##/
    
    fun$SynPopLUToTAZ  <- function(housesDF,curTaz,curPop=NULL) {
                              
                              ####### REPLACE WITH APPROPRIATE UNIV. POP ######
                              if(as.logical(runUniversityModel)) {
                                  if(curPop%in%"SynPop_StdLst") {
                                        housesDF$HHWgt <- housesDF$HHWt_St
                                        housesDF$AdjNP <- housesDF$AdjNP_St 
                                  }
                                  if(curPop%in%"SynPop_StdFacLst") {
                                        housesDF$HHWgt <- housesDF$HHWt_StFc
                                        housesDF$AdjNP <- housesDF$AdjNP_StFc           
                                  }
                              }              
                                                                                                             
                              ####### HHBASE #######
                              HHBASEDF <- data.frame("HHBASE"=tapply(housesDF$HHWgt,housesDF$taz,sum ),stringsAsFactors=F)
                              HHBASEDF$TAZ <- as.numeric(rownames(HHBASEDF))
                      
                              ####### POPBASE ######
                              POPBASEDF <- data.frame("POPBASE"=tapply(housesDF$AdjNP,housesDF$taz,sum ),stringsAsFactors=F)
                              POPBASEDF$TAZ <- as.numeric(rownames(POPBASEDF))
                      
                              ####### HHSIZE DISTRIBUTION #######
                              hhsCut <- as.vector(cut(housesDF$np,c(0,1,2,3,(max(housesDF$np)+1)),include.lowest = F, right = T,labels=hs))
                              hhsDF <- as.data.frame(tapply(housesDF$HHWgt,list(housesDF$taz,hhsCut),function(x) sum(x)))
                              hhsDF[is.na(hhsDF)]  <- 0
                              hhsDF$TAZ <- as.numeric(rownames(hhsDF))
                      
                              ####### INCOME DISTRIBUTION #######
                              housesDF[housesDF$hhincAdj<0,"hhincAdj"] <- 0
                              housesDF$hhincAdj <- round(housesDF$hhincAdj*deflat,0)
                              incCut <- as.vector(cut(housesDF$hhincAdj,c(0,15000,30000,60000,500000),include.lowest = T, right = F,labels=hi))
                              incDF <- as.data.frame(tapply(housesDF$HHWgt,list(housesDF$taz,incCut),function(x) sum(x)))
                              incDF[is.na(incDF)] <- 0
                              incDF$TAZ <- as.numeric(rownames(incDF))
                      
                              ####### AGE DISTRIBUTION #######
                              ageCut <- as.vector(cut(housesDF$ageRelp,c(0,24,54,64,150),include.lowest = F, right = T,labels=ha))
                              ageDF <- as.data.frame(tapply(housesDF$HHWgt,list(housesDF$taz,ageCut),function(x) sum(x)))
                              ageDF[is.na(ageDF)] <- 0
                              ageDF$TAZ <- as.numeric(rownames(ageDF))
                      
                              ###### COMPILE TAZ FILE #######
                              DFLst <- list(HHBASEDF,POPBASEDF,hhsDF,incDF,ageDF)
                              for(x in 1:(length(DFLst)-1)) DFLst[[x+1]] <- merge(DFLst[[x]],DFLst[[x+1]],by="TAZ",all=T)
                              TAZsyn <- DFLst[[5]]
                              rownames(TAZsyn) <- TAZsyn$TAZ
                      
                              ####### AHHSBASE DISTRIBUTION #######
                              TAZsyn$AHHSBASE <- round(TAZsyn$POPBASE/TAZsyn$HHBASE,4)
                              TAZsyn[is.na(TAZsyn$AHHSBASE),"AHHSBASE"] <- 0
                      
                              ####### singleFamily DISTRIBUTION #######
                              percSF <- t(as.data.frame(tapply(housesDF$HHWgt,list(as.numeric(housesDF$bld%in%2)+1,housesDF$taz),function(x) sum(x))))
                              percSF[is.na(percSF)]  <- 0
                              TAZsyn$percentSingleFamilyDetached <- round(percSF[,2]/(percSF[,1]+percSF[,2]),4)
                              TAZsyn[is.na(TAZsyn$percentSingleFamilyDetached),"percentSingleFamilyDetached"] <- 0
                      
                              ######## CREATE HH DIST PERCENTS ######
                              for(x in c("hs","hi","ha")) TAZsyn[,get(x)] <- round(sweep(TAZsyn[,get(x)],1,TAZsyn$HHBASE,"/"),4)
                              if(any(is.infinite(unlist(TAZsyn)))) for(curCol in colnames(TAZsyn)) TAZsyn[is.infinite(TAZsyn[,curCol]),curCol] <- 0
                              if(any(is.nan(unlist(TAZsyn)))) for(curCol in colnames(TAZsyn)) TAZsyn[is.nan(TAZsyn[,curCol]),curCol] <- 0
                      
                              ####### UPDATE TAZ FILE WITH SYNPOP #######
                              oldTAZ <- curTaz[,tazCols]
                              curTaz$POPBASE <- curTaz$POPBASE - curTaz$GQPOPBASE 
                              curTaz[rownames(TAZsyn) ,colnames(TAZsyn)] <- TAZsyn
 
                              curTaz$POPBASE <- curTaz$POPBASE + curTaz$GQPOPBASE
                      
                              ####### CREATE PUMS FILE #######
                              hiaAry <- tapply(housesDF$HHWgt,list(hhsCut,incCut,ageCut),sum)
                              hiaAry[is.na(hiaAry)] <- 0
                              puma <- as.data.frame(matrix(hiaAry,ncol=1,dimnames=list(c(),"puma")) )
                              pumHHSyn <- housesDF  #New PUMS FILE
                      
                              return(list("modTaz"=curTaz,"taz_census"=oldTAZ,"puma"=puma,"pumsHHSyn"=pumHHSyn))

                          }
    
########################################################### END ##############################################################################

