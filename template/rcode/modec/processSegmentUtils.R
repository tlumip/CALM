#/
#@filename processSegmentUtils.R
#@author Ben Stabler
#@version 1.2
#@date 4/8/04
#
#Process market segment coefficients (utilities) and save out results.
#Market segment utilities are those utilities
#that only apply to specific market segments.
#
#Created 3/2/04 benjamin.stabler@odot.state.or.us
#Revised 3/8/04 Ben Stabler, to generate one file by purpose
# instead of one file for each mode by purpose
#Revised 4/8/04 to work with <purpose>MarketSegments.csv input
# which is only the market segment utility coefficients.
#Revised 11/10/14 minor syntx formatting change - no impact to code.
#/

    cat("Process market segment utilities and save\n\n")
    
    fun$processSegmentUtils <- function(purposesVector) {     
    
        ##/
        #PROCESS MARKET SEGMENT UTILITIES
        #This function creates a table of market segment coefficients (utilities) that are later used to in mode choice to calculate mode choice by market segment.  It allows for flexible market segment groups as opposed to a group of market segments.
        #Unique market segments are those combinations of market segments that have the same market segment specific utilities. By collapsing on these the calculations are cut down by a factor of about five.
        #@return marketUtil - market segment specific utility table by purpose
        ##/
        
        #Create directory to store resultss
        for(purpose in purposesVector) {
            for(income in c("lowInc","midInc","highInc")) {
                if(!file.exists(paste("modec/", purpose, sep=""))) dir.create(paste("modec/", purpose, sep=""))
                if(!file.exists(paste("modec", purpose, income, sep="/"))) dir.create(paste("modec/", purpose, "/", income, sep=""))
            }
        }
        
        ##/
        #NEEDED FUNCTIONS
        ##/
        
            #Function to add segment utilities to classesDf
            addNums <- function(classDf, segmentValues, cvalIndex, cMode) {
                          #Setup dummy 0 to hold results
                          classDf[cMode] <- 0
                          
                          #Get none zero market segment utility values
                          segmentValues <- segmentValues[segmentValues != 0]
                          
                          for(variable in names(segmentValues)) {                          
                              #Get segment specific value
                              value <- unlist(segmentValues[variable])                          
                              #cval
                              if(length(grep("cval", variable) == 1)) {
                                  num <- as.numeric(strsplit(variable, "cval")[[1]][2])
                                  classDf[cvalIndex == num, cMode] <- value
                              }
                              #hhSize
                              if(variable %in% c("h1","h2","h3","h4")) {
                                  num <- as.numeric(strsplit(variable, "h")[[1]][2])
                                  classDf[classDf$h == num, cMode] <- classDf[classDf$h == num, cMode] + value
                              }                          
                              #worker
                              if(variable %in% c("w1","w2","w3","w4")) {
                                  num <- as.numeric(strsplit(variable, "w")[[1]][2])
                                  classDf[classDf$w == num, cMode] <- classDf[classDf$w == num, cMode] + value
                              }
                          }                             
                          #Return classDf    
                          classDf
                        }                                
        
        ##/
        #DEFINE MARKET SEGMENTS FOR UTILITY ADDITIONS
        ##/
        
            #Define h, w, and c class variables for utilities
            hClasses <- as.numeric(gsub("h","",segmentsH))
            wClasses <- as.numeric(gsub("w","",segmentsW))
            cClasses <- as.numeric(gsub("c","",segmentsC))
            
            #Create all combinations of h1-4, w1-4, and c1-4
            classesDf <- expand.grid(hClasses,wClasses,cClasses)
            classesDf <- classesDf[order(classesDf[,1],classesDf[,2],classesDf[,3]),]
            colnames(classesDf) <- c("h","w","c")
        
        ##/
        #CREATE AN INDEX FOR MARKET SEGMENTS FOR UTILITY ADDITIONS
        ##/
        
            #Create cval index to classesDf
            cvalIndex <- rep(0, nrow(classesDf))
            cvalIndex[classesDf$c < classesDf$w] <- 1 #cval1
            cvalIndex[classesDf$c == classesDf$w] <- 2 #cval2
            cvalIndex[classesDf$c > classesDf$w] <- 3 #cval3
            cvalIndex[classesDf$c == 1] <- 0 #cval0
  
        ##/
        #BUILD MARKET SEGMENT SPECIFIC UTILITIES FOR MODE CHOICE
        #Get mode and purpose specific market segment specific utilities             
        ##/

            columns <- c("cval0","cval1","cval2","cval3","h1","h2","h3","h4","w1","w2","w3","w4")
            for(purpose in purposesVector) {
                
                #current Purpose Market Segment
                curMrktSeg <- paste(purpose, "MarketSegments", sep="")
            
                #Setup result data frame
                marketUtil <- classesDf
                cModes <- rownames(get(curMrktSeg))
                
                #GET MARKET SEGMENT UTILITIES
                for(cMode in cModes) {
                    #Get additive utilities from coeff matrix
                    segmentValues <- unlist(get(curMrktSeg)[cMode, columns])
                    
                    #Loop through segmentValues adding them to table
                    marketUtil <- addNums(marketUtil, segmentValues, cvalIndex, cMode)
                    
                    #NA utility for cval1 when mode equal to driveAlone or drivePass since cval1 is no car
                    if(cMode %in% c("driveAlone", "drivePass")) marketUtil[marketUtil$c == 1, cMode] <- NA
                }
                
                #INDEX UNIQUE HWC COMBINATIONS OF MARKET SEGMENT UTILITIES
                combinations <- apply(marketUtil[,4:ncol(marketUtil)],1, function(x) paste(x, collapse=" "))
                uniqueMarketUtil <- marketUtil[match(unique(combinations), combinations),]
                marketUtil$index <- match(combinations, unique(combinations))
                marketUtil <- list(marketUtil=marketUtil, uniqueMarketUtil=uniqueMarketUtil)
        
                #Save out resulting market segment utility table for all modes for one purpose
                save(list=paste("marketUtil", sep=""), file=paste("modec/", purpose, "/marketUtil.RData", sep=""))
                rm(marketUtil)       
            }
    }

########################################## END #################################################################