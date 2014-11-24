rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        tempdata <- data.frame()
        
        stateexist <- 0
        outcomeexist <- 0
        
        ## Check that state and outcome are valid
        for(i in 1:nrow(data))
        {
                if(data[i,7] == state)
                {
                        stateexist <- stateexist + 1
                        if(outcome == "heart attack" && data[i,11] != "Not Available")
                        {
                                outcomeexist <- 1
                                tempdata <- rbind (tempdata,data[i,c(2,11,7)])
                        }
                        if(outcome == "heart failure" && data[i,17] != "Not Available")
                        {
                                outcomeexist <- 1
                                tempdata <- rbind (tempdata,data[i,c(2,17,7)])
                        }
                        if(outcome == "pneumonia" && data[i,23] != "Not Available")
                        {
                                outcomeexist <- 1
                                tempdata <- rbind (tempdata,data[i,c(2,23,7)])
                        }
                }
                
        }
        
        if(stateexist == 0)
        {
              stop("invalid state")  
        }
        if(outcomeexist == 0)
        {
                stop("invalid outcome")  
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        class(tempdata[,2]) <- "numeric"
        
        tempdata <- tempdata[order(tempdata[,2],tempdata[,1],na.last = TRUE),]
        if(num == "best") num <- 1
        if(num == "worst") num <- nrow(tempdata)
        if(num <= nrow(tempdata)) 
        {
                tempdata[num,1]
        }
        else
        {
                NA
        }
}
