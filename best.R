best <- function(state,outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        countstate <- 0
        countoutcome <- 0
       
        tempframe <- data.frame()
        minv <- c()
        
        for(i in 1:nrow(data))
        {
                if(data[i,7] == state)
                {
                        countstate <- 1
                        if(outcome == "heart attack" && data[i,11] != "Not Available")
                        {
                                countoutcome <- 1
                                tempframe <- rbind(tempframe,data[i,c(2,7,11)])
                        }
                        if(outcome == "heart failure" && data[i,17] != "Not Available")
                        {
                                countoutcome <- 1
                                tempframe <- rbind(tempframe,data[i,c(2,7,17)])
                        }
                        if(outcome == "pneumonia" && data[i,23] != "Not Available")
                        {
                                countoutcome <- 1
                                tempframe <- rbind(tempframe,data[i,c(2,7,23)])
                        }
                }
                
        }
        
        if(!identical(tempframe,data.frame()))
        {
                class(tempframe[,3]) <- "numeric"
                
                
                min <- min(tempframe[,3],na.rm = TRUE)
                
                for(i in 1:nrow(tempframe))
                {
                        if(tempframe[i,3] == min)
                        {
                                minv <- c(minv,tempframe[i,1])
                        }
                }
                
                minv[order(minv, na.last = NA)]    
        }
        else
        {
                if(countstate == 0)
                {
                        
                       stop("invalid state")
                        
                }
                if(countoutcome == 0)
                {
                        stop("invalid outcome")
                }
        }
        
        
        
}
