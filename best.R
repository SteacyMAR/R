
best <- function(state,outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## flags to check if the input is correct
        countstate <- 0
        countoutcome <- 0
        
        tempframe <- data.frame() ## a dataframe to store some information we care about
        minv <- c() ## a vector(because maybe there are one more hospital's rate are the same) to store the best hospital's name and return it 
        
        for(i in 1:nrow(data))
        {
                if(data[i,7] == state)
                {
                        countstate <- 1
                        if(outcome == "heart attack" && data[i,11] != "Not Available") ## filter NA
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
                ##get the min rate value
                class(tempframe[,3]) <- "numeric" 
                min <- min(tempframe[,3],na.rm = TRUE) 
                
                for(i in 1:nrow(tempframe))
                {
                        if(tempframe[i,3] == min)
                        {
                                minv <- c(minv,tempframe[i,1])
                        }
                }
                
                minv[order(minv, na.last = NA)]    ##order
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
