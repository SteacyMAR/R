rankall <- function(outcome, num = "best") 
{
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        result <- data.frame()
        
        ## Check that state and outcome are valid
        if(outcome == "heart attack" 
           || outcome == "heart failure" 
           || outcome == "pneumonia")
        {
                result <- rbind(result,rankeach(data[1:98,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[99:115,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[116:192,c(2,7,11,17,23)],outcome,num))
                result <- rbind(result,rankeach(data[193:269,c(2,7,11,17,23)],outcome,num))
                result <- rbind(result,rankeach(data[270:610,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[611:682,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[683:714,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[715:720,c(2,7,11,17,23)],outcome,num))
                result <- rbind(result,rankeach(data[721:728,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[729:908,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[909:1040,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1041:1059,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1060:1089,c(2,7,11,17,23)],outcome,num))
                result <- rbind(result,rankeach(data[1090:1268,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1269:1392,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1393:1501,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1502:1619,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1620:1715,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1716:1829,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1830:1866,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1867:1911,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1912:1979,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[1980:2113,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2114:2246,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2247:2329,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2330:2437,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2438:2491,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2492:2581,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2582:2609,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2610:2635,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2636:2700,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2701:2740,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2741:2925,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[2926:3037,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3038:3073,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3074:3243,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3244:3369,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3370:3428,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3429:3603,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3604:3654,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3655:3666,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3667:3729,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3730:3777,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[3778:3893,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[c(3894:4219,4663:4706),c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4220:4261,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4262:4276,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4277:4278,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4279:4365,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4366:4453,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4454:4507,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4508:4632,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4633:4661,c(2,7,11,17,23)],outcome,num)) 
                result <- rbind(result,rankeach(data[4662,c(2,7,11,17,23)],outcome,num)) 
                ##result <- rbind(result,rankeach(data[4663:4706,c(2,7,11,17,23)],outcome,num))
                
                
                result[order(result[,2]),]
                
        }
        
        else
        {
                stop("invalid outcome")
        }
        ## For each state, find the hospital of the given rank
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}

rankeach <- function(tempdata,outcome,num)
{
        valid <- data.frame()
        
        for(i in 1:nrow(tempdata))
        {
                if(outcome == "heart attack" && tempdata[i,3] != "Not Available")
                {
                        valid <- rbind(valid,tempdata[i,c(1,2,3)])
                }
                if(outcome == "heart failure" && tempdata[i,4] != "Not Available")
                {
                        valid <- rbind(valid,tempdata[i,c(1,2,4)])
                }
                if(outcome == "pneumonia" && tempdata[i,5] != "Not Available")
                {
                        valid <- rbind(valid,tempdata[i,c(1,2,5)])
                }
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        class(valid[,3]) <- "numeric"
        
        valid <- valid[order(valid[,3],valid[,1],na.last = TRUE),]
        if(num == "best") num <- 1
        ##print(nrow(valid))
        if(num == "worst") num <- nrow(valid)
        if(num <= nrow(valid)) 
        {
                c <- valid[num,c(1,2)]
        }
        else
        {
                c <- cbind("<NA>",tempdata[1,2])
        }
        
        colnames(c) <- c("hospital","state")
        rownames(c) <- valid[1,2]
        c
}
