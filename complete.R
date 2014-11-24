complete <- function(directory, id = 1:332) {
        temp_result <- matrix(0,0,2)
        
        for(i in id){
                if(i < 10)
                {
                        filena <- paste(directory,"/00",as.character(i),".csv",sep = "")
                }
                if(i >=10 & i<100)
                {
                        filena <- paste(directory,"/0",as.character(i),".csv",sep = "")
                }
                if(i >= 100)
                {
                        filena <- paste(directory,"/",as.character(i),".csv",sep = "")
                }
                a <- read.csv(filena,head = TRUE)
                count <- sum(complete.cases(a))
                temp_result <-rbind(temp_result,c(i,count))
        }
        
        my_result <-data.frame(temp_result)
        colnames(my_result) <- c("id","nobs")
        my_result 
}