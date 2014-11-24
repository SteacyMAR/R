pollutantmean <- function (directory, pollutant, id = 1:332) {
        data <- data.frame()
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
                data <-rbind(data,a)
               
        }
        if(pollutant == "sulfate")
        {
                data <- data[,2]
        }
        if(pollutant == "nitrate")
        {
                data <- data[,3]
        }
        mean(data[!is.na(data)])
}