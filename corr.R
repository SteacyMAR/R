corr <- function(directory, threshold = 0) {
        temp_corr <- c()
        class(temp_corr) <- "numeric"
        for(id in 1:332)
        {
                if(complete(directory,id)$nobs > threshold)
                {
                        if(id < 10)
                        {
                                filena <- paste(directory,"/00",as.character(id),".csv",sep = "")
                        }
                        if(id >=10 & id<100)
                        {
                                filena <- paste(directory,"/0",as.character(id),".csv",sep = "")
                        }
                        if(id >= 100)
                        {
                                filena <- paste(directory,"/",as.character(id),".csv",sep = "")
                        }
                        a <- read.csv(filena,head = TRUE)
                        sulfate <- a[complete.cases(a),2]
                        nitrate <- a[complete.cases(a),3]
                        temp_corr <- c(temp_corr,cor(sulfate,nitrate))
                }
        }
        temp_corr
}