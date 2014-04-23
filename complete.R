complete <- function(directory, id = 1:332) {
    ##reads in numbered(id) csv files from directory and calculates no of complete cases in file
    ## returns data frame with id = file number and nobs = no of complete cases in file for columns 
    myData <- data.frame(id=integer(0), nobs=integer(0))
    for(i in seq_along(id)){
        file <- read.csv(paste(directory, "/", sprintf("%03d", id[i]), ".csv", sep=""))
        c<- complete.cases(file)
        myData[nrow(myData)+1,]<-c(id[i], sum(c))
    }
    myData
}