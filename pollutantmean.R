pollutantmean <- function(directory, pollutant, id = 1:332) {
    ##reads in numbered(id) csv files from directory and calculates mean of
    ##pollutant(sulfate or nitrate) excluding NAs
    ##returns numeric mean
    v <- vector(mode="numeric")
    for(i in seq_along(id)){
        file <- read.csv(paste(directory, "/", sprintf("%03d", id[i]), ".csv", sep=""))
        c<- file[, pollutant]
        v<-append(v, c[!is.na(c)])
    }
    mean(v)
}