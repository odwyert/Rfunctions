corr <- function(directory, threshold = 0) {
    ## reads directory of numbered csv files
    ## returns numeric vector of correlations between sulfate and nitrate that have complete cases
    ## above threshold number
    id<-1:332
    v <- vector(mode="numeric")
    for(i in seq_along(id)){
        file <- read.csv(paste(directory, "/", sprintf("%03d", id[i]), ".csv", sep=""))
        c<- file[complete.cases(file), ]
        if(nrow(c) > threshold)
            v<-append(v, cor(c["sulfate"], c["nitrate"]))
    }
    v
}