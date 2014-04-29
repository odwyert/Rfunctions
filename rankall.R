## reads in hospital data from file and returns the hospital for the state,
## outcome, and num(rank) arguments. Ties are resolved by sorting hospital names in alphabetical 
## order and picking the first
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    result <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    outcomes<-list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23 )
    if (is.null(outcomes[[outcome]]))
        stop("invalid outcome")
    else outcomeColumn<-as.numeric(outcomes[outcome])
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    stateranks<-data.frame(hospital=character(), state=character())
    statelist<-append(state.abb, c("DC", "VI"))
    sorted<-statelist[order(statelist)]
    for (state in sorted)
    {
        resultst<-subset(result, State == state & result[,outcomeColumn] != 'Not Available' & !is.na(result[,outcomeColumn]), select=c(Hospital.Name, outcomeColumn))
        resultst[, 2]<-as.numeric(resultst[, 2])
        resultorder<-resultst[ order(resultst[,2], resultst[, 1]), ]
        if (num == "best"){
            stateranks<-rbind(stateranks, data.frame(hospital=resultorder[1,1], state=state))
        }
        else if (num == "worst"){
            stateranks<-rbind(stateranks, data.frame(hospital=resultorder[nrow(resultorder), 1], state=state))
        }
        else stateranks<-rbind(stateranks, data.frame(hospital=resultorder[as.numeric(num), 1], state=state))
    }
    stateranks
}
