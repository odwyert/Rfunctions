## reads in hospital data from file and returns the hospital for the state,
## outcome, and num(rank) arguments. Ties are resolved by sorting hospital names in alphabetical 
## order and picking the first
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    result <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcomes<-list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23 )
    if (is.null(outcomes[[outcome]]))
        stop("invalid outcome")
    else outcomeColumn<-as.numeric(outcomes[outcome])
    if (is.na(match(state, state.abb)))
        stop("invalid state")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    resultst<-subset(result, State == state & result[,outcomeColumn] != 'Not Available' & !is.na(result[,outcomeColumn]), select=c(Hospital.Name, outcomeColumn))
    resultst[, 2]<-as.numeric(resultst[, 2])
    resultorder<-resultst[ order(resultst[,2], resultst[, 1]), ]
    if (num == "best"){
        resultorder[1,1]
    }
    else if (num == "worst"){
        resultorder[nrow(resultorder), 1]
    }
    else resultorder[as.numeric(num), 1]
}