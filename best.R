## reads in hospital data from file and returns best hospital for the state and 
## outcome arguments. Ties are resolved by sorting hospital names in alphabetical 
## order and picking the first
best <- function(state, outcome) {
    ## Read outcome data
    result <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcomes<-list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23 )
    if (is.null(outcomes[[outcome]]))
        stop("invalid outcome")
    else outcomeColumn<-as.numeric(outcomes[outcome])
    if (is.na(match(state, state.abb)))
        stop("invalid state")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    resultst<-subset(result, State == state & result[,outcomeColumn] != 'Not Available' & !is.na(result[,outcomeColumn]), select=c(Hospital.Name, outcomeColumn))
    resultst[, 2]<-as.numeric(resultst[, 2])
    resultorder<-resultst[ order(resultst[,2], resultst[, 1]), ]
    resultorder[1,1]
}
