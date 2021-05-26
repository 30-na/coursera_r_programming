# Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = 'best') {
    library(dplyr)
    ## Read outcome data
    cm_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    valid_state <- function(x) {
        state_list <- unique(cm_data$State)
        x %in% state_list
    }
    
    valid_outcome <- function(x){
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        x %in% outcome_list
    }
    
    if (valid_state(state) == FALSE){
        stop("invalid state")}
    if (valid_outcome(outcome) == FALSE){
        stop('invalid outcome')}
    
    ## Return hospital name in that state with lowest 30-day death
    # filter by state
    data_by_state <- cm_data %>% filter(State == state)
    
    # check the outcome
    if (outcome == "heart attack"){
        outcome = 11
    }
    else if (outcome == 'heart failure'){
        outcome = 17
    }
    else if (outcome == 'pneumonia'){
        outcome = 23
    }
    
    #slice dataframe by state
    data_by_state <- cm_data %>% filter(State == state)
    
    #slice dataframe by hospital name and rate
    data_by_state[ ,outcome] <- suppressWarnings(as.numeric(data_by_state[ ,outcome]))
    new_df <- data.frame(Hospital.Name=data_by_state$Hospital.Name, Rate=data_by_state[,outcome])
   
    #sort by rate (if tie sort by hospital name)
    new_df <- new_df[order(new_df$Rate, new_df$Hospital.Name, na.last=NA), ]
    
    #add Rank column
    new_df$Rank <- seq.int(nrow(new_df))
    
    ## rate
    if (num == 'worst'){
        num = nrow(new_df)
    }
    if (num == 'best'){
        num = 1
    } 
    return(new_df[num, 'Hospital.Name'])
}


