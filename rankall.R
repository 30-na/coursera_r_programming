# Ranking hospitals in all states
rankall <- function(outcome, num = 'best') {
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
    
    
    
    # list of the states
    state_list <- unique(cm_data$State)
    
    # select columns
    df <- cm_data %>% select(Hospital.Name, State, 11, 17, 23)
    colnames(df) <- c('hospital_name', 'state', 'heart attack', 'heart failure', 'pneumonia')
    
    # factor by state
    suppressWarnings(df[[outcome]] <- as.numeric(df[[outcome]]))
    list_by_state <- split(df, df['state'])
    
    # order by outcome
    for (state in state_list){
        list_by_state[[state]] <- list_by_state[[state]][order(list_by_state[[state]][outcome],
                                                               list_by_state[[state]]['hospital_name'],
                                                               na.last=NA) , ]
    }
    print(num)
    ## define num
    if (num == 'best'){
        num = 1
    } 
    
    # initial the return dataframe
    return_df <- data.frame()
    if (num == 'worst'){
    for (state in state_list){
            num = nrow(list_by_state[[state]])
            new_row <- c(list_by_state[[state]][num,'hospital_name'], state)
            return_df <- rbind(return_df, new_row)
        }
    }
    else{
        for (state in state_list){
            new_row <- c(list_by_state[[state]][num,'hospital_name'], state)
            return_df <- rbind(return_df, new_row)
        }
        }
        
    colnames(return_df) <- c( 'hospital' , 'state')
    return_df <- return_df[order(return_df$state) , ]
    
    
    
    return(return_df)
    }





