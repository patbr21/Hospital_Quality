rankhospital <- function(state, outcome, num) {
        ## Read outcome data
        outcome_tbl <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (any(outcome_tbl$State == state, na.rm = TRUE) ==TRUE){
                ## Return hospital name in that state with lowest 30-day death
                if (outcome == "heart attack"){
                        hosp<- outcome_tbl %>%
                                filter(State == state)%>%
                                select(2, 11)
                        names(hosp) <- c("Hospital", "heart_attack")
                        hosp$heart_attack <- as.numeric(hosp$heart_attack)
                        if (num == "worst"){
                                minimum <- order(hosp$heart_attack, decreasing = TRUE, na.last = TRUE)[1]
                        }
                        else{
                                minimum <- order(hosp$heart_attack,hosp$Hospital, decreasing = FALSE, na.last = TRUE)[num]
                        }
                        hosp <- hosp$Hospital[minimum] 
                        print(hosp)   
                }
                if (outcome == "heart failure"){
                        hosp<- outcome_tbl %>%
                                filter(State == state)%>%
                                select(2, 17)
                        names(hosp) <- c("Hospital", "heart_failure")
                        hosp$heart_failure <- as.numeric(hosp$heart_failure)
                        if (num == "worst"){
                                minimum <- order(hosp$heart_failure, decreasing = TRUE, na.last = TRUE)[1]
                        }
                        else{
                                minimum <- order(hosp$heart_failure, hosp$Hospital , decreasing = FALSE, na.last = TRUE)[num]
                        }
                        hosp <- hosp$Hospital[minimum]
                        print(hosp)
                }
                if (outcome == "pneumonia"){
                        hosp<- outcome_tbl %>%
                                filter(State == state)%>%
                                select(2, 23)
                        names(hosp) <- c("Hospital", "pneumonia")
                        hosp$pneumonia <- as.numeric(hosp$pneumonia)
                        if (num == "worst"){
                                minimum <- order(hosp$pneumonia, decreasing = TRUE, na.last = TRUE)[1]
                        }
                        else{
                                minimum <- order(hosp$pneumonia,hosp$Hospital, decreasing = FALSE, na.last = TRUE)[num]
                                } 

                                
                        hosp <- hosp$Hospital[minimum]
                        print(hosp)
                }
        }
        
        else{
                message('invalid state')
                
        }
        ## rate
        
}
