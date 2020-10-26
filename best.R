# week 4 assignment
outcome_tbl <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
ncol(outcome)
nrow(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# the 2-character abbreviated name of a state and an
#The function reads the outcome-of-care-measures.csv file and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
library(dplyr)
# outcomes:: “heart attack”, “heart failure”, or “pneumonia”
outcome name
best <- function(state, outcome) {
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
                     minimum <- order(hosp$heart_attack, decreasing = FALSE, na.last = TRUE)[1]
                hosp <- hosp$Hospital[minimum] 
                print(hosp)   
                 }
                if (outcome == "heart failure"){
                        hosp<- outcome_tbl %>%
                                filter(State == state)%>%
                                select(2, 17)
                        names(hosp) <- c("Hospital", "heart_failure")
                        hosp$heart_failure <- as.numeric(hosp$heart_failure)
                        minimum <- order(hosp$heart_failure, decreasing = FALSE, na.last = TRUE)[1]
                        hosp <- hosp$Hospital[minimum]
                        print(hosp)
                }
                if (outcome == "pneumonia"){
                        hosp<- outcome_tbl %>%
                                filter(State == state)%>%
                                select(2, 23)
                        names(hosp) <- c("Hospital", "pneumonia")
                        hosp$pneumonia <- as.numeric(hosp$pneumonia)
                        minimum <- order(hosp$pneumonia, decreasing = FALSE, na.last = TRUE)[1]
                        hosp <- hosp$Hospital[minimum]
                        print(hosp)
                }
        }
                
        else{
                message('invalid state')
                
        }
        ## rate
        
}
