setwd("/home/rogelio/Desktop/datasciencecoursera/ProgrammingAssignment3")
outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#outcome argument for tests "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"

best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        
        if (!outcome %in% names(outcome1)) {
                stop("invalid outcome")
        } else if(!state %in% outcome1$State) {
                stop("invalid state")
        }

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        state_hosp <- subset(outcome1, State == state)
        
        A <- as.numeric(unlist(state_hosp[outcome]))
        
        hosp <- factor()

        hosp <- state_hosp$Hospital.Name[A == min(A, na.rm = TRUE)]
        hosp <- hosp[!is.na(hosp)]
        sort(hosp)[1]
        
}