setwd("/home/rogelio/Desktop/datasciencecoursera/ProgrammingAssignment3")
outcome1 <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
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

worst <- function(state, outcome) {
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
        
        hosp <- state_hosp$Hospital.Name[A == max(A, na.rm = TRUE)]
        hosp <- hosp[!is.na(hosp)]
        sort(hosp)[1]
        
}

rankhospital <- function(state, outcome, num = "best") {

        if(num == "best" | num == 1){
                return(best(state, outcome))
        }
        else if(num == "worst"){
                return(worst(state, outcome))
        }
        
        #in case num is neither "best" nor "worst"
        
        if (!outcome %in% names(outcome1)) {
                stop("invalid outcome")
        } else if (!state %in% outcome1$State) {
                stop("invalid state")
        }
        
        
        #takes the state's hospitals
        state_hosp <- subset(outcome1, State == state)

        
        #sorts state's hospitals by outcome
        state_hosp_ord <- state_hosp[order(state_hosp[outcome]), ]
        
        
        #extracts the list of sorted outcomes from state_hosp_ord
        A <- as.numeric(unlist(state_hosp_ord[outcome]))
        
        
        #eliminates unnecessary columns
        ranking <- data.frame(state_hosp_ord$Hospital.Name, A)
        
        
        #eliminates NAs
        no_NA <- ranking[complete.cases(ranking),]
        
        
        #user requested rank for which there is no data
        if(num > nrow(no_NA)){
                return("requesting rank for which there is no data")
        }
        
        
        #check if there is a tie
        if(no_NA$A[num] == no_NA$A[num + 1] | no_NA$A[num] == no_NA$A[num - 1]){
                tie <- subset(no_NA, A == ranking$A[num])
                #return(tie)
                return(tie[order(tie$state_hosp_ord.Hospital.Name),][1,])
        }
        
        
        no_NA[num,]
}