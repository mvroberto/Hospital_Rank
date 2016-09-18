
	
	
	
best <- function(state, outcome){
	## state ---- Abv name of state ("TX","MD")
	## outcome --- "heart attack","pneumonia", "heart failure"
	
	########## Read File
	data_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#### check outcomes
	
	states_names_total <- data_file$State
	states_names_unique <- unique(states_names_total)
	
	
	
	condition_unique <- c("heart attack","pneumonia", "heart failure")
	
	if(is.element(state,states_names_unique) && is.element(outcome,condition_unique)){
		
		outcomes <- TRUE
		}
	else if( !is.element(state,states_names_unique) && !is.element(outcome,condition_unique)){
		return("Please choose state and a real outcome")
	}
	else if( is.element(state,states_names_unique) && !is.element(outcome,condition_unique)){
		return("Please choose a real condition")
		
	}
	
	else if( !is.element(state,states_names_unique) && is.element(outcome,condition_unique)){
		return("Please choose a real state")
		
	}
	
	
	if(outcome == "heart attack" ){
		outcome_text <-"Heart.Attack"
		
	}
	else if(outcome == "pneumonia"){
		outcome_text <-"Pneumonia"
		
	}
	else if(outcome == "heart failure"){
		outcome_text <-"Heart.Failure"
		
	}
	
	condition_text <- "Hospital.30.Day.Death..Mortality..Rates.from."
	condition_outcome_text <- paste(condition_text, outcome_text, sep="")
	#numeric_file <- as.numeric(data_file)
	
	#### return hospital name
	subset_vector <- c(state, outcome)
	subsetted_file_state <- subset(data_file, data_file[["State"]] == state)
	
	
	condition_data <- as.numeric(subsetted_file_state[[condition_outcome_text]])
	bad_condition_data <- is.na(condition_data)
	cleanded_condition_data <- condition_data[!bad_condition_data]
	
	min_cleanded_condition_data <- min(cleanded_condition_data)
	
	hospital_name <- subsetted_file_state[["Hospital.Name"]]
	
	extracted_data_frame <- data.frame(hospital_name ,condition_data)
	ordered_extracted_data_frame <-  extracted_data_frame[order(condition_data),]
	
	
	winner_condition <- subset(extracted_data_frame, condition_data == min_cleanded_condition_data)
	####### Rank Alphabetically
	if(nrow(winner_condition) == 1){
		winner_hospital <- winner_condition[["hospital_name"]]
		return(winner_hospital)
	}
	else{
		
		ordered_winner_table <- winner_condition[order[,hospital_name],]
		
		winner_hospital <- ordered_winner_table[1,1]
		return(winner_hospital)
	}
}

rankhospital <- function(state, outcome, rank){
	## state ---- Abv name of state ("TX","MD")
	## outcome --- "heart attack","pneumonia", "heart failure"
	## rank -------- "best","worst","1","2"
	########## Read File
	data_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#### check outcomes
	
	states_names_total <- data_file$State
	states_names_unique <- unique(states_names_total)
	
	condition_unique <- c("heart attack","pneumonia", "heart failure")
	
	if(is.element(state,states_names_unique) && is.element(outcome,condition_unique)){
		
		outcomes <- TRUE
		}
	else if( !is.element(state,states_names_unique) && !is.element(outcome,condition_unique)){
		return("Please choose state and a real outcome")
	}
	else if( is.element(state,states_names_unique) && !is.element(outcome,condition_unique)){
		return("Please choose a real condition")
		
	}
	
	else if( !is.element(state,states_names_unique) && is.element(outcome,condition_unique)){
		return("Please choose a real state")
		
	}
	
	
	if(outcome == "heart attack" ){
		outcome_text <-"Heart.Attack"
		
	}
	else if(outcome == "pneumonia"){
		outcome_text <-"Pneumonia"
		
	}
	else if(outcome == "heart failure"){
		outcome_text <-"Heart.Failure"
		
	}
	
	condition_text <- "Hospital.30.Day.Death..Mortality..Rates.from."
	condition_outcome_text <- paste(condition_text, outcome_text, sep="")
	#numeric_file <- as.numeric(data_file)
	
	#### return hospital name
	subset_vector <- c(state, outcome)
	subsetted_file_state <- subset(data_file, data_file[["State"]] == state)
	
	
	condition_data <- as.numeric(subsetted_file_state[[condition_outcome_text]])
	bad_condition_data <- is.na(condition_data)
	cleanded_condition_data <- condition_data[!bad_condition_data]
	
	min_cleanded_condition_data <- min(cleanded_condition_data)
	
	hospital_name <- subsetted_file_state[["Hospital.Name"]]
	
	extracted_data_frame <- data.frame(hospital_name ,condition_data)
	ordered_extracted_data_frame <-  extracted_data_frame[order(condition_data, hospital_name),]
	
	ranks <-c(1:nrow(ordered_extracted_data_frame))
	ordered_extracted_data_frame["rank"] <- ranks  
	nonNA_ordered_extracted_data_frame   <- subset(ordered_extracted_data_frame, condition_data != is.na(condition_data))
	
	total_hospitals <- nrow(nonNA_ordered_extracted_data_frame)
	total_hospitals
	
	if(rank == "best"){
		rank <- 1
	
	}
	else if(rank == "worst"){
		rank <- total_hospitals
		
	}
	else{
		rank
	}
	
	chosen_hospital <- ordered_extracted_data_frame[rank,1]
	
	chosen_hospital
	
}

############################################################################################################

rankall <- function(outcome, rank = "best"){
	
	## outcome --- "heart attack","pneumonia", "heart failure"
	## rank -------- "best","worst","1","2"
	########## Read File
	data_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#### check outcomes
	
	states_names_total <- data_file$State
	states_names_unique <- unique(states_names_total)
	states_names_unique	< states_names_unique[order(states_names_unique)]
	
	condition_unique <- c("heart attack","pneumonia", "heart failure")
	
	if(is.element(outcome,condition_unique)){
		
		outcomes <- TRUE
		}
	else if(!is.element(outcome,condition_unique)){
		return("Please choose a real outcome")
	}
	
	
	
	if(outcome == "heart attack" ){
		outcome_text <-"Heart.Attack"
		
	}
	else if(outcome == "pneumonia"){
		outcome_text <-"Pneumonia"
		
	}
	else if(outcome == "heart failure"){
		outcome_text <-"Heart.Failure"
		
	}

	condition_text <- "Hospital.30.Day.Death..Mortality..Rates.from."
	condition_outcome_text <- paste(condition_text, outcome_text, sep="")
	
	
	hospitals <- c()
	
	
	i <- 1
	
	while(i <= length(states_names_unique)){
		subsetted_state_data_frame <-  subset(data_file, data_file["State"] == states_names_unique[i] & data_file[condition_outcome_text] != "Not Available")
		subsetted_state_vector <-subsetted_state_data_frame$State
		subsetted_hospital_vector <-subsetted_state_data_frame$Hospital.Name
		subsetted_condition_vector <- as.numeric(subsetted_state_data_frame[[condition_outcome_text]])
		
		if(rank == "best"){
		rank <- 1
	
		}
		else if(rank == "worst"){
		rank <- length(subsetted_hospital_vector)
		
		}
		else{
		rank
		}
	
		
		my_data_frame <- data.frame(subsetted_hospital_vector,subsetted_state_vector,subsetted_condition_vector)
		my_data_frame <- my_data_frame[order(subsetted_condition_vector),]
		hospitals[i] <- as.character(my_data_frame[rank,1])
		
	
	  i <- i + 1
	  
	  
		}

 	my_table <- data.frame(states_names_unique, hospitals)
	data <- c(subsetted_hospital_vector, subsetted_state_vector, subsetted_condition_vector)
	my_table
}




a <- function(outcome, rank = "best"){
	## outcome --- "heart attack","pneumonia", "heart failure"
	## rank -------- "best","worst","1","2"
	########## Read File
	data_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	#### check outcomes
	
	states_names_total <- data_file$State
	states_names_unique <- unique(states_names_total)
	states_names_unique	< states_names_unique[order(states_names_unique)]
	
	condition_unique <- c("heart attack","pneumonia", "heart failure")
	
	if(is.element(outcome,condition_unique)){
		
		outcomes <- TRUE
		}
	else if(!is.element(outcome,condition_unique)){
		return("Please choose a real outcome")
	}
	
	
	
	if(outcome == "heart attack" ){
		outcome_text <-"Heart.Attack"
		
	}
	else if(outcome == "pneumonia"){
		outcome_text <-"Pneumonia"
		
	}
	else if(outcome == "heart failure"){
		outcome_text <-"Heart.Failure"
		
	}

	condition_text <- "Hospital.30.Day.Death..Mortality..Rates.from."
	condition_outcome_text <- paste(condition_text, outcome_text, sep="")
	
	
	hospitals <- c()
	
	
	i <- 1
	
	while(i <= length(states_names_unique)){
	
		
		if(rank == "best"){
		rank <- 1
	
		}
		else if(rank == "worst"){
		rank <- 3000
		subsetted_state_data_frame <-  subset(data_file, data_file[condition_outcome_text] != "Not Available")
		
		splitted_data <- split(subsetted_state_data_frame, subsetted_state_data_frame$State)
		state_hospitals <- lapply(splitted_data,nrow)
		un_listed	<- unlist(state_hospitals)
		
		
		
		
		#subsetted_state_vector <-subsetted_state_data_frame$State
		#subsetted_hospital_vector <-subsetted_state_data_frame$Hospital.Name
		#subsetted_condition_vector <- as.numeric(subsetted_state_data_frame[[condition_outcome_text]])
		
		
		#my_data_frame <- data.frame(subsetted_hospital_vector,subsetted_state_vector,subsetted_condition_vector)
		#my_data_frame <- my_data_frame[order(subsetted_condition_vector),]
		#hospitals[i] <- as.character(my_data_frame[rank,1])
		
		#my_table <- data.frame(states_names_unique, hospitals)
	#data <- c(subsetted_hospital_vector, subsetted_state_vector, subsetted_condition_vector)
	#return(my_table)
			
			
			
				
		}
		else{
		rank
		}
		
		subsetted_state_data_frame <-  subset(data_file, data_file["State"] == states_names_unique[i] & data_file[condition_outcome_text] != "Not Available")
		subsetted_state_vector <-subsetted_state_data_frame$State
		subsetted_hospital_vector <-subsetted_state_data_frame$Hospital.Name
		subsetted_condition_vector <- as.numeric(subsetted_state_data_frame[[condition_outcome_text]])
		
		my_data_frame <- data.frame(subsetted_hospital_vector,subsetted_state_vector,subsetted_condition_vector)
		my_data_frame <- my_data_frame[order(subsetted_condition_vector),]
		hospitals[i] <- as.character(my_data_frame[rank,1])
		
	
	  i <- i + 1
	  
	  
		}

 	
	
	
}