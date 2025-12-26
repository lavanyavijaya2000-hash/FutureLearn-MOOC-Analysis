#listing everything downloaded
all_names <- ls()

#finding objects that contain "question.response"
question_names <- all_names[grep("_question\\.response$", all_names)]

#combining question.response tables into list
question_list <- lapply(question_names, get)
question_list <- question_list[sapply(question_list, is.data.frame)]

#combining into one dataframe
question_response <- bind_rows(question_list, .id= 'source_name')

#creating run numbers from source names
temp <- sub(".*\\.", "", question_response$source_name)
temp <- sub("_.*","", temp)
question_response$run <- as.integer(temp)

#missing value s in 'cloze_response' is structurally induced by question design and does not indicate
#data quality issues

#handling empty strings
#empty strings at learner_id value arise from system-generated records, rows retained to preserve response
#level integrity
question_response <- question_response %>%
  mutate(learner_id=na_if(learner_id,""))

#removing duplicate rows to avoid double counting
n_before <-nrow(question_response)

question_response <- question_response%>%
  distinct()
n_after <- nrow(question_response)

n_removed <- n_before-n_after
message("Removed ", n_removed, " exact duplicate rows from question_response")

#converting submission time to datetime format for analysis
question_response <- question_response %>%
  mutate(submitted_at=as.POSIXct(submitted_at))

#saving processed data
cache("question_response")
