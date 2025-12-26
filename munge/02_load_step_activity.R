#listing everything downloaded
#all_names <- ls()

#finding objects that has "step-activity"
step_names <- all_names[grep("_step\\.activity$", all_names)]

#combining step activity tables into list
step_list <- lapply(step_names, get)

# combining them into one dataframe
step_activity <- bind_rows(step_list, .id = "source_name")

#creating run numbers from source names

temp <- sub("cyber\\.security\\.","", step_activity$source_name)
temp <- sub("_.*", "", temp)
step_activity$run <- as.integer(temp)

#no missing values detected in step_activity

#handling empty strings
#empty strings at last_completed_at values indicate steps that were visited but not completed, values retained as NA
step_activity <- step_activity %>%
  mutate(last_completed_at=na_if(last_completed_at, ""))

#converting visit and completion times to datetime format for analysis
step_activity <- step_activity %>%
  mutate(first_visited_at = as.POSIXct(first_visited_at),
         last_completed_at=as.POSIXct(last_completed_at))

cache("step_activity")
