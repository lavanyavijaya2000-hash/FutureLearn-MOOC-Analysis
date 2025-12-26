#listing everything downloaded
all_names <- ls()

#finding the objects that end with "_enrolments"
enrol_names <- all_names[grep("_enrolments$",all_names)]

#combining enrolment tables into list
enrol_list <- lapply (enrol_names, get)

#combining into one dataframe
enrolments <- bind_rows(enrol_list, .id = "source_name")

#creating run numbers from the source names
#removing symbols
temp <- sub("cyber\\.security\\.", "",enrolments$source_name)
#removing symbols after underscore
temp <- sub("_.*","",temp)
#converting as integer
enrolments$run <- as.integer(temp)

#handling missing country information
enrolments <- enrolments%>%
  mutate(country= replace_na(country, "Unknown"),
         detected_country= replace_na(detected_country, "Unknown"))

#handling empty strings
#empty strings in timestamp field indicate events that did not occur (eg,learner did not unenrol,
#did not fully participate or did not purchase ) these are structural and retained;so converting the empty strings to NA for anlysis
enrolments <- enrolments %>%
  mutate(across(c(unenrolled_at,fully_participated_at, purchased_statement_at), ~na_if(., "")))

#converting date columns to datetime format for analysis
enrolments <- enrolments %>%
  mutate(enrolled_at=as.POSIXct(enrolled_at),
         unenrolled_at= as.POSIXct(unenrolled_at),
         fully_participated_at=as.POSIXct(fully_participated_at),
         purchased_statement_at=as.POSIXct(purchased_statement_at))

#saving processed data
cache('enrolments')





