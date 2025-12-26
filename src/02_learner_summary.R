library(dplyr)


#creating learner summary
enrolments2 <- enrolments %>%
  mutate(completed_course = !is.na(fully_participated_at),
         purchased_cert= !is.na(purchased_statement_at),
         unenrolled= !is.na(unenrolled_at))

#creating one row per learner to count completion/purchase/unenrollement easily
learner_summary <- enrolments2 %>%
  group_by(run,learner_id) %>%
  summarise(completed_course = any(completed_course),
            purchased_cert= any(purchased_cert),
            unenrolled=any(unenrolled),
            .groups = "drop")
table(learner_summary$completed_course)
table(learner_summary$purchased_cert)
table(learner_summary$unenrolled)

