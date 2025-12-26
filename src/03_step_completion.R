library(dplyr)

step_activity2 <- step_activity %>%
  mutate(visited = !is.na(first_visited_at),
         completed= !is.na(last_completed_at))

 #step level summary
step_summary <- step_activity2 %>%
  group_by(run, week_number, step_number) %>%
  summarise(visits= sum(visited),
            completions= sum(completed),
            completion_rate= completions/visits,
            .groups ="drop")
step_summary

#finding steps with low completion rate
step_summary %>%
  filter(visits >0 )%>%
  arrange(completion_rate) %>%
  head(10)

dim(step_summary)
head(step_summary)
summary(step_summary$completion_rate)
