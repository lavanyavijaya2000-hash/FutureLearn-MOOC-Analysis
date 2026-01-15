#checking how big the tables are
dim(enrolments)
dim(step_activity)
dim(question_response)

#checking the column names
names(enrolments)
names(step_activity)
names(question_response)

#checking the data types
str(enrolments)
str(step_activity)
str(question_response)

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


# analysing the outcomes by runs(completion, purchase, unenrolment)
outcomes_by_run <-learner_summary %>%
  group_by(run) %>%
  summarise(learners =n(),
            completed_n = sum(completed_course),
            purchased_n = sum(purchased_cert),
            unenrolled_n= sum(unenrolled))
outcomes_by_run
#outcomes (percentages) by runs
outcomes_by_run <- outcomes_by_run %>%
  mutate(completed_pct = completed_n/ learners * 100,
         purchased_pct = purchased_n/ learners * 100,
         unenrolled_pct= unenrolled_n/learners * 100)
outcomes_by_run

#conversion completion rate to percentage
step_summary <- step_summary%>%
  mutate(completion_pct = completion_rate * 100)

step_summary %>%
  arrange(completion_pct)%>%
  head(10)


#analysing learners outcomes by each run
plot_data <- outcomes_by_run %>%
  select(run,completed_pct, purchased_pct, unenrolled_pct) %>%
  pivot_longer(
    cols= c(completed_pct,purchased_pct, unenrolled_pct),
    names_to= "outcomes",
    values_to= "percent"
  )
plot_data




#purchase rate among learners who completed the course

#learners who completed
completers_by_run <- learner_summary %>%
  filter(completed_course == TRUE) %>%
  group_by(run) %>%
  summarise( completers_n= n(),
             purchasers_among_completers= sum(purchased_cert),
             .groups = "drop")

completers_by_run <- completers_by_run %>%
  mutate(purchase_rate_among_completers = purchasers_among_completers / completers_n * 100)



step_activity2 <- step_activity %>%
  mutate(visited = !is.na(first_visited_at),
         completed= !is.na(last_completed_at))

step_summary <- step_activity2 %>%
  group_by(run, week_number, step_number)%>%
  summarise(visits=sum(visited),
            completions= sum(completed),
            completion_pct = completions/ visits * 100,
            .groups = "drop")

#finding lowest completion steps (top 10 overall)
low_completion_steps <- step_summary %>%
  filter(visits > 100) %>%
  arrange (completion_pct) %>%
  head(10)

low_completion_steps <- low_completion_steps %>%
  arrange(step_number)

low_completion_steps %>%
  select(run, week_number, step_number, completion_pct)



#for each learner finding the furthest step they have reached
learner_max_step <- step_activity %>%
  filter(!is.na(first_visited_at))%>%
  group_by(run, learner_id)%>%
  summarise(max_step= max(step_number),
            .groups= "drop")

retention_by_step <- learner_max_step%>%
  group_by(run) %>%
  mutate(total_learners = n()) %>%
  ungroup() %>%
  group_by(run, max_step, total_learners) %>%
  summarise(count = n(), .groups = "drop")
retention_by_step

#what percentage of learners are active in each step and each run
retention_curve <- retention_by_step %>%
  group_by(run) %>%
  arrange(max_step)%>%
  mutate(active_learners = rev(cumsum(rev(count))),
         active_pct = active_learners/ total_learners * 100) %>%
  ungroup()
retention_curve




#calculating at which stage biggest drop-off occur
dropoff_by_step <- retention_curve %>%
  group_by(run) %>%
  arrange(max_step) %>%
  mutate(prev_active = lag(active_pct),
         dropoff_pct= prev_active - active_pct) %>%
  ungroup()

largest_drop_off_by_run <- dropoff_by_step %>%
  filter(!is.na(dropoff_pct)) %>%
  group_by(run) %>%
  slice_max(dropoff_pct, n=1, with_ties = FALSE) %>%
  select(run, max_step, dropoff_pct)
largest_drop_off_by_run

largest_drop_off_by_run <- largest_drop_off_by_run%>%
  rename(step_number = max_step)
largest_drop_off_by_run

#summary of datasets
data_overview <- data.frame(
  Dataset = c("Enrolments", "Step activity", "Question responses"),
  Rows = c(
    nrow(enrolments),
    nrow(step_activity),
    nrow(question_response)
  ),
  Columns = c(
    ncol(enrolments),
    ncol(step_activity),
    ncol(question_response)
  )
)

data_overview

# step_summary already exists with completion_pct

step_overall <- step_summary %>%
  group_by(step_number) %>%
  summarise(
    avg_completion_pct = mean(completion_pct, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(step_overall, aes(x = step_number, y = avg_completion_pct)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(min(step_overall$step_number),
                 max(step_overall$step_number), by = 2)
  ) +
  scale_y_continuous(
    limits = c(80, 100),
    breaks = seq(80, 100, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Course step number (ordered learning sequence)",
    y = "Average completion percentage across learners"
  )

cache("data_overview")
cache("completion_rate")
cache("learner_summary")
cache("outcomes_by_run")
cache("plot_data")
cache ("completers_by_run")
cache("step_activity2")
cache("step_summary")
cache("low_completion_steps")
cache("learner_max_step")
cache("retention_by_step")
cache("dropoff_by_step")
cache("largest_drop_off_by_run")
cache("step_summary")