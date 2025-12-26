library(dplyr)
library(ggplot2)

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

ggplot(plot_data, aes(x= factor(run), y= percent, fill= outcomes))+
  geom_col(position="dodge")+
  labs(x="course run",
       y= "percentage of learners (%)",
       title= "learner outcomes by course run",
       fill= "outcomes")

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


ggplot(completers_by_run, aes(x= factor(run),
                            y= purchase_rate_among_completers)) +
  geom_col(fill= "steelblue")+
  labs(x= "Course run",
       y= "certificate purchase rate among completors (%)",
       title= "Certificate purchase rate among course completers by run")

