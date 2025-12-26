library(dplyr)
library(ggplot2)

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

#finding the lowest completion steps for every run
ggplot(low_completion_steps,
       aes(x= factor(step_number),y= completion_pct))+
  geom_col(fill = "orange")+
  facet_wrap(~run)+
  labs( x= "Step number",
        y= "completion rate (%)",
        title= "Lowest Completion steps by couse runs")

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

ggplot(retention_curve, aes(x= max_step, y=active_pct, colour= factor(run)))+
  annotate("rect", xmin= 19.5, xmax = 21.5, ymin= -Inf, ymax= Inf, alpha =0.08)+
  geom_line(linewidth = 1, alpha= 0.9)+
  scale_y_continuous(limits= c(0,100), breaks= seq(0,100,10))+
  labs( x= "step number",
        y= "learners still active(%)",
        colour = "course run",
        title= " Learner retention (drop-off) curve by course run")+
  theme_minimal(base_size = 12)+
  theme(panel.grid.minor = element_blank())


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

ggplot(largest_drop_off_by_run, aes(x=factor(run), y= dropoff_pct)) +
  geom_point(size = 3) +
  geom_text(aes(label = step_number), vjust =-0.8, size=3)+
  labs(x= "Course run",
       y= "largest single-step drop(percentage points)",
       title= "Biggest drop-off point by course run",
       subtitle= "Number above each point= step where the biggest drop occurs")+ 
  theme_minimal()
  