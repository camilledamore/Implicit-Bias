#### Analysis of IBT based on Race
library(tidyverse)
library(janitor)
library(snakecase)
library(stringr)


#load ibt data

ibt_data <- read_csv("ibt_testdata.csv")

###CLEAN THE DATA!
 
# Remove the first 2 rows from ibt_data (garbage -- dont need to know how long ppl had the tab open lol)

ibt_data <- ibt_data[-c(1, 2), ]

#snakecase
colnames(ibt_data) <- to_snake_case(colnames(ibt_data))
# Assuming ibt_data is your dataset
library(stringr)  # Load stringr package for string manipulation

# Find column names containing "q_6" and replace it with "q6"
colnames(ibt_data) <- str_replace_all(colnames(ibt_data), "q_6", "q6")

# Selecting columns
ibt_data <- ibt_data[, 
c("duration_in_seconds", 
"finished", "q65_page_submit",
"q66_page_submit", "race_ethnicity", 
"english")]


#remove all unfinished tests (all where finished == "FALSE")
ibt_data_finished<- ibt_data %>% 
                    filter(finished=="TRUE")

race_count <- ibt_data_finished %>%
               group_by(race_ethnicity) %>%
               summarize(n = n())
  


ibt_data_clean <- ibt_data_finished %>%
  mutate(race = case_when(grepl("asi", race_ethnicity) ~ "asian",
                          grepl("Asi", race_ethnicity) ~ "asian",
                          grepl("Chi", race_ethnicity) ~ "asian",
                          grepl("chi", race_ethnicity) ~ "asian",
                          grepl("korean", race_ethnicity) ~ "asian",
                          grepl("Korean", race_ethnicity) ~ "asian",
                          grepl("white", race_ethnicity) ~ "white",
                          grepl("White", race_ethnicity) ~ "white")) %>%

  mutate(race=ifelse(is.na(race), 'other', race)) %>%

  mutate(race = case_when(
    race %in% c("white", "asian") ~ race,
    TRUE ~ "other"
  ))


ibt_data_clean <- ibt_data_clean %>%
  mutate(
    q66_page_submit = as.numeric(as.character(q66_page_submit)),
    q65_page_submit = as.numeric(as.character(q65_page_submit)),
    duration_in_seconds = as.numeric(duration_in_seconds)
  )



### ANALYSE

race_count_clean <- ibt_data_clean %>%
  group_by(race) %>%
  summarize(n = n())

#q65 (exp_)

avg_q65_time <- mean(ibt_data_clean$q65_page_submit)

q65_stat <- ibt_data_clean %>% 
  mutate(q65_avg = ifelse(q65_page_submit >= avg_q65_time, "above_average", "below_average"))


race_q65_stat <- q65_stat %>% 
  tabyl(race, q65_avg) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  rename("race/q65_indicator"="race")


#q66 (control)


avg_q66_time <- mean(ibt_data_clean$q66_page_submit)

q66_stat <- ibt_data_clean %>% 
  mutate(q66_avg = ifelse(q66_page_submit >= avg_q66_time, "above_average", "below_average"))


race_q66_stat <- q66_stat %>% 
  tabyl(race, q66_avg) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  rename("race/q66_indicator"="race")


## GRAPH


ggplot(q65_stat_long, aes(x = english, y = completion_time, fill = english)) +
  geom_violin() +
  geom_jitter(position = position_nudge(x = 0), alpha = 0.25) +  
  geom_hline(data = avg_times, aes(yintercept = avg_time, color = question, linetype = "solid"), size = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = c("Average Time for q65" = "black", "Average Time for q66" = "black"), 
                     name = "Average Response Time", 
                     labels = c("q65" = "Q65 Average", "q66" = "Q66 Average")) +
  facet_wrap(~ question, scales = "free_y", labeller = labeller(question = c("q65_page_submit" = "q65", "q66_page_submit" = "q66"))) +
  labs(title = "Completion Times for Questions q65 and q66 by Race",
       fill = "Race",
       y = "Completion Time (seconds)") +
  # y-axis limits -- synchronize scales
  coord_cartesian(ylim = c(0, max(q65_stat_long$completion_time, na.rm = TRUE)))
  
