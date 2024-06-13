#### Analysis of Effect of Language -- native vs non native eng speakers

library(tidyverse)
library(janitor)
library(snakecase)
library(stringr)


#import experiment data
ibt_data <- read_csv("ibt_testdata.csv")



# Remove the first 2 rows from ibt_data
ibt_data <- ibt_data[-c(1, 2), ]
#snakecase


### DATA CLEANING! 
colnames(ibt_data) <- to_snake_case(colnames(ibt_data))


#chanfe colnames w "q_6" to "q6"
colnames(ibt_data) <- str_replace_all(colnames(ibt_data), "q_6", "q6")

# Selecting columns
ibt_data <- ibt_data[, 
                     c("duration_in_seconds", 
                       "finished", "q65_page_submit",
                       "q66_page_submit", "race_ethnicity", 
                       "english")]


#remove incomplete tests (take out all entries w finished == "FALSE")
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


##########

# Analyse 

#q65
  avg_q65_time <- mean(ibt_data_clean$q65_page_submit)

q65_stat <- ibt_data_clean %>% 
  mutate(q65_avg = ifelse(q65_page_submit >= avg_q65_time, "above_average", "below_average"))



native_q65_stat <- q65_stat %>% 
  tabyl(english, q65_avg) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  rename("native english/q65_indicator"="english")


#q66
avg_q66_time <- mean(ibt_data_clean$q66_page_submit)

q66_stat <- ibt_data_clean %>% 
  mutate(q66_avg = ifelse(q66_page_submit >= avg_q66_time, "above_average", "below_average"))



native_q66_stat <- q66_stat %>% 
  tabyl(english, q66_avg) %>% 
  adorn_totals(c("row","col")) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  rename("native english/q66_indicator"="english")



### GRAPH

ggplot(q65_stat_long, aes(x = english, y = completion_time, fill = english)) +
  geom_violin() +
  geom_jitter(position = position_nudge(x = 0), alpha = 0.25) +  
  geom_hline(data = avg_times, aes(yintercept = avg_time, color = question, linetype = "solid"), size = 0.5, show.legend = TRUE) +
  scale_fill_manual(values = colors) +  
  scale_color_manual(values = c("Average Time for q65" = "black", "Average Time for q66" = "black"), 
                     name = "Average Response Time", 
                     labels = c("q65" = "Q65 Average", "q66" = "Q66 Average")) +
  facet_wrap(~ question, scales = "free_y", labeller = labeller(question = c("q65_page_submit" = "q65", "q66_page_submit" = "q66"))) +
  labs(
       fill = "Native English Speaker",
       y = "Completion Time (seconds)") +
  coord_cartesian(ylim = c(0, max(q65_stat_long$completion_time, na.rm = TRUE)))


