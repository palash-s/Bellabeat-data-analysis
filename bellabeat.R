library(tidyverse)


library(skimr)


daily_df = read.csv("/Users/palas/Downloads/archive_fitbit/daily_activity_data.csv")

str(daily_df)
glimpse(daily_df)

skim_without_charts(daily_df)
colnames(daily_df)


###

user_id <- daily_df %>% 
  distinct(Id)

head(user_id)

summary_activity <- daily_df %>% 
select(Id, TotalDistance, Calories) %>%  
group_by(Id) %>%   
summarise( mean_total_dist = mean(TotalDistance), max_total_dist = max(TotalDistance), min_total_dist = min(TotalDistance),
           mean_total_cal = mean(Calories), max_cal = max(Calories), min_cal = min(Calories))

View(summary_activity)


daily_df %>% 
  select(Id, TotalSteps) %>%  
  group_by(Id) %>%   
  summarise( mean_total_steps = mean(TotalSteps), max_total_steps = max(TotalSteps), min_total_steps = min(TotalSteps))



daily_df %>% 
  ggplot(aes(x = TotalDistance, y = Calories, col = Id)) + geom_point() + geom_smooth(method = lm) + 
  labs(title = "relationship between distance and calories")

## from first chart it is clear that some users walking or running shorter distance and burning higher calories.
## While some are walking or running longer dinstace but burning less calories. 


daily_df %>% 
  ggplot(aes(x = TotalDistance, y = Calories)) + geom_point() + facet_wrap(~Id) + 
  labs(title = "relationship between distance and calories for each user")

## here we can clearly see which user usually runs fast or walk particular distance and burns how many calories on an average.


####
## users based on burnt calory

calories_burn <- daily_df %>% 
  
  group_by(Id)


  calories_burn$cal_categories = case_when(
    
    calories_burn$Calories >= 3500 ~ "high cal-loss",
    calories_burn$Calories >= 1500 ~ "mid cal-loss",
    calories_burn$Calories < 1500 ~ "less cal-loss")
  

View(calories_burn)

colnames(calories_burn)


calories_burn %>% 
  ggplot(aes(x = TotalDistance, y = Calories)) + geom_boxplot() +
  facet_wrap(~cal_categories ) + labs(title = "calories burnt vs Total distace by users")

## different users based on calories burnt category


calories_burn %>% 
  ggplot(aes(x = TotalDistance, y = Calories,col = Id, shape = cal_categories)) + geom_point() +
  facet_wrap(~cal_categories ) + labs(title = "calories burnt vs Total distace by users")

less_cal_user <- calories_burn %>% 
  filter(cal_categories == "less cal-loss") %>% 
  group_by(cal_categories) 


less_cal_user %>% 
  ggplot(aes(x = TotalDistance, y = Calories, col = Id)) + geom_point() + facet_wrap(~Id) + 
  labs(title = "relationship between distance and low calories loss")


mid_cal_user <- calories_burn %>% 
  filter(cal_categories == "mid cal-loss") %>% 
  group_by(cal_categories) 


mid_cal_user %>% 
  ggplot(aes(x = TotalDistance, y = Calories, col = Id)) + geom_point() + facet_wrap(~Id) + 
  labs(title = "relationship between distance and mid calories loss users")


high_cal_user <- calories_burn %>% 
  filter(cal_categories == "high cal-loss") %>% 
  group_by(cal_categories) 


high_cal_user %>% 
  ggplot(aes(x = TotalDistance, y = Calories, col = Id)) + geom_point() + facet_wrap(~Id) + 
  labs(title = "relationship between distance and high calories loss users")


