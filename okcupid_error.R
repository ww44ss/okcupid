## code

library(okcupiddata)
library(dplyr)

## clean and convert to tibble
data_cleaned <- filter(profiles, !is.na(age), !is.na(sex), age > 21, age < 80) %>%
                as_data_frame() %>% select(sex, age)

age_count <- data_cleaned %>% group_by(age, sex) %>% summarize(n_age = n())
sex_count <- data_cleaned %>% group_by(sex) %>% summarize(n_sex = n())

analyzed_data <- left_join(age_count, sex_count, by = "sex") %>% mutate(percent = 100.*n_age/n_sex)

male <- analyzed_data %>% filter(sex == "m")
female <- analyzed_data %>% filter(sex == "f")

wide_cleaned <- inner_join(male, female, by = "age")

wide_analyzed <- wide_cleaned %>% mutate(percent_difference = percent.x - percent.y)
