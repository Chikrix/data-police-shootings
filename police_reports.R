library(tidyverse)
library(readr)

police_shootings <- read_csv("fatal-police-shootings-data.csv")
head(police_shootings)
colnames(police_shootings)
min(police_shootings$date)
max(police_shootings$date)
police_shootings$date

### Number of police killings since 2nd Jan 2015 - 7th Dec 2017
max_day_in_df <- max(police_shootings$date)
min_day_in_df <- min(police_shootings$date)
max_day_in_df - min_day_in_df
(no_of_shootings <- nrow(police_shootings))

ordered_shootings <- police_shootings %>%
  arrange(date) %>%
  mutate(shooting_days = weekdays(date))

colnames(ordered_shootings)

ordered_shootings %>%
  group_by(shooting_days) %>%
  summarise(average_age_per_day = mean(age, na.rm = TRUE),
            shootings_per_day = length(age))

# The average age of a person shot in the US between 2015 and 2017 is between 36 - 37. The weekday with the highest number of police shooting is Wednesday. Although it doesn't seem this even matters (would have to do some test for that)

table(police_shootings$flee, useNA = "always")/nrow(police_shootings)
# From the above, about 66% of people shot were not fleeing from the police when they were shot

police_shootings %>%
  filter(flee == "Not fleeing") %>%
  group_by(race) %>%
  summarise(shootings_fleeing = length(race)/nrow(.))

# Interestingly, of those not fleeing shot by the police, ~ 51% of them where whites, and 22% of them blacks. I thought otherwise though.

police_shootings %>%
  group_by(race) %>%
  summarise(shootings_fleeing = length(race)/nrow(.))

# About similar thing happens in the full dataset (without filtering for "not fleeing"), with ~ 49 percent whites.
