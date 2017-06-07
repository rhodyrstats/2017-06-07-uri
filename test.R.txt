download.file("https://ndownloader.figshare.com/files/2292169",
              "data/portal_data_joined.csv")
surveys <- read.csv('data/portal_data_joined.csv')
tail(surveys)
colnames(surveys)
library(dplyr)
surveys2 <- mutate(surveys,weight_kg2=weight/1000)
surveys$weight_kg = surveys$weight/1000
survey_means <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))
surveys %>%
  group_by(plot_type) %>%
  tally
levels(surveys$sex)
levels(surveys$sex)[1] <- "missing"
levels(surveys$sex) <- c("missing", "Male","Female")
surveys %>%
  group_by(sex) %>%
  tally
#reload
levels(surveys$sex) <- c("missing", "Female","Male")
surveys$sex <- factor(surveys$sex, levels = c("missing", "Male","Female"))
t<-surveys %>%
  group_by(year) %>%
  tally

library(lubridate)
surveys$date<-ymd(paste(surveys$year, surveys$month, surveys$day, sep="-"))
surveys[is.na(surveys$date),2:4]

surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

library(tidyr)
surveys_gw_wide <- surveys_gw %>%
  spread(genus, mean_weight, fill=0)


surveys_gw_wide_cor <- surveys_gw %>%
  spread(genus, mean_weight, fill=0) %>%
  cor(use = "pairwise.complete")


wide_long2 <- surveys_gw_wide %>%
  gather(genus, mean_weight, -plot_id) %>%
  filter(mean_weight>0)

surveys_long <-surveys %>% gather(measurement, value, hindfoot_length:weight)
measurements_summary <- surveys_long %>% filter(!is.na(value)) %>% 
  group_by(measurement,year,plot_type) %>%
  summarize(meanval = mean(value))

measurements_summary2 <- measurements_summary %>% spread(measurement, meanval)


surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")  
species_counts <- surveys_complete %>%
  group_by(species_id) %>%
  tally %>%
  filter(n >= 50)
