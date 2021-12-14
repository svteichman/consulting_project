library(tidyverse)

# read in dataset 
lynden_dat <- readxl::read_excel("lynden_data_modified.xlsx",
                                 col_names = TRUE)[,-c(3:5)]
colnames(lynden_dat) <- c("date", "virus_per_liter", "weekly_cases", "count_2_week",
                          "county_cum_count", "county_1_week_increase")

# fix date 
lynden_dat$date <- as.Date(lynden_dat$date, origin = "1899-12-30")
lynden_dat$week <- lubridate::isoweek(lynden_dat$date) # define week starting on a Monday
lynden_dat$year <- lubridate::year(lynden_dat$date)
# correct year for 2nd and 96th observations, they are miscoded in original dataset as 2021 
lynden_dat$year[2] <- 2020
lynden_dat$year[96] <- 2020

# make dataset for wastewater 
waste_dat <- lynden_dat %>% 
  select(c(date, virus_per_liter, week, year))
waste_dat <- waste_dat[!is.na(waste_dat$virus_per_liter), ]
waste_dat_21 <- waste_dat %>% filter(year == 2021) %>% select(-year)
# check how many observations per week in 2021 dataset 
weekly_obs <- waste_dat_21 %>% 
  group_by(week) %>%
  summarise(count = n())
# need to find out eventually how to define week! 
# for now, do M-Su, scale sum of wastewater by number of observations in that week
weekly_waste <- waste_dat_21 %>%
  filter(week != 53) %>%
  group_by(week) %>%
  mutate(obs_num = n()) %>% 
  summarise(virus_in_week = sum(virus_per_liter),
            virus_normed = virus_in_week/obs_num) %>%
  unique()
ggplot(weekly_waste, aes(x = week, y = virus_normed)) + 
  geom_bar(stat = "identity") + 
  xlab("Week") + 
  ylab("Average Virus Copies per Liter") + 
  ggtitle("Weekly Observed Virus in Lynden in 2021") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("virus_bar.png")

# make dataset for lynden cases 
lynden_case_dat <- lynden_dat %>% 
  select(c(date, weekly_cases, week, year))
lynden_case_dat$week <- lubridate::week(lynden_case_dat$date)
lynden_case_dat <- lynden_case_dat[!is.na(lynden_case_dat$weekly_cases), ]
lynden_case_dat_21 <- lynden_case_dat %>% filter(year == 2021) %>% select(-c(year))  
lynden_case_dat_21 <- rbind(lynden_case_dat_21, c(NA, 7)) %>%
  arrange(week, desc = F)
ggplot(lynden_case_dat_21, aes(x = week, y = weekly_cases)) + 
  geom_bar(stat = "identity") + 
  xlab("Week") + 
  ylab("Number of Cases") + 
  ggtitle("Weekly Covid Cases in Lynden in 2021") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("cases_bar.png")

# plot time series for cases and wastewater on same axes
weekly_dat <- full_join(weekly_waste, lynden_case_dat_21, by = "week") %>%
  select(-date) %>%
  filter(week != 27)
weekly_dat$rel_cases <- weekly_dat$weekly_cases/max(weekly_dat$weekly_cases, na.rm = T)  
weekly_dat$rel_virus <- weekly_dat$virus_normed/max(weekly_dat$virus_normed, na.rm = T)
trends_plot <- ggplot(weekly_dat, aes(x = week, y = rel_cases)) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_line(aes(y = rel_virus), color = "blue") + 
  geom_point(aes(y = rel_virus), color = "blue") + 
  xlab("Week") + 
  ylab("Relative Number of Cases/Relative Virus Load") + 
  ggtitle("Trends in Cases (red) and Virus (blue) in Lynden in 2021") + 
  theme(plot.title = element_text(hjust = 0.5))
trends_plot
ggsave("trends.png")
trends_plot +  
  ylab("Cases/Virus") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  ggtitle("Trends in Cases (red) and Virus (blue)")
ggsave("simple_trends.png")
