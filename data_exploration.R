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
# check how many observations per week in dataset 
weekly_obs <- #waste_dat_21 %>% 
  waste_dat %>%
  group_by(week, year) %>%
  summarise(count = n()) %>%
  arrange(year)
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
# look at 2020 as well 
weekly_waste_all <- waste_dat %>%
  filter(week != 53) %>%
  mutate(week_cum = ifelse(year == 2021, week + 52, week)) %>%
  group_by(week_cum) %>%
  mutate(obs_num = n()) %>% 
  summarise(virus_in_week = sum(virus_per_liter),
            virus_normed = virus_in_week/obs_num) %>%
  unique()
ggplot(weekly_waste_all, aes(x = week_cum, y = virus_normed)) + 
  geom_bar(stat = "identity") + 
  xlab("Week") + 
  ylab("Average Virus Copies per Liter") + 
  ggtitle("Weekly Observed Virus in Lynden in 2021") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("virus_2020_2021.png")

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
# look into 2020 as well 
weekly_cases <- lynden_case_dat %>%
  group_by(week, year) %>% 
  summarise(count = n()) %>%
  arrange(year)
lynden_plot_dat <- lynden_case_dat %>%
  select(-date) %>%
  rbind(c(NA, 7, 2021)) %>%
  arrange(year, week) %>%
  mutate(week_cum = ifelse(year == 2021, week + 52, week))
ggplot(lynden_plot_dat, aes(x = week_cum, y = weekly_cases)) + 
  geom_bar(stat = "identity") + 
  xlab("Week") + 
  ylab("Number of Cases") + 
  ggtitle("Weekly Covid Cases in Lynden") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("case_2020_2021.png")

# plot time series for cases and wastewater on same axes
lynden_case_dat_21_shift <- lynden_case_dat_21
lynden_case_dat_21_shift$week <- lynden_case_dat_21_shift$week - 1
weekly_dat <- full_join(weekly_waste, lynden_case_dat_21_shift, by = "week") %>%
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
# also look at 2020
lynden_plot_dat_shift <- lynden_plot_dat
lynden_plot_dat_shift$week_cum <- lynden_plot_dat_shift$week_cum - 1
weekly_dat_all <- full_join(weekly_waste_all, lynden_plot_dat, by = "week_cum") 
weekly_dat_all$rel_cases <- weekly_dat_all$weekly_cases/
  max(weekly_dat_all$weekly_cases, na.rm = T)  
weekly_dat_all$rel_virus <- weekly_dat_all$virus_normed/
  max(weekly_dat_all$virus_normed, na.rm = T)
trends_plot <- ggplot(weekly_dat_all, aes(x = week_cum, y = rel_cases)) + 
  geom_line(color = "red") + 
  geom_point(color = "red") + 
  geom_line(aes(y = rel_virus), color = "blue") + 
  geom_point(aes(y = rel_virus), color = "blue") + 
  xlab("Week") + 
  ylab("Relative Number of Cases/Relative Virus Load") + 
  ggtitle("Trends in Cases (red) and Virus (blue) in Lynden in 2021") + 
  theme(plot.title = element_text(hjust = 0.5))
trends_plot
ggsave("trends_2020_2021.png")


