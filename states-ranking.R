library(tidyverse)
library(RcppRoll)
library(hrbrthemes)
library(ggthemes)

covid_state <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# I thought I was going to be able to use the daily's to do some factors, (fct_lump_n), but
# turns out that some states have negative daily numbers (corrections to the accounting?) 
# so nope
covid_state <- covid_state %>% 
  group_by(fips) %>% 
  arrange(date) %>%
  mutate(daily_cases = replace_na(cases - lag(cases),0),
         daily_deaths = replace_na(deaths - lag(deaths),0))%>%
  ungroup()

# the last row is the current (today() - 1 day) tally, so find the
# top n

top_count <- 10

top_by_deaths_tib = covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = deaths) %>% arrange(desc(deaths))

top_by_cases_tib = covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = cases) %>% arrange(desc(cases))

# and now create factors so that we can plot easier. Create a new level
# that will accumulate all of the "others"

covid_state <- covid_state %>% 
  mutate(top_by_deaths = if_else(state %in% top_by_deaths_tib$state, state, "Other"),
         top_by_cases = if_else(state %in% top_by_cases_tib$state, state, "Other")) %>%
  mutate(top_by_deaths = factor(top_by_deaths, levels = c(top_by_deaths_tib$state, "Other")),
         top_by_cases = factor(top_by_cases, levels = c(top_by_cases_tib$state, "Other")))

# and now start accumulating by the factors
covid_sl <- covid_state %>%
  group_by(top_by_deaths, date) %>%
  summarize(deaths = sum(deaths)) %>%
  ungroup() 

# calculate a 7-day moving average (a rolling mean)
covid_sla <- covid_sl %>% 
  group_by(top_by_deaths) %>%
  arrange(date) %>%
  mutate(delta_d = deaths - lag(deaths),
         mean_7 = roll_mean(delta_d, 7, fill=0, align="right"))

my_pal <- c(ipsum_pal()(9), few_pal()(8), colorblind_pal()(8))

# and plot the deaths
covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=top_by_deaths)) + geom_area() +
  theme_ipsum() +
  scale_fill_manual("States", values = my_pal) +
  labs(
    title = "COVID-19 New Deaths",
    subtitle = "7 day moving average",
    y = "Count"
  )

# do everything that we did above for deaths, but for cases
covid_sl <- covid_state %>%
  group_by(top_by_cases, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() 

covid_sla <- covid_sl %>% 
  group_by(top_by_cases) %>%
  arrange(date) %>%
  mutate(delta_c = cases - lag(cases),
         mean_7 = roll_mean(delta_c, 7, fill=0, align="right"))


covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=top_by_cases)) + geom_area() +
  theme_ipsum() +
  scale_fill_manual("States", values = my_pal) +
  labs(
    title = "COVID-19 New Cases",
    subtitle = "7 day moving average",
    y = "Count"
  )

  
