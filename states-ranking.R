
covid_state <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")


covid_sl <- covid_state %>% 
  mutate(lumped_d = fct_lump_n(state, n=8, w=deaths)) %>%
  group_by(lumped_d, date) %>%
  summarize(deaths = sum(deaths)) %>%
  ungroup() 

state_levels <- covid_sl %>% filter(lumped_d != "Other") %>% 
  group_by(lumped_d) %>%
  tally(deaths) %>% 
  mutate(lumped_d = fct_reorder(lumped_d, -n))

state_levels <- fct_expand(state_levels$lumped_d, "Other")

covid_sl$lumped_d = factor(covid_sl$lumped_d, levels(state_levels))

covid_sla <- covid_sl %>% 
  group_by(lumped_d) %>%
  arrange(date) %>%
  mutate(delta_d = deaths - lag(deaths),
         mean_7 = roll_mean(delta_d, 7, fill=0, align="right"))

covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=lumped_d)) + geom_area() +
  theme_ipsum() +
  scale_fill_ipsum("States") +
  labs(
    title = "COVID-19 New Deaths",
    subtitle = "7 day moving average",
    y = "Count"
  )


covid_sl <- covid_state %>% 
  mutate(lumped_d = fct_lump_n(state, n=8, w=cases)) %>%
  group_by(lumped_d, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() 

state_levels <- covid_sl %>% filter(lumped_d != "Other") %>% 
  group_by(lumped_d) %>%
  tally(cases) %>% 
  mutate(lumped_d = fct_reorder(lumped_d, -n))

state_levels <- fct_expand(state_levels$lumped_d, "Other")

covid_sl$lumped_d = factor(covid_sl$lumped_d, levels(state_levels))

covid_sla <- covid_sl %>% 
  group_by(lumped_d) %>%
  arrange(date) %>%
  mutate(delta_i = cases - lag(cases),
         mean_7 = roll_mean(delta_i, 7, fill=0, align="right"))

covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=lumped_d)) + geom_area() +
  theme_ipsum() +
  scale_fill_ipsum("States") +
  labs(
    title = "COVID-19 New Cases",
    subtitle = "7 day moving average",
    y = "Count"
  )

  
