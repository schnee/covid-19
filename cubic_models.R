library(tidyverse)

filter_pivot <- function(tib){
  tib <- tib %>% filter(`Country/Region` == "US") %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    pivot_longer(-`Country/Region`, names_to = "date", values_to = "count")
  
  tib %>% 
    mutate(date = mdy(date)) %>%
    rename(region = `Country/Region`) %>% 
    group_by(region, date) %>% summarize(sum_ct = sum(count))
}

pred_on_week <- function(cutoff_date, cd_orig) {
  
  #cutoff_date <- ymd("2020-05-04") - weeks(weeks_ago)
  cutoff_num <- as.numeric(cutoff_date) - as.numeric(min(cd_orig$date))
  cd <- cd_orig %>% filter(date_num <= cutoff_num)
  
  # boom, here it is. a cubic model
  mod <- lm(delta_casualty ~ poly(date_num, 3, raw=TRUE), 
            data=cd)
  
  the_range <- cd %>% 
    select(date_num) %>% 
    range()
  
  num_grid = seq(from=min(the_range) , to = max(the_range)+30)
  
  preds <- predict(mod, newdata=list(date_num = num_grid))
  
  tibble(
    data_date = format(cutoff_date),
    date_num = num_grid,
    prediction = preds
  ) %>% mutate(date = min(cd_orig$date) + days(date_num))

}


covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

covid_death_longer <- filter_pivot(covid_death) %>% 
  rename(deaths = sum_ct)


cd_orig <- covid_death_longer %>%
  filter(date >= ymd("2020-02-26")) #@cae's data seems to start here

# calc the per-day casualties, and convert the date to
# a simple (and small) integer
cd_orig <- cd_orig %>% 
  mutate(delta_casualty = deaths - lag(deaths),
         date_num = as.numeric(date) - as.numeric(min(date))) %>%
  ungroup()

# last 6 weeks
the_weeks <- seq(ymd("2020-05-04") - weeks(5), 
                 ymd("2020-05-04") - weeks(0),
                 by = "1 week")

# process
preds <- the_weeks %>% map_dfr(pred_on_week,cd_orig)

# plot
ggplot(preds) + 
  geom_line(aes(x=date, y=prediction, color=data_date, group=data_date), size=1.1) +
  geom_line(data = cd_orig, aes(x=date, y=delta_casualty), size=2) +
  scale_color_ipsum("Maximum data date\nto build model") +
  theme_ipsum() +
  labs(
    title = "Cubic models over time",
    subtitle = "COVID-19 Casualties",
    y = "Casualties per day"
  ) +
  ylim(-200,5000)
