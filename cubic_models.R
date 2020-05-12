library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(ggthemes)
library(Cairo)

# transforms JHU data from wide to long, and just use the US numbers
filter_pivot <- function(tib){
  tib <- tib %>% filter(`Country/Region` == "US") %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    pivot_longer(-`Country/Region`, names_to = "date", values_to = "count")
  
  tib %>% 
    mutate(date = mdy(date)) %>%
    rename(region = `Country/Region`) %>% 
    group_by(region, date) %>% summarize(sum_ct = sum(count))
}

# build a model and return the predicted values based on
# the cutoff_date and all the data. The data is filtered
# to include only less-than-or-equal-to the cutoff_date
pred_on_week <- function(cutoff_date, cd_orig) {
  
  cutoff_num <- as.numeric(cutoff_date) - as.numeric(min(cd_orig$date))
  cd <- cd_orig %>% filter(date_num <= cutoff_num)
  
  # boom, here it is. a cubic model
  mod <- lm(log1p(delta_casualty) ~ poly(date_num, 3, raw=TRUE), 
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

# Get the latest from JHU
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

# from last monday to the monday before the national emergency
the_weeks <- seq(floor_date(ymd("2020-03-13"), unit = "week", week_start =1),
                 floor_date(today(), unit = "week", week_start = 1),
                 by = "1 week")

# process
preds <- the_weeks %>% map_dfr(pred_on_week,cd_orig)

my_pal <- c(ipsum_pal()(9), ggthemes::few_pal("Dark")(8))

# plot
ggplot(preds) + 
  geom_line(aes(x=date, y=expm1(prediction), color=data_date, group=data_date), size=1.1) +
  geom_line(data = cd_orig, aes(x=date, y=delta_casualty), size=2, alpha=0.5) +
  scale_color_manual("Backtest date\nto build model", values = my_pal) +
  theme_ipsum() +
  labs(
    title = "Backtesting cubic models over time",
    subtitle = "COVID-19 Casualties",
    caption = paste("Each model trained with data available on date",
                    "https://github.com/schnee/covid-19/blob/master/cubic_models.R",
                    sep="\n"),
    y = "Casualties per day"
  ) + ylim(0,2*max(cd_orig$delta_casualty, na.rm=TRUE))

dpi <- 100
ggsave("cubic-fark.png", width = 850 / dpi, height = 679/dpi , dpi=dpi, type = "cairo")
