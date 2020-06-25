library(mcp)
library(tidybayes)
library(changepoint)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggthemes)
library(ggplot2)
library(stringr)
library(RcppRoll)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

state <- counties %>% filter(state == "Arizona")

one_county <- state %>% filter(county == "Maricopa")

one_county <- one_county %>% 
  mutate(sequence = as.numeric(date)) %>%
  arrange(date) %>%
  mutate(daily_deaths = deaths - lag(deaths),
         daily_cases = cases - lag(cases)) %>%
  replace_na(list(daily_deaths = 0,
                  daily_cases = 0)) %>%
  mutate(mean_cases_7 = roll_mean(daily_cases, 7, fill=0, align="right"),
         mean_deaths_7 = roll_mean(daily_deaths, 7, fill=0, align="right")) %>%
  ungroup()

# use two-phase changepoint detection:
# 1) changepoint package for quick search 
# 2) build model from quick search and use
#    that model in mcp package for more 
#    thorough search

fit_changepoint = cpt.meanvar(one_county$mean_cases_7, method = "AMOC")
plot(fit_changepoint)
str(fit_changepoint)

cps <- cpts(fit_changepoint)

# maybe add the change point locations as priors, somehow? Right now,
# just the number of changepoints is a "prior" of sorts
#
# using 1 + number of changepoints above as an aggressive attempt
# to find changepoints. Not sure if sound...
model <- c(mean_cases_7 ~ sequence, 
           rep_len(c(1~1), 
                   1 + ncpts(fit_changepoint))) %>% 
  as.list()

model

fit <- mcp(model, data = one_county)
plot(fit)
fit

cp_df <- fit$mcmc_post %>%
  tidy_draws() %>%
  summarize_all(mean) %>%
  select(starts_with("cp_")) %>%
  pivot_longer(cols = starts_with("cp_"), 
               names_to = "changepoint", 
               values_to = "sequence") %>%
  mutate(sequence = floor(sequence))

one_county <- one_county %>% left_join(cp_df) %>% 
  fill(changepoint, .direction="up") %>% 
  replace_na(list(changepoint = "cp_n")) %>%
  group_by(changepoint) %>%
  mutate(mean_7_mean = mean(mean_cases_7)) %>%
  ungroup() %>%
  mutate(cummax = cummax(daily_cases),
         is_highpoint = cummax == daily_cases)

extract_segments <- function(one_county) {
  one_county %>% 
    group_by(changepoint) %>% 
    summarize(xmin = min(date)-days(1), xmax=max(date))%>%
    mutate(changepoint = factor(changepoint)) %>%
    mutate(shade = as.numeric(changepoint) %%2)
}

one_county %>%
  pivot_longer(cols = c(mean_cases_7, daily_cases), names_to = "type", values_to = "count") %>%
  ggplot() +
  geom_rect(data = extract_segments, aes(xmin = xmin, xmax=xmax, fill = as.factor(shade)), ymin=0, ymax=Inf, color=NA) +
  scale_fill_manual(guide=FALSE, values = c("#eeeeee", "#e0e0e0")) +
  geom_line(aes(x=date, y=count, color=type)) +
  scale_color_manual(NULL,values = c("gray", "black"), labels = c("Daily", "7-day average")) +
  geom_point(aes(x=date, y=count, shape = is_highpoint, size = type), color="black", show.legend = FALSE) +
  scale_size_manual(NULL, values = c(1.5,0)) + # zero-sized if on the average line
  scale_shape_manual(NULL, values = c(NA,1)) + # no shape if not a highpoint
  labs(
    title = paste(min(one_county$county), "County", min(one_county$state)),
    subtitle = max(one_county$date),
    caption = str_wrap("Shading shows break points in data, circles are new highpoints",60),
    y = "Cases",
    x = "Date"
  ) + theme_few() +
  theme(
    #legend.position="none"
  )

dpi=100
img_name <- "one-county-hosp-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

