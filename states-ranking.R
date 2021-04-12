library(tidyverse)
library(RcppRoll)
library(hrbrthemes)
library(ggthemes)
library(lubridate)
library(Cairo)
library(patchwork)
library(ggridges)
devtools::load_all("./covidutil/")

images <- list()

states_to_remove <- c("Guam", "Northern Mariana Islands", "Virgin Islands")

# read from the NY Times state-level covid data, and from the US Census
# population data, and from jakevdp's github repo for the state areas

covid_state <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  filter(!state %in% states_to_remove)

state_pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv") %>%
  mutate(NAME = if_else(NAME == "Puerto Rico Commonwealth", "Puerto Rico", NAME) ) %>%
  filter(!NAME %in% states_to_remove)

state_area <- read_csv("https://raw.githubusercontent.com/jakevdp/data-USstates/master/state-areas.csv", 
                       col_names = c("state", "area"),
                       skip = 1) %>%
  filter(!state %in% states_to_remove)

# join the pop with the area, and compute the population density

state_pop <- state_pop %>%
  right_join(state_area, by=c("NAME" = "state")) %>%
  mutate(density = POPESTIMATE2019 / area) %>% 
  select(NAME, POPESTIMATE2019, area, density)


# I thought I was going to be able to use the daily's to do some factors,
# (fct_lump_n), but turns out that some states have negative daily numbers
# (corrections to the accounting?) so nope. Keep the daily_ data incase I want
# to do something with it later.

# Join (by state name) the covid data to the population data. Calculate the
# per-100k numbers. Group by the FIPS code and arrange by date to calculate the
# daily_ numbers (replacing NA with 0 as we go).

covid_state <- covid_state %>% 
  left_join(state_pop, by = c("state" = "NAME")) %>%
  mutate(cases_per_100k = cases / (POPESTIMATE2019 / 100000),
         deaths_per_100k = deaths / (POPESTIMATE2019 / 100000)) %>%
  group_by(fips) %>% 
  arrange(date) %>%
  mutate(daily_cases = replace_na(cases - lag(cases),0),
         daily_deaths = replace_na(deaths - lag(deaths),0))%>% 
  mutate(daily_cases_per_100k = daily_cases / (POPESTIMATE2019 / 100000),
         daily_deaths_per_100k = daily_deaths / (POPESTIMATE2019 / 100000)) %>%
  ungroup() %>%
  select(-density)

# the last row is the current (today() - 1 day) tally, so find the
# top n

top_count <- 10

top_by_deaths_tib <- covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = deaths) %>% arrange(desc(daily_deaths))

top_by_cases_tib <- covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = cases) %>% arrange(desc(daily_cases))

top_by_deaths_per_100k_tib <- covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = deaths_per_100k) %>% arrange(desc(deaths_per_100k))

top_by_cases_per_100k_tib <- covid_state %>% filter(date == max(date)) %>%
  top_n(top_count, wt = cases_per_100k) %>% arrange(desc(cases_per_100k))

# and now create factors so that we can plot easier. Create a new level
# that will accumulate all of the "others"

covid_state <- covid_state %>% 
  mutate(top_by_deaths = if_else(state %in% top_by_deaths_tib$state, state, "Other"),
         top_by_cases = if_else(state %in% top_by_cases_tib$state, state, "Other"),
         top_by_deaths_per_100k = if_else(state %in% top_by_deaths_per_100k_tib$state, state, "Other"),
         top_by_cases_per_100k = if_else(state %in% top_by_cases_per_100k_tib$state, state, "Other")) %>%
  mutate(top_by_deaths = factor(top_by_deaths, levels = c(top_by_deaths_tib$state, "Other")),
         top_by_cases = factor(top_by_cases, levels = c(top_by_cases_tib$state, "Other")),
         top_by_deaths_per_100k = factor(top_by_deaths_per_100k, levels = c(top_by_deaths_per_100k_tib$state, "Other")),
         top_by_cases_per_100k = factor(top_by_cases_per_100k, levels = c(top_by_cases_per_100k_tib$state, "Other")))

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

# and plot the deaths by raw death count
death_area <- covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=top_by_deaths)) + geom_area(color=NA) +
  theme_modern_rc() +
  scale_fill_manual("States", values = covid_pal) +
  labs(
    title = "COVID-19 Deaths",
    subtitle = "7 day moving average",
    y = "Count",
    caption = max(covid_sla$date)
  )
dpi <- 100
ggsave(plot=death_area, "deaths-area-7day-ma.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "deaths-area-7day-ma-wide.png"
ggsave(plot=death_area, img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)


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


cases_area <- covid_sla %>%
  ggplot(aes(x=date, y=mean_7, fill=top_by_cases)) + geom_area(color=NA) +
  theme_modern_rc() +
  scale_fill_manual("States", values = covid_pal) +
  labs(
    title = "COVID-19 Cases",
    subtitle = "7 day moving average",
    y = "Count",
    caption = max(covid_sla$date)
  )
dpi <- 100
ggsave(plot=cases_area, "cases-area-7day-ma.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "cases-area-7day-ma-wide.png"
ggsave(plot=cases_area, img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

# get the date of the first death

first_death <- covid_state %>% filter(deaths > 0) %>%
  pull(date) %>% min

# get the deaths and the deaths per 100k
states_deaths_per_100k_tib <- covid_state %>%
  select(date, state, deaths, deaths_per_100k)

# create a dummy table with all states and all dates, filled with zeros
dummy <- states_deaths_per_100k_tib %>% complete(date, state, fill = list(deaths_per_100k = 0,
                                                                          deaths = 0)) %>%
  left_join(state_pop, by=c("state" = "NAME"))

# join in the actual deaths "on top of" the dummies. This will give us a nice complete 
# table of all cases
sdp100k <- dummy %>% 
  left_join(states_deaths_per_100k_tib) %>%
  group_by(date) %>%
  arrange(desc(deaths_per_100k), desc(density), state) %>%
  mutate(ranking = row_number()) %>%
  ungroup()

lhs <- sdp100k %>% filter(date == first_death - days(2)) %>% 
  select(state, ranking, density) 

rhs <- sdp100k %>% filter(date == max(date)) %>%
#  filter(state %in% lhs$state) %>%
  select(state, ranking, deaths_per_100k)  
  
# gonna cheat a bit and convert the color aesthetic from state names to 
# an integer. this allows for use of scale_color_gradient directly.
sdp100k %>%
  filter(date >= first_death - days(2)) %>%
  filter(state %in% lhs$state) %>% 
  mutate(state = factor(state, levels = lhs$state)) %>%
  ggplot(aes(y=ranking, group = state)) + 
  geom_line(aes(x=date, color = as.integer(state)), size=1.1) + 
  scale_y_reverse() + 
  scale_color_viridis_c(option = "plasma", begin=0.4)+
  geom_text(data = lhs, aes(label=state), x= first_death - days(3),
             hjust = 1,
             size = 3.5) +
  geom_text(data = rhs, aes(label=state), x= max(sdp100k$date) + days(1),
            hjust = 0,
            size = 3.5) +
  coord_cartesian(xlim = c(first_death - days(16), 
                           max(sdp100k$date) + days(15))) +
  theme_modern_rc() +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "COVID-19 Deaths Per 100k population",
    subtitle = max(sdp100k$date),
    caption = "Ordered by {Deaths Per 100k, Population Density, State Name} by Day",
    y = NULL
  )

dpi <- 100
ggsave("deaths-ranking.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "deaths-ranking-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

corr <- signif(cor(sdp100k$density, sdp100k$deaths_per_100k), digits = 2)

sdp100k %>%
  filter(date == max(date)) %>%
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x=density, y=deaths_per_100k)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_modern_rc() +
  labs(
    title = "Population Density v Deaths per 100K",  
    subtitle = paste("Correlation:",corr),
    y = "Deaths per 100k",
    x = "Population Density (people per square mile)",
    caption = max(sdp100k$date)
  )


dpi <- 100
ggsave("deaths-cor.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "deaths-cor-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

# get the date of the first case
first_case <- covid_state %>% filter(cases > 0) %>%
  pull(date) %>% min

# get the cases and the cases per 100k
states_cases_per_100k_tib <- covid_state %>%
  select(date, state, cases, cases_per_100k)

# create a dummy table with all states and all dates, filled with zeros
# add in a row that starts before the first case so that the initial
# ordering is by population density
dummy <- states_cases_per_100k_tib %>% 
  bind_rows(tibble(date = min(states_cases_per_100k_tib$date) - days(1))) %>%
  complete(date, state, fill = list(cases_per_100k = 0,
                                    cases = 0)) %>%
  left_join(state_pop, by=c("state" = "NAME"))

# join in the actual cases "on top of" the dummies. This will give us a nice complete 
# table of all cases
scp100k <- dummy %>% 
  left_join(states_cases_per_100k_tib) %>%
  group_by(date) %>%
  arrange(desc(cases_per_100k), desc(density), state) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  filter(!is.na(state))

lhs <- scp100k %>% filter(date == min(date)) %>% 
  select(state, ranking, density) 

rhs <- scp100k %>% filter(date == max(date)) %>%
  #  filter(state %in% lhs$state) %>%
  select(state, ranking, cases_per_100k)  

scp100k %>%
  #filter(date >= first_case - days(2)) %>%
  filter(state %in% lhs$state) %>% 
  mutate(state = factor(state, levels = lhs$state)) %>%
  ggplot(aes(y=ranking, group = state)) + 
  geom_line(aes(x=date, color = as.integer(state)), size=1.1) + 
  scale_y_reverse() + 
  scale_color_viridis_c(option = "viridis", begin=0.4)+
  geom_text(data = lhs, aes(label=state), x= first_case - days(3),
            hjust = 1,
            size = 3.5) +
  geom_text(data = rhs, aes(label=state), x= max(scp100k$date) + days(1),
            hjust = 0,
            size = 3.5) +
  coord_cartesian(xlim = c(first_case - days(16), 
                           max(scp100k$date) + days(15))) +
  theme_modern_rc() +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "COVID-19 Cases Per 100k population",
    subtitle = max(scp100k$date),
    caption = "Ordered by {Cases Per 100k, Population Density, State Name} by Day",
    y = NULL
  )

dpi <- 100
ggsave("cases-ranking.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "cases-ranking-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

corr <- signif(cor(scp100k$density, scp100k$cases_per_100k), digits = 2)

scp100k %>%
  filter(date == max(date)) %>%
  filter(state != "District of Columbia") %>% 
  ggplot(aes(x=density, y=cases_per_100k)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_modern_rc() +
  labs(
    title = "Population Density v Cases per 100K",
    subtitle = paste("Correlation:",corr),
    y = "Cases per 100k",
    x = "Population Density (people per square mile)",
    caption = max(scp100k$date)
  )

dpi <- 100
ggsave("cases-cor.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")

img_name <- "cases-cor-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)
# 
# scp100k %>% group_by(state) %>% arrange(date) %>%
#   mutate(week_ago = lag(ranking,7), delta = lag(ranking, 7)- ranking) %>% 
#   ungroup() %>%
#   filter(date==max(date)) %>% arrange(desc(delta)) %>% view()
# 
# 
# sdp100k %>% group_by(state) %>% arrange(date) %>%
#   mutate(week_ago = lag(ranking,7), delta = lag(ranking, 7)- ranking) %>% 
#   ungroup() %>%
#   filter(date==max(date)) %>% arrange(desc(delta)) %>% view()

stacked <- cases_area / death_area

img_name <- "stacked_area.png"
ggsave(plot=stacked, img_name, width = 16, height = 18 , dpi=dpi, type = "cairo")

images <- c(images, img_name)


#
# Generate a ridge plot for the states ordered by date of most recent
#

# filter down to the top_n by cases. note, here I'm just using them all
# as the visual is tested

tops <- covid_state %>% 
  filter(date == max(date)) %>%
  top_n(n_distinct(state), wt = cases) 

# calculate delta cases and rolling means and then find the high points
csla <- covid_state %>% 
  filter(state %in% tops$state) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate( delta_c = cases - lag(cases),
          mean_7 = roll_mean(delta_c, n = 7, fill=0, align ="right")) %>%
  ungroup() %>%   replace_na(list(mean_7 = 0)) %>%
  group_by(state) %>%
  mutate(
    cummax = cummax(mean_7),
    is_highpoint = (cummax == mean_7),
    is_highest_high = factor(mean_7 == max(mean_7))
  ) %>%
  ungroup()

# define the ordering for the plot, based on highpoint date
ordered_by_last_highpoint <- csla %>%
  group_by(state) %>%
  filter(is_highpoint) %>% top_n(1, wt = date) %>%
  #  select(top_by_cases, date, mean_7) %>%
  arrange(date, mean_7, state)

scale_factor <- 0.001

csla  %>% 
  mutate(
    state = factor(
      state,
      levels = ordered_by_last_highpoint$state,
      labels = ordered_by_last_highpoint$state
    ),
    shade = as.factor(as.numeric(state) %% 3)
  ) %>%
  ggplot(aes(x=date, y=state, height = mean_7, group=state)) + 
  geom_ridgeline(aes(fill = shade, color=shade), alpha = 0.1, size =.2,
                 scale= scale_factor, show.legend = FALSE) +
  scale_fill_manual(NULL, values = c("#00ffff", "#ff00ff", "#ffff00"))+
  #scale_fill_manual(NULL, values = c(covid_pal[22], covid_pal[21])) +
  geom_point(aes(y=as.numeric(state) + mean_7 * scale_factor, x=date, 
                 shape = is_highest_high, color = shade), 
             stroke = 1,
             show.legend = FALSE) +
  scale_color_manual(NULL, values = c("#00ffff", "#ff00ff", "#ffff00"))+
  #scale_color_manual(NULL, values = c(covid_pal[22], covid_pal[21])) +
  scale_shape_manual(NULL, values = c(NA,1)) + # no shape if not a high point
  theme_modern_rc() +
  labs(
    title = paste("7 day moving average of cases for top",
                  nrow(tops),
                  "States and Territories"),
    subtitle = "Ordered by date of last peak, peak value",
    y= "State",
    caption = paste("Data through ", max(csla$date), 
                    "\nState peaks highlighted by marker")
  ) +
  theme(
    axis.text.y = element_text(size = 8)
  ) +
  xlim(ymd("2020-03-01", max(csla$date)))

dpi <- 100

img_name <- "top-states-ridges-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

covidutil::gauth(email= "schneeman@gmail.com")
images %>% map(covidutil::upload_images)

