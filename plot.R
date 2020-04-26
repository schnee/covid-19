library(tidyverse)
library(googledrive)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(hrbrthemes)
library(forcats)
library(Cairo)

filter_pivot <- function(tib){
  tib <- tib %>% filter(`Country/Region` == "US") %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    pivot_longer(-`Country/Region`, names_to = "date", values_to = "count")
  
  tib %>% 
    mutate(date = mdy(date)) %>%
    rename(region = `Country/Region`) %>% 
    group_by(region, date) %>% summarize(sum_ct = sum(count))
}
  

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

covid_case_longer <- filter_pivot(covid) %>% 
  rename(infections = sum_ct)

covid_death_longer <- filter_pivot(covid_death) %>% 
  rename(deaths = sum_ct)

covid_longer_j <- covid_case_longer %>% 
  left_join(covid_death_longer)

events <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTt1di48F1DAjek8K95-spPQIJDxvmJFKuPP9tZYmzJMSA6zwKMqfB14CA-1BT42dk6rRyDhH_hKDEM/pub?gid=1160102752&single=true&output=csv")

events <- events %>% arrange(date) %>% 
  left_join(covid_case_longer) %>% 
  mutate(label = paste(who, desc, sep=": "),
         lbl_len= str_length(label)) %>%  
  rowwise() %>%
  mutate(label = if_else(lbl_len > 100, str_wrap(label, width = lbl_len / 3), label))

levels <- events %>% group_by(who) %>% tally() %>% arrange(desc(n))

#color the top 20% of the commentors, just because
the_most <- levels %>% top_frac(.2, n) %>% nrow()

events <- events %>% mutate(who = fct_infreq(who))

rose_colored_glasses <- "white" ##FF9ECF"

casualties <- tribble(
  ~what, ~ct,
  "Benghazi", 4,
  "9/11", 2977,
  "Afghanistan", 2440,
  "Vietnam", 58220,
  "Desert Storm", 383,
  "Vince Foster", 1,
  "2017 Las Vegas", 58,
  "H1N1-2009", 12469
) %>% mutate(what = fct_reorder(what, -ct))

covid_longer_j %>%
  ggplot() + 
  geom_col(aes(x=date, y=infections), width=1) +
  geom_area(aes(x=date, y=deaths*10), fill = "red", alpha = 0.75, position = position_nudge((x=0.5))) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Casualties", 
                                         labels = scales::label_comma()), 
                     labels = scales::label_comma()) +
  #scale_y_log10() +
  labs(
    title = "COVID-19 US Cases",
    subtitle = paste("Infections:",scales::label_comma()(max(covid_longer_j$infections)), 
                     "Casualties:", scales::label_comma()(max(covid_longer_j$deaths))),
    y = "Infections",
    x = "Date",
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\nLabels: media and tweets\n",
                     today())
  ) + 
  geom_label_repel(data = events,
                   aes(x=date, y=infections, label = label, fill = who),
                   alpha = 0.85,
                   arrow = NULL, 
                   force = 10,
                   max.iter = 50000,
                   direction="both", 
                   hjust = 1, size = 2.5,
                   box.padding = 1,
                   xlim = c(min(covid_case_longer$date), today() - days(20)),
                   ylim = c(100, max(covid_case_longer$infections)),
                   show.legend = FALSE) +
  scale_fill_manual(values = c(rep(rose_colored_glasses, the_most), rep("white", nrow(levels) - the_most ))) +
  theme_ipsum(grid = FALSE) +
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  geom_rug(data = casualties %>% filter(ct < 3*max(covid_longer_j$deaths)), 
           aes(y=ct*10, color = what), sides="r", size = 2) +
  scale_color_few(palette = "Dark", "Other\nCasualties")

img_name <- "covid-crazy.png"
ggsave(img_name, width = 16, height=9, dpi = 100, type = "cairo")

drive_auth(email= "schneeman@gmail.com")

cc <- drive_find(pattern = "covid_img", n_max = 10)

if(nrow(cc) == 1) {
  drive_put(img_name, path = as_id(cc$id), img_name)
}


covid_longer_j %>% ungroup() %>% arrange(date) %>% 
  mutate(delta_casualty = deaths - lag(deaths),
         delta_infection = infections - lag(infections)) %>%
  select(date, delta_casualty, delta_infection) %>%
  pivot_longer(cols = -date, names_to = 'type', values_to = 'ct') %>%
  ggplot(aes(x=date, y=ct, color = type)) + geom_line(size=1.5) +
  scale_y_log10() +
  theme_ipsum() +
  scale_color_ipsum() +
  labs(
    title = "US COVID-19 Cases"
  )
