library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(hrbrthemes)
library(forcats)
library(Cairo)
devtools::load_all("./covidutil/")

filter_pivot <- function(tib){
  tib <- tib %>% filter(`Country/Region` == "US") %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    pivot_longer(-`Country/Region`, names_to = "date", values_to = "count")
  
  tib %>% 
    mutate(date = mdy(date)) %>%
    rename(region = `Country/Region`) %>% 
    group_by(region, date) %>% summarize(sum_ct = sum(count))
}

images <- list()
  

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

covid_case_longer <- filter_pivot(covid) %>% 
  rename(infections = sum_ct)

covid_death_longer <- filter_pivot(covid_death) %>% 
  rename(deaths = sum_ct)

onset_to_death <- 13

covid_longer_j <- covid_case_longer %>% 
  left_join(covid_death_longer) %>%
  mutate(cfr = deaths / infections,
         cfr_lag = deaths / lag(infections, n=onset_to_death)) 

events <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTt1di48F1DAjek8K95-spPQIJDxvmJFKuPP9tZYmzJMSA6zwKMqfB14CA-1BT42dk6rRyDhH_hKDEM/pub?gid=1160102752&single=true&output=csv")
    
events <- events %>% arrange(date) %>% #filter(who == "Donald Trump") %>%
  left_join(covid_case_longer) %>% 
  mutate(label = paste(who, desc, sep=": "),
         lbl_len= str_length(label),
         who = fct_lump_n(who, 3)) %>%  
  rowwise() %>%
  mutate(label = if_else(lbl_len > 60, str_wrap(label, width = lbl_len / 3), label))

alphas <- c(0.85, rep(0.5, length(levels(events$who)) -2), 0.2)

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

crazy_plot <- covid_longer_j %>% 
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
                     "Casualties:", scales::label_comma()(max(covid_longer_j$deaths)),
                     "CFR:", scales::percent_format(accuracy = 0.01)(covid_longer_j %>% pull(cfr) %>% last())),
    y = "Infections",
    x = "Date",
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\nLabels: media and tweets\n",
                     today())
  ) + 
  geom_label_repel(data = events %>% filter(date <= max(covid_longer_j$date)),
                   aes(x=date, y=infections, label = label, alpha = who),
                   arrow = NULL, 
                   force = 10,
                   max.iter = 50000,
                   direction="both", 
                   hjust = 1, size = 2.5,
                   segment.alpha= 0.5,
                   segment.color = "white",
                   box.padding = 1,
                   xlim = c(min(covid_case_longer$date), today() - days(20)),
                   ylim = c(100, max(covid_case_longer$infections)),
                   show.legend = FALSE) +
  scale_alpha_manual(values = alphas) +
  theme_modern_rc(grid = FALSE) +
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  geom_rug(data = casualties %>% filter(ct < 3*max(covid_longer_j$deaths)), 
           aes(y=ct*10, color = what), sides="r", size = 2) +
  scale_color_few(palette = "Dark", "Reference\nCasualties")

img_name <- "covid-crazy.png"
ggsave(plot = crazy_plot, filename = img_name, width = 16, height=9, dpi = 100, type = "cairo")

images <- c(images, img_name)

covid_longer_j %>% ungroup() %>% arrange(date) %>% 
  mutate(delta_casualty = deaths - lag(deaths),
         delta_infection = infections - lag(infections)) %>%
  select(date, delta_casualty, delta_infection) %>%
  pivot_longer(cols = -date, names_to = 'type', values_to = 'ct') %>%
  ggplot(aes(x=date, y=ct, color = type)) + geom_line(size=1.5) +
  scale_y_log10( labels = scales::label_comma(accuracy = 1)) +
  theme_modern_rc() +
  scale_color_ipsum() +
  labs(
    title = "US COVID-19 Daily Cases"
  )

dpi <- 100
ggsave("delta-fark.png", width = 850 / dpi, height = 679/dpi , dpi=dpi, type = "cairo")

img_name <- "delta-wide.png"
ggsave(img_name, width = 16, height = 9, dpi = dpi, type = "cairo")

images <- c(images, img_name)

covid_longer_j %>%
  ggplot() + 
  geom_col(aes(x=date, y=infections), width=1) +
  geom_area(aes(x=date, y=deaths*10), fill = "red", alpha = 0.75, position = position_nudge((x=0.5))) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Fatalities", 
                                         labels = scales::label_comma()), 
                     labels = scales::label_comma()) +
  #scale_y_log10() +
  labs(
    title = "COVID-19 US Cases",
    subtitle = paste("Cases:",scales::label_comma()(max(covid_longer_j$infections)), 
                     "Fatalities:", scales::label_comma()(max(covid_longer_j$deaths)),
                     "CFR:", scales::percent_format(accuracy =0.01)(covid_longer_j %>% pull(cfr) %>% last())),
    y = "Cases",
    x = "Date",
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\n",
                     today())
  ) + 
  theme_modern_rc(grid = FALSE) +
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  geom_rug(data = casualties %>% filter(ct < 3*max(covid_longer_j$deaths)), 
           aes(y=ct*10, color = what), sides="r", size = 1) +
  scale_color_few(palette = "Dark", "Reference\nFatalities") +
  theme(legend.position = "bottom")

dpi <- 100
ggsave("covid-fark.png", width = 850 / dpi, height = 679/dpi , dpi=dpi, type = "cairo")

img_name <- "covid-wide.png"
ggsave(img_name, width = 16, height = 9, dpi = dpi, type = "cairo")

images <- c(images, img_name)

covid_longer_j %>%
  ggplot(aes(x=date, y=cfr_lag)) +
  geom_point()  +
  labs(
    title = "COVID-19 US Case Fatality Rate",
    subtitle = paste0(onset_to_death," days onset-to-death lag\n",
               "Current Infections: ",scales::label_comma()(max(covid_longer_j$infections)), 
               "\nCurrent Casualties: ", scales::label_comma()(max(covid_longer_j$deaths))),
    y = "Case Fatality Rate",
    x = "Date",
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\n",
                     today())
  ) + 
  theme_modern_rc(grid = FALSE) + 
  scale_y_percent(limits = c(0,0.2))

dpi <- 100
ggsave("cfr-fark.png", width = 850 / dpi, height = 679/dpi , dpi=dpi, type = "cairo")

covidutil::gauth(email= "schneeman@gmail.com")
images %>% map(covidutil::upload_images)
