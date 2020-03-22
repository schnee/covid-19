library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)

filter_pivot <- function(tib){
  tib <- tib %>% filter(`Country/Region` == "US") %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    pivot_longer(-`Country/Region`, names_to = "date", values_to = "count")
  
  tib %>% 
    mutate(date = mdy(date)) %>%
    rename(region = `Country/Region`) %>% 
    group_by(region, date) %>% summarize(sum_ct = sum(count))
}
  

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

covid_case_longer <- filter_pivot(covid) %>% 
  rename(infections = sum_ct)

covid_death_longer <- filter_pivot(covid_death) %>% 
  rename(deaths = sum_ct)

covid_longer_j <- covid_case_longer %>% 
  left_join(covid_death_longer)

# TODO move this data to google sheet
events <- tribble(
  ~date, ~who, ~desc,
  ymd("2020-01-22"), "Donald Trump","totally under control",
  ymd("2020-02-02"), "Donald Trump", "We pretty much shut it down",
  ymd("2020-01-30"), "Donald Trump", "we have it very well under control",
  ymd("2020-02-29"), "Donald Trump", "Everything is really under control",
  ymd("2020-03-13"), "", "US Declares Natl Emergency",
  ymd("2020-02-25"), "Donald Trump","is very well under control in our country",
  ymd("2020-02-25"), "Rush Limbaugh", "well under control",
  ymd("2020-02-26"), "Donald Trump", "we're going to be pretty soon at only five people",
  ymd("2020-02-28"), "Tucker Carslon", "warns viewers",
  ymd("2020-02-28"), "Tucker Carslon", "warns social and economic",
  ymd("2020-03-06"), "Marc Siegel", "worst case scenario — it could be the flu",
  ymd("2020-03-07"), "Jeanine Pirro", "more deadly [than the flu] doesn’t reflect reality",
  ymd("2020-03-08"), "Ted Cruz", "Everyone should continue to treat this outbreak seriously",
  ymd("2020-03-09"), "Trish Regan", "impeachment scam",
  ymd("2020-03-09"), "Sean Hannity", "let's bludgeon Trump with this new hoax",
  ymd("2020-03-11"), "Matt Schlapp", "hard to get",
  ymd("2020-03-13"), "Sean Hannity", "slim to no chance of contracting",
  ymd("2020-03-13"), "Laura Ingraham", "great time to fly",
  ymd("2020-03-13"), "Donald Trump", "I don't take responsibility at all",
  ymd("2020-03-14"), "Chip Roy", "...free toilet paper for all",
  ymd("2020-03-15"), "Donald Trump", "Have tremendous control over",
  ymd("2020-03-03"), "Jesse Waters", "downplays",
  ymd("2020-03-16"), "Donald Trump", "Chinese Virus",
  ymd("2020-03-18"), "John Cornyn", "China is to blame...eats bats and snakes and dogs",
  ymd("2020-03-19"), "Mary Colbert", "in China,...eating bats and snakes...unclean animal",
  ymd("2020-03-19"), "Donald Trump", "could have been stopped ... [if known about it earlier]",
  ymd("2020-03-20"), "Donald Trump", "Coming together is much harder when we have dishonest journalists",
  ymd("2020-03-20"), "Donald Trump","there is a very low incidence of death"
) %>% arrange(date) %>% 
  left_join(covid_case_longer) %>% 
  mutate(label = paste(who, desc, sep=": "),
         label = str_wrap(label, width = 60))

levels <- events %>% group_by(who) %>% tally() %>% arrange(desc(n))

#color the top 20% of the commentors, just because
the_most <- levels %>% top_frac(.2, n) %>% nrow()

events <- events %>% mutate(who = factor(who, levels = levels$who))

rose_colored_glasses <- "#FF9ECF"

covid_longer_j %>%
  ggplot() + 
  geom_col(aes(x=date, y=infections)) +
  geom_area(aes(x=date, y=deaths*10), fill = "red", alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Deaths")) +
  #scale_y_log10() +
  labs(
    title = "COVID-19 US Cases",
    y = "Infections",
    x = "Date",
    caption = "Confirmed cases: https://github.com/CSSEGISandData/COVID-19\nLabels: media and tweets"
  ) + 
  #geom_vline(xintercept = events$date) +
  geom_label_repel(data = events %>% arrange(desc(who)), 
                   aes(x=date, y=infections, label = label, fill = who),
                   arrow = NULL, direction="y", hjust = 1, size = 3.5,
                   box.padding = 1,
                   xlim = c(NA,ymd("2020-03-01")),
                   ylim = c(100, max(covid_case_longer$infections)),
                   show.legend = FALSE) +
  scale_fill_manual(values = c(rep(rose_colored_glasses, the_most), rep("white", nrow(levels) - the_most ))) +
  theme_few()
