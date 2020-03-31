library(tidyverse)
library(googledrive)
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
  

covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

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
  ymd("2020-02-05"), "Alex Azar", "didn't need additional [emergency] funding", 
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
  ymd("2020-03-20"), "Donald Trump","there is a very low incidence of death",
  ymd("2020-03-22"), "Steve Mnuchin", "Nobody expected this to take off at the rate it did",
  ymd("2020-03-22"), "John Cornyn", "Blah Blah Blah",
  ymd("2020-03-24"), "Kathaleen Wall", "#COVID19 will save more lives this week than it takes!",
  ymd("2020-03-26"), "Donald Trump", "I don't believe you need 40000 or 30000 ventilators",
  ymd("2020-03-27"), "Donald Trump", "I'm not sure anybody even knows what [the coronavirus] is",
  ymd("2020-03-27"), "Donald Trump", "none of [the former Presidents] ever thought a thing like this could happen",
  ymd("2020-03-27"), "Rush Limbaugh", "We didn't elect a president to defer to a bunch of health experts that we don't know",
  ymd("2020-03-29"), "Donald Trump", "the \'Ratings\' of my News Conferences etc. are so high, \'Bachelor finale, Monday Night Football type numbers\'",
  ymd("2020-03-29"), "Donald Trump", "If we have between 100k and 200k deaths, we've altogether done a very good job"
) %>% arrange(date) %>% 
  left_join(covid_case_longer) %>% 
  mutate(label = paste(who, desc, sep=": "),
         label = str_wrap(label, width = 160))

levels <- events %>% group_by(who) %>% tally() %>% arrange(desc(n))

#color the top 20% of the commentors, just because
the_most <- levels %>% top_frac(.2, n) %>% nrow()

events <- events %>% mutate(who = factor(who, levels = levels$who))

rose_colored_glasses <- "white" ##FF9ECF"

covid_longer_j %>%
  ggplot() + 
  geom_col(aes(x=date, y=infections)) +
  geom_area(aes(x=date, y=deaths*10), fill = "red", alpha = 0.75) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Deaths", 
                                         labels = scales::label_comma()), 
                     labels = scales::label_comma()) +
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
                   arrow = NULL, 
                   direction="y", 
                   hjust = 1, size = 3.5,
                   box.padding = 1,
                   xlim = c(NA,ymd("2020-03-01")),
                   ylim = c(100, max(covid_case_longer$infections)),
                   show.legend = FALSE) +
  scale_fill_manual(values = c(rep(rose_colored_glasses, the_most), rep("white", nrow(levels) - the_most ))) +
  theme_few() +
  theme(axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"))

img_name <- "covid-crazy.png"
ggsave(img_name, width = 16, height=9, dpi = 100)

cc <- drive_find(pattern = "covid_img", n_max = 10)

if(nrow(cc) == 1) {
  drive_put(img_name, path = as_id(cc$id), img_name)
}
