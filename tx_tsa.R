library(rvest)
library(janitor)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(httr)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(patchwork)
devtools::load_all("./covidutil/")

# get a big palette to allow for lots of layers - this gives a quick way to
# select "top 8, 10, 15, ..." up to "top 24" (because one color is reserved for
# the "Other" group)
#covid_pal <- c(ipsum_pal()(9), few_pal()(8), colorblind_pal()(8))

images <- list()

# yay, parse Wikipedia tables - that's sure to be maintenance free....

url <- "https://en.wikipedia.org/wiki/List_of_counties_in_Texas"
page <- read_html(url)

tables <- html_table(html_nodes(page, css ="table.wikitable.sortable"), fill=TRUE)

# on 2020 06 10, the wiki page consisted of only one table with that css selector,
# and that selector selected the counties table. Let's assert that to be safe
stopifnot(length(tables) == 1)

# and now convert the formerly readable friendly content to 
# machine friendly content, and drop columns that we don't
# care about at this point in time

counties <- tables[[1]] %>% clean_names() %>%
  mutate(county = str_remove(county, "\\s+\\w+$")) %>%
  mutate(population = parse_number(population_10)) %>%
  mutate(area = str_remove(area_9, "\\s+.*$")) %>%
  mutate(area = parse_number(area)) %>%
  select(-etymology, -origin, -est_9, -county_seat_9, 
         -population_10, -area_9, -map)

rm(page, tables, url) 

# now read in the county name to TSA map
c_to_t <- read_csv("./data/county-tsa-map.csv")

# and add the tsa column to the main county table
counties <- counties %>%
  left_join(c_to_t, by = c("county" = "county"))

# read in some facts about each TSA
tsa_fact <-read_csv("./data/tsa-fact.csv", comment = "# ")

# summarize each tsa, and annotate it with the fact table

tsa <- counties %>%
  group_by(tsa) %>%
  summarize(population = sum(population),
            area = sum(area)) %>%
  mutate(pop_density = population / area) %>%
  arrange(desc(pop_density)) %>%
  left_join(tsa_fact, by = c("tsa" = "tsa_id"))

# OK now read in the TSA hospitalization. 

# a little risky b/c TX DSHS is a website, not a data repo: all of their data
# is oriented towards human access. The main URL is
# https://dshs.texas.gov/coronavirus/AdditionalData.aspx

dshs_hosp_url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19HospitalizationsOverTimebyTSA.xlsx"

GET(dshs_hosp_url, write_disk(tf <- tempfile(fileext = ".xlsx", tmpdir="./data")))
tsa_hosp_w <- read_excel(tf, skip = 2)
unlink(tf)

tsa_hosp_w <- tsa_hosp_w %>% filter(!is.na(`TSA ID`))

# It is a wide data frame, we need to make it long. The way the excel file is
# formatted confuses read_excel and the column headers that are Dates in excel
# look like "4/8" instead of "4/8/2020" even though the year is part of the
# cell content. Somehow readxl doesn't like that. This data begins on 4/8
# and the "origin" of the date-based sequence is 1899-12-30 for Excel, so
# use it. Also strip of the trailing period from the TSA ID. ANd just because
# we're ready to, join in the TSA demo data

tsa_hosp <- tsa_hosp_w %>%
  pivot_longer(cols = !starts_with("TSA"), 
               names_to="sequence",
               values_to="hosp_ct") %>% 
  clean_names() %>%
  mutate(sequence = as.numeric(sequence)) %>%
  mutate(date = as.Date(sequence, origin = "1899-12-30")) %>%
  mutate(tsa_id = str_remove(tsa_id, "[\\W]")) %>%
  select(-sequence) %>%
  left_join(tsa, by = c("tsa_id" = "tsa")) %>%
  mutate(hosp_cap = hosp_ct / tsa_beds)

# we will need a couple of dates at the beginning of the tsa_hosp frame
# so that the initial rankings are purely based on population density

tsa_pad <- tsa_hosp %>% 
  bind_rows(tibble(date = seq(from = min(tsa_hosp$date) - days(2), 
                              to = min(tsa_hosp$date) - days(1), 
                              by = "day"))) %>%
  complete(date, nesting(tsa_id, tsa_name.x, tsa_name.y), fill = list(hosp_ct = 0,
                                                                      hosp_cap = 0)) %>%
  select(date, tsa_id, tsa_name.x, tsa_name.y, hosp_ct, hosp_cap) %>%
  filter(!is.na(tsa_id)) %>%
  left_join(tsa, by=c("tsa_id" = "tsa")) %>%
  group_by(date) %>%
  mutate(hosp_per_100k = hosp_ct / (population / 1e5)) %>%
  arrange(desc(hosp_per_100k), desc(pop_density), tsa_id) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  arrange(date, tsa_id)

lhs <- tsa_pad %>% filter(date == min(date)) %>%
  arrange(ranking)

rhs <- tsa_pad %>% filter(date == max(date)) %>% 
  arrange(ranking)

tsa_pad %>% 
  mutate(tsa_name = factor(tsa_name, levels = lhs$tsa_name)) %>%
  ggplot(aes(x=date, y=ranking)) + 
  geom_line(aes(color=tsa_name), size=1.1) +
  scale_y_reverse() +
  scale_color_viridis_d(option = "plasma", begin=0.4) +
  geom_text(data = lhs, aes(label=tsa_name.x), x= min(tsa_pad$date) - days(3),
            hjust = 1,
            size = 3.5) +
  geom_text(data = rhs, aes(label=tsa_name.x), x= max(tsa_pad$date) + days(1),
            hjust = 0,
            size = 3.5) +
  theme_modern_rc() +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "Hospital Occupancy Rankings per 100k per Day",
    subtitle = "TSA relative rankings",
    caption = max(tsa_pad$date)
  ) +
  coord_cartesian(xlim = c(min(tsa_pad$date) - days(16), 
                           max(tsa_pad$date) + days(15))) 

dpi=100
img_name <- "tsa-hosp-ranking-wide.png"
ggsave(img_name, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

hosp_per <- tsa_pad %>% ggplot(aes(x=date, y=hosp_per_100k)) +
  geom_line(color=covid_pal[11], size=1.2) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  facet_wrap(~tsa_name.x, ncol = 4) +
  theme_modern_rc() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Hospital Occupancy per 100k per day",
    subtitle = "Trauma Service Areas",
    y = "Hospital Occupancy per 100k", 
    caption = max(tsa_pad$date)
  )   +
  theme(
    strip.text = element_text(color = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size=8)
  )

img_name <- "tsa-hosp-per100k-wide.png"
ggsave(img_name, plot=hosp_per, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

hosp_ct <- tsa_pad %>% ggplot(aes(x=date, y=hosp_ct)) +
  geom_line(color=covid_pal[11], size = 1.2) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  facet_wrap(~tsa_name.x, ncol = 4, scales = "free_y") +
  theme_modern_rc() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Hospital Occupancy per Day due to COVID-19",
    subtitle = "Trauma Service Areas",
    y = "Hospital Occupancy",
    caption = max(tsa_pad$date)
  )  +
  theme(
    strip.text = element_text(color = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size=8)
  )

img_name <- "tsa-hosp-ct-wide.png"
ggsave(img_name, plot=hosp_ct, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

hosp_pct <- tsa_pad %>% ggplot(aes(x=date, y=hosp_cap)) +
  geom_line(color = covid_pal[11], size = 1.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~tsa_name.x, ncol = 4, ) +
  theme_modern_rc() +
  #ggthemes::theme_few() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "COVID-19 Hospital Bed Usage",
    subtitle = "Trauma Service Areas",
    y = "COVID-19 Beds as Percent of Total Available Beds",
    caption = max(tsa_pad$date)
  ) +
  theme(
    strip.text = element_text(color = "white"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

img_name <- "tsa-hosp-cap-wide.png"
ggsave(img_name, plot = hosp_pct, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)

stacked_hosp <- hosp_ct / hosp_per / hosp_pct

img_name <- "tsa_stacked_hosp.png"
ggsave(img_name, plot = stacked_hosp, width = 16, height = 27 , dpi=dpi, type = "cairo")

images <- c(images, img_name)


tsa_hosp <- tsa_hosp %>%
  mutate(curr_hosp_per_100k = hosp_ct / (population / 1e5)) %>%
  group_by(tsa_id) %>%
  mutate(
    cummax = cummax(curr_hosp_per_100k),
    is_highpoint = (cummax == curr_hosp_per_100k)
  ) %>%
  ungroup()

ordered_by_last_highpoint <- tsa_hosp %>%
  group_by(tsa_id) %>%
  filter(is_highpoint) %>% top_n(1, wt = date) %>%
  select(tsa_id, date, curr_hosp_per_100k, tsa_name.x) %>%
  arrange(desc(date), desc(curr_hosp_per_100k), tsa_id)

ranked_tsa_ridges <- tsa_hosp %>%
  mutate(
    tsa_id = factor(
      tsa_id,
      levels = ordered_by_last_highpoint$tsa_id,
      labels = ordered_by_last_highpoint$tsa_name.x
    )
  ) %>%
  ggplot(aes(
    x = date,
    y = tsa_id,
    height = curr_hosp_per_100k,
    group = tsa_id
  )) +
  geom_ridgeline(
    aes(fill = tsa_id),
    scale = 0.05,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(NULL, values = covid_pal) +
  theme_modern_rc() +
  labs(title = "Current hospital beds per 100k residents per TSA",
       caption = "Ordered by date of most recent peak, beds / 100k",
       y = "Trauma Service Area")   +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))

img_name <- "tsa_ranked_ridges.png"
ggsave(img_name, plot = ranked_tsa_ridges, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)


covidutil::gauth(email= "schneeman@gmail.com")
images %>% map(covidutil::upload_images)