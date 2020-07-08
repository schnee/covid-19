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
library(ggridges)
library(ggrepel)
devtools::load_all("./covidutil/")

# begin a list of images to upload to google
images <- list()

# yay, parse Wikipedia tables - that's sure to be maintenance free....

url <- "https://en.wikipedia.org/wiki/List_of_counties_in_Texas"
page <- read_html(url)

tables <- html_table(html_nodes(page, css ="table.wikitable.sortable"), fill=TRUE)

# on 2020 06 10, the wiki page consisted of only one table with that css
# selector, and that selector selected the counties table. Let's assert that to
# be safe
stopifnot(length(tables) == 1)

# and now convert the formerly readable friendly content to machine friendly
# content, and drop columns that we don't care about at this point in time

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

tsa_hosp_w <- tsa_hosp_w %>% filter(!is.na(`TSA ID`)) %>%
  filter(`TSA ID` != "Total") %>%
  mutate_at(vars(!starts_with("TSA")), as.numeric)

stopifnot(nrow(tsa_hosp_w) != 23)

# It is a wide data frame, we need to make it long. The Great State of Texas
# makes the column headers unfriendly, sort of. "Hosptialization 4/8" etc, so we
# need to remove the text, turn it into a string date and then convert to a
# Date. Also strip of the trailing period from the TSA ID. ANd just because
# we're ready to, join in the TSA demo data

tsa_hosp <- tsa_hosp_w %>%
  pivot_longer(cols = !starts_with("TSA"), 
               names_to="sequence",
               values_to="hosp_ct") %>% 
  clean_names() %>% mutate(date = ymd(sequence)) %>%
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
  complete(date, nesting(tsa_id, tsa_area, tsa_name), fill = list(hosp_ct = 0,
                                                                      hosp_cap = 0)) %>%
  select(date, tsa_id, tsa_area, tsa_name, hosp_ct, hosp_cap) %>%
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
  mutate(tsa_area = factor(tsa_area, levels = lhs$tsa_area)) %>%
  ggplot(aes(x=date, y=ranking)) + 
  geom_line(aes(color=tsa_area), size=1.1) +
  scale_y_reverse() +
  scale_color_viridis_d(option = "plasma", begin=0.4) +
  geom_text(data = lhs, aes(label=tsa_area), x= min(tsa_pad$date) - days(3),
            hjust = 1,
            size = 3.5) +
  geom_text(data = rhs, aes(label=tsa_area), x= max(tsa_pad$date) + days(1),
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
  geom_line(color=covid_pal[23], size=1.2) + 
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
  geom_line(color=covid_pal[23], size = 1.2) + 
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
  geom_line(color = covid_pal[23], size = 1.2) + 
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
    is_highpoint = (cummax == curr_hosp_per_100k),
    is_highest_high = factor(curr_hosp_per_100k == max(curr_hosp_per_100k))
  ) %>%
  ungroup()

ordered_by_last_highpoint <- tsa_hosp %>%
  group_by(tsa_id) %>%
  filter(is_highpoint) %>% top_n(1, wt = date) %>%
  select(tsa_id, date, curr_hosp_per_100k, tsa_area) %>%
  arrange(date, curr_hosp_per_100k, tsa_id)

highpoints <- tsa_hosp %>%
  filter(is_highest_high == TRUE) %>% 
  arrange(desc(date)) %>%
  distinct(tsa_id, .keep_all = TRUE) %>%
  mutate(
    tsa_id = factor(
      tsa_id,
      levels = ordered_by_last_highpoint$tsa_id,
      labels = ordered_by_last_highpoint$tsa_area
    ))

scale_factor = 0.1

ranked_tsa_ridges <- tsa_hosp %>%
  mutate(
    tsa_id = factor(
      tsa_id,
      levels = ordered_by_last_highpoint$tsa_id,
      labels = ordered_by_last_highpoint$tsa_area
    ),
    shade = as.factor(as.numeric(tsa_id) %% 2)
  ) %>% 
  ggplot(aes(
    x = date,
    y = tsa_id,
    height = curr_hosp_per_100k,
    group = tsa_id
  )) +
  geom_ridgeline(
    aes(fill = shade),
    colour = "#909090",
    scale = scale_factor,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  scale_fill_manual(NULL, values = c(covid_pal[22], covid_pal[23])) +
  geom_point(aes(y=as.numeric(tsa_id) + curr_hosp_per_100k * scale_factor, x=date, 
                 shape = is_highest_high, color = shade), 
             stroke = 1,
             show.legend = FALSE) +
  scale_color_manual(NULL, values = c(covid_pal[22], covid_pal[23])) +
  scale_shape_manual(NULL, values = c(NA,1)) + # no shape if not a high point
  geom_label_repel(data = highpoints, aes(x=date, y=as.numeric(tsa_id) + curr_hosp_per_100k * scale_factor,
                       label = paste0(round(curr_hosp_per_100k, digits = 0)," (",hosp_ct,")")), 
                   size = 4,
                  color = "black",
                  xlim = c(min(tsa_hosp$date), max(tsa_hosp$date) - days(5)),
                  segment.color = "white") +
  theme_modern_rc() +
  labs(title = "COVID-19 hospital beds occupied per 100k residents",
       caption = paste("Ordered by date of most recent peak, occupied beds / 100k",
       "Label X (Y): Beds per 100k (raw count) at highpoint",
       max(tsa_hosp$date),sep='\n'),
       y = "Trauma Service Area")   +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        axis.text.y = element_text(vjust = 0))
ranked_tsa_ridges
img_name <- "tsa_ranked_ridges.png"
ggsave(img_name, plot = ranked_tsa_ridges, width = 16, height = 9 , dpi=dpi, type = "cairo")

images <- c(images, img_name)


covidutil::gauth(email= "schneeman@gmail.com")
images %>% map(covidutil::upload_images)
