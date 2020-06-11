library(rvest)
library(janitor)
library(readr)
library(readxl)
library(tidyr)
library(httr)

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

# summarize each tsa

tsa <- counties %>%
  group_by(tsa) %>%
  summarize(population = sum(population),
            area = sum(area)) %>%
  mutate(pop_density = population / area) %>%
  arrange(desc(pop_density)) %>%
  mutate(pop_density_ranking = row_number())

# OK now read in the TSA hospitalization. 

# a little risky b/c TX DSHS is a website, not a data repo: all of their data
# is oriented towards human access. The main URL is
# https://dshs.texas.gov/coronavirus/AdditionalData.aspx

dshs_hosp_url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19HospitalizationsOverTimebyTSA.xlsx"

GET(dshs_hosp_url, write_disk(tf <- tempfile(fileext = ".xlsx", tmpdir="./data")))
tsa_hosp_w <- read_excel(tf, skip = 2)
unlink(tf)


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
  left_join(tsa, by = c("tsa_id" = "tsa"))

