
library(urbnmapr)
library(tidyverse)

assocs <- tribble(
  ~region, ~association,
  "california", "pacific",
  "washington", "pacific",
  "oregon", "pacific",
  "illinois", "great lakes", 
  "michigan", "great lakes",
  "ohio", "great lakes",
  "wisconsin", "great lakes",
  "minnesota", "great lakes",
  "indiana", "great lakes",
  "kentucky", "great lakes",
  "new york", "northeast",
  "new jersey", "northeast",
  "connecticut", "northeast",
  "pennsylvania", "northeast",
  "rhode island", "northeast",
  "delaware", "northeast",
  "massachusetts", "northeast",
  "maryland", "midatlantic",
  "virginia", "midatlantic",
  "district of columbia", "midatlantic",
  "vermont", "new new england",
  "new hampshire", "new new england",
  "maine", "new new england"
)

unique_assocs <- assocs %>% distinct(association) %>% arrange(association) %>% pull(association)

states <- urbnmapr::states %>% 
#  mutate(association = "unaffiliated") %>%
  mutate(state_name = tolower(state_name)) %>%
  left_join(assocs, by = c("state_name" = "region")) %>%
  mutate(association = if_else(is.na(association), "none", association)) %>%
  mutate(association = factor(association, levels = c(unique_assocs, "none")))



p <- ggplot(data = states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = association))

p + geom_polygon(color = "gray", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c(ipsum_pal()(length(unique_assocs)), "#dddddd")) +
  labs(
    title = "COVID-19 Regional Response Associations"
  )+
  theme_void() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

dpi <- 200
ggsave("states-covid-fark.png", width = 850/dpi, height = 679/dpi, dpi = dpi)
