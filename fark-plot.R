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
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\n",
                     today())
  ) + 
  theme_ipsum(grid = FALSE) +
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red")) +
  geom_rug(data = casualties %>% filter(ct < 3*max(covid_longer_j$deaths)), 
           aes(y=ct*10, color = what), sides="r", size = 1) +
  scale_color_few(palette = "Dark", "Benchmark\nCasualties") +
  theme(legend.position = "bottom")

library(Cairo)

dpi <- 100
ggsave("covid-fark.png", width = 850 / dpi, height = 679/dpi , dpi=dpi, type = "cairo")



