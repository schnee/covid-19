states_to_highlight <- scp100k %>% filter(date == min(date),
                                          ranking >=8,
                                          ranking <=12) 

scp100k %>%
  #filter(date >= first_case - days(2)) %>%
  filter(state %in% lhs$state) %>% 
  mutate(highlight = state %in% states_to_highlight$state) %>% 
  mutate(state = factor(state, levels = lhs$state)) %>%
  ggplot(aes(y=ranking, group = state)) + 
  geom_line(aes(x=date, color = highlight), size=1.1) + 
  scale_y_reverse() + 
  scale_color_manual(values = c("#333333", "white"))+
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
    subtitle = "Ordered by {Cases Per 100k, Population Density, State Name} by Day",
    y = NULL
  )

dpi <- 100
ggsave("cases-ranking-highlight.png", width = 850 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")


