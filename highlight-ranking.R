states_to_highlight <- scp100k %>% 
  filter(date >= ymd("2020-05-01")) %>%
  group_by(state) %>%
  mutate(        from_min = abs(min(ranking) - ranking),
                 from_max = abs(max(ranking) - ranking),
                 greatest_move = max(c(from_min, from_max)))%>% 
  ungroup() %>%
  filter(date == max(date)) %>%
  top_n(10, greatest_move) %>%
  select(state) %>% distinct()

centered_state <- lhs %>% 
  filter(state == "Florida") %>% 
  pull(ranking)

states_to_highlight <- 
  lhs %>% filter(ranking >= centered_state -2,
                 ranking <= centered_state +2) 

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
ggsave("cases-ranking-highlight.png", width = 1550 / dpi, height = 1000/dpi , dpi=dpi, type = "cairo")


