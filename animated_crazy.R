library(gganimate)

clj <- covid_longer_j %>%
  mutate(i_fmt = scales::comma_format()(infections),
         d_fmt = scales::comma_format()(deaths))

p <- clj %>% 
  ggplot() + 
  geom_col(aes(x=date, y=infections, group=seq_along(date)), width=1) +
  geom_area(aes(x=date, y=deaths*10), fill = "red", alpha = 0.75, position = position_nudge((x=0.5))) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "Casualties", 
                                          labels = scales::label_comma()), 
                      labels = scales::label_comma()) +
  # #scale_y_log10() +
  labs(
    title = "COVID-19 US Cases",
    subtitle = paste("Infections:", 
                     '{clj %>% filter(date == ymd(frame_along)) %>% pull(i_fmt)}',
                     "Casualties:", '{clj %>% filter(date == ymd(frame_along)) %>% pull(d_fmt)}'),
    y = "Infections",
    x = "Date",
    caption = paste0("Confirmed cases: https://github.com/CSSEGISandData/COVID-19\nLabels: media and tweets\n",
                     '{frame_along}')
  ) +
  geom_label_repel(data = events %>% filter(date <= max(clj$date)),
                   aes(x=date, y=infections, label = label),
                   alpha = 0.85,
                   arrow = NULL,
                   force = 10,
                   direction="both",
                   hjust = 1, size = 5,
                   segment.alpha= 0.5,
                   box.padding = 1,
                   xlim = c(min(clj$date), today() - days(20)),
                   ylim = c(100, max(clj$infections)),
                   show.legend = FALSE) +
  theme_ipsum(grid = FALSE) +
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 24)) +
  geom_rug(data = casualties %>% filter(ct < 3*max(clj$deaths)),
           aes(y=ct*10, color = what), sides="r", size = 2) +
  scale_color_few(palette = "Dark", "Reference\nCasualties") +
  NULL


anim <- p + transition_reveal(date) +
#  shadow_mark() +
#  enter_grow() +
#  enter_fade() + 
  NULL

animate(anim, nframes = nrow(clj), height = 679, width = 850,
        renderer = gifski_renderer("gganim.gif"), fps = 1.2)
  
