# scp100k %>% group_by(state) %>% arrange(date) %>% 
#   mutate(delta_per_100k = cases_per_100k - lag(cases_per_100k)) %>%
#   mutate(delta_per_100k = if_else(is.na(delta_per_100k), 0, delta_per_100k)) %>%
#   mutate(cummax = cummax(delta_per_100k)) %>% ungroup() %>% 
#   select(state, date, cases_per_100k, delta_per_100k, cummax) %>% arrange(state, date) %>% View()


library(mcp)
library(tidybayes)
library(changepoint)
library(dplyr)
library(tidyr)
library(RcppRoll)

one_tsa <- tsa_hosp %>% 
  mutate(sequence = as.numeric(date)) %>%
  group_by(tsa_id) %>%
  arrange(date) %>%
  mutate(mean_7 = roll_mean(hosp_ct, 7, fill=0, align="right")) %>%
  ungroup() %>%
  filter(tsa_id == "O") 

fit_changepoint = cpt.meanvar(one_tsa$mean_7, method = "AMOC")
plot(fit_changepoint)
str(fit_changepoint)

cps <- cpts(fit_changepoint)

# maybe add the change point locations as priors, somehow?
model <- c(mean_7 ~ sequence, rep_len(c(1~1),1+length(cps))) %>% 
  as.list()

model

fit <- mcp(model, data = one_tsa)
plot(fit)
fit

cp_df <- fit$mcmc_post %>%
  tidy_draws() %>%
  summarize_all(mean) %>%
  select(starts_with("cp_")) %>%
  pivot_longer(cols = starts_with("cp_"), 
               names_to = "changepoint", 
               values_to = "sequence") %>%
  mutate(sequence = floor(sequence))

one_tsa <- one_tsa %>% left_join(cp_df) %>% fill(changepoint, .direction="up") %>% 
  replace_na(list(changepoint = "cp_n")) %>%
  group_by(changepoint) %>%
  mutate(hosp_ct_mean = mean(mean_7)) %>%
  ungroup() 

segment_tib <- one_tsa %>% 
  group_by(changepoint) %>% 
  summarize(xmin = min(date), xmax=max(date))%>%
  mutate(changepoint = factor(changepoint)) %>%
  mutate(shade = as.numeric(changepoint) %%2)


one_tsa %>%
  ggplot() +
  geom_rect(data = segment_tib, aes(xmin = xmin, xmax=xmax, fill = as.factor(shade)), ymin=0, ymax=Inf, color=NA) +
  scale_fill_manual(values = c("#eeeeee", "white")) +
  geom_line(aes(x=date, y=mean_7), color="black") +
  geom_step(aes(x=date, y=hosp_ct_mean), color="black", direction="mid", linetype=2) +
  labs(
    title = paste(min(one_tsa$tsa_id),min(one_tsa$tsa_name.y), sep="-"),
    y = "Hospitalization Count (7-day moving average)"
  ) + theme_few() +
  theme(
    legend.position="none"
  )

