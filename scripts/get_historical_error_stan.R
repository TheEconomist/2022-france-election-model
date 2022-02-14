# package setup:
# lapply(c('tidyverse','mvtnorm','janitor','lubridate','mgcv','boot','zoo','brms',install.packages)
# install.packages("remotes")
# remotes::install_github("stan-dev/cmdstanr")
# cmdstanr::install_cmdstan()


library(tidyverse)
library(mvtnorm)
library(janitor)
library(lubridate)
library(mgcv)
library(boot)
library(zoo)
library(brms)


# load data from wlezien jennings 2018
history = read_rds('raw-data/LONG_MI_NATURE_20180111.rds')

# filter to france, 200 days before r1 and r2
history = history %>% 
  filter(country == 'France') %>% 
  select(year = electionyr, round, poll_date = polldate, 
         election_date = elecdate, days_until_ed = daysbeforeED, 
         party = partyid, poll_pct = poll_,vote_pct = vote_) %>%
  filter(days_until_ed <= 200) %>%
  mutate(year = as.numeric(gsub('\\..*','',year))) %>%
  na.omit() %>%
  mutate(poll_pct = poll_pct / 100,
         vote_pct = vote_pct / 100)

history %>%
  filter(party == 1) %>%
  count(year)

# charts
history %>%
  ggplot(., aes(x=days_until_ed, y = abs(poll_pct - vote_pct))) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~round) +
  scale_x_reverse()

# dataset of errors 
history_errors = history %>%
  group_by(round) %>%
  group_by(year, days_until_ed, round) %>%
  summarise(error =  mean(abs(poll_pct - vote_pct))) %>%
  ungroup() %>%
  arrange(desc(days_until_ed)) %>%
  group_by(year) %>%
  mutate(error = rollapply(error, 7, FUN = mean, partial = T, align = 'right')) 

ggplot(history_errors, aes(x=days_until_ed, y = error, group = year, col = as.character(year))) + 
  geom_line() + 
  # geom_smooth() +
  facet_wrap(~round) +
  scale_x_reverse()


# fit functions to predict error for r1 and r2 on every day
r1_error_function = brm(error ~ s(days_until_ed),
                        sigma ~ s(days_until_ed),
                        data = history_errors %>% filter(round == 1),
                        family = 'student',
                        #family = 'gaussian',
                        iter = 20000, warmup = 500,
                        backend = 'cmdstan',
                        threads = threading(2),
                        prior = c(set_prior('normal(0, 1)', class = "b"), 
                                  set_prior('student_t(3, 0, 20)', class = "sigma")),
                        chains = 4, cores = 4,
                        control = list(adapt_delta = 0.9), refresh = 10
)


r1_draws = tibble(days_until_ed = 1:200) %>%
  posterior_predict(r1_error_function,.,ndraws = 19500 * 4) 

r1_error = tibble(days_until_ed = 1:200,
                  median = apply(r1_draws,2,median),
                  sd = apply(r1_draws,2,sd)) %>%
  mutate(upper = median + sd*2,
         lower = pmax(0, median -sd*2))

ggplot(r1_error, aes(x=days_until_ed)) +
  geom_line(aes(y=median)) +
  geom_line(aes(y=upper),linetype=2) +
  geom_line(aes(y=lower),linetype=2)

# now r2
r2_error_function = brm(error ~ s(days_until_ed),
                        sigma ~ s(days_until_ed),
                        data = history_errors %>% filter(round == 2),
                        family = 'student',
                        #family = 'gaussian',
                        iter = 20000, warmup = 500,
                        backend = 'cmdstan',
                        threads = threading(2),
                        prior = c(set_prior('normal(0, 1)', class = "b"), 
                                  set_prior('student_t(3, 0, 20)', class = "sigma")),
                        chains = 4, cores = 4,
                        control = list(adapt_delta = 0.9), refresh = 10
)


r2_draws = tibble(days_until_ed = 1:200) %>%
  posterior_predict(r2_error_function,.,ndraws = 19500 * 4) 

r2_error = tibble(days_until_ed = 1:200,
                  median = apply(r2_draws,2,median),
                  sd = apply(r2_draws,2,sd)) %>%
  mutate(upper = median + sd*2,
         lower = pmax(0, median -sd*2)) 

ggplot(r2_error, aes(x=days_until_ed)) +
  geom_line(aes(y=median)) +
  geom_line(aes(y=upper),linetype=2) +
  geom_line(aes(y=lower),linetype=2)



# output for reading in to main file --------------------------------------

# get rmse for borth r1 and r2
r1_error = apply(r1_draws,2,function(x){sqrt(mean(x^2))})

r2_error = apply(r2_draws,2,function(x){sqrt(mean(x^2))})


write_rds(r1_error,'output-data/historical_r1_error_over_time.rds')
write_rds(r2_error,'output-data/historical_r2_error_over_time.rds')


tibble(days_until_eleection = 1:200,
       r1_error = r1_error,
       r2_error = r2_error) %>%
  write_csv('output-data/historical_r1_r2_error.csv')

