# package setup:
# lapply(c('tidyverse','mvtnorm','janitor','lubridate','mgcv','imputeTS','doParallel','foreach','boot','beepr','zoo','rvest','gridExtra','data.table','dtplyr'),install.packages)

start_time = Sys.time()

library(tidyverse)
library(janitor)
library(lubridate)

library(mvtnorm)
library(mgcv)
library(boot)
library(beepr)
library(zoo)

library(gridExtra)
library(data.table)
library(dtplyr)
library(lqmm)

# some variables that get called a lot
num_sims_r1 = 1000
num_sims_r2 = 100
max_knots = 100
conf_interval = 1.282 # 80% CI
conf_interval_upper_quantile = 0.9 # if upper = 0.9 and lower = 0.1, range = 80%
conf_interval_lower_quantile = 0.1

# run on a particular date? useful for looping...
RUN_DATE = Sys.Date()

#RUN_DATE = ymd('2022-01-25')
#RUN_DATES_LOOP = as.character(seq.Date(ymd('2022-01-27'), Sys.Date() ,'day'))
#for(RUN_DATE in RUN_DATES_LOOP){;RUN_DATE = ymd(RUN_DATE);
    
print(sprintf('Running model on %s with %s simulations',
              RUN_DATE,
              scales::number(num_sims_r1 * num_sims_r2,big.mark = ',') ))
       

# read in data ------------------------------------------------------------
print("Scraping and reading data...")
# call script to update polls
source("scripts/scrape_polls.R")

# read polls in
polls_r1 = read_csv('output-data/raw_polls_round_one_current.csv') %>%
  filter(end_date <= RUN_DATE) 
polls_r2 = read_csv('output-data/raw_polls_round_two_current.csv') %>%
  filter(end_date <= RUN_DATE) 

# filter to day of election
polls_r1 = polls_r1 %>% filter(end_date < RUN_DATE)
polls_r2 = polls_r2 %>% filter(end_date < RUN_DATE)


# get averages for round 1 ------------------------------------------------
print("Calculating round 1 and 2 averages...")
# trends_r1 = lapply(unique(polls_r1$cand), 
#                    function(c){
#                      # print(c)
#                      tmp = polls_r1 %>% filter(cand == c)
#                      
#                      # min k for this candidate/matchup
#                      min_k = length(unique(tmp$end_date));length(unique(tmp$pct))
#                      
#                      # fit a spline to the data and make predictions on every day
#                      mod = gam(pct ~ s(end_date.num, k = pmin(min_k, max_knots)), data = tmp, weights = weight)
#                      
#                      err = sqrt(mean(mod$residuals^2))
#                      
#                      tibble(cand = c,
#                             end_date = seq.Date(min(tmp$end_date),
#                                                 RUN_DATE,'day')) %>%
#                        mutate(end_date.num = as.numeric(end_date)) %>%
#                        mutate(trend = predict(mod, .),
#                               upper = trend + err * conf_interval,
#                               lower = trend - err * conf_interval) 
#                      
#                    }) %>% bind_rows


# optimize k for rmse of each party's vote share
# looping through values of k and adding predictions, calculating error
polls_r1_clone <- polls_r1
gam_knots_TOP_CANDS <- lapply(
  seq(2, max_knots),
  function(x) {
    # x = 5
    # print(x)
    suppressWarnings(
      rmse_in <- try((
        polls_r1_clone %>%
          group_by(cand) %>% 
          filter(median(pct) > 0.1) %>%
          nest() %>%
          mutate(model = data %>% 
                   map(~gam(pct ~ s(end_date.num, k=x), data = ., weights = weight))) %>% 
          mutate(trend = map2(model, data, predict)) %>% 
          unnest(c(trend, data)) %>%
          mutate(trend = case_when(trend > 1 ~ 1, 
                                   trend < 0 ~ 0,
                                   TRUE ~ trend),
                 error = trend - lead(pct)) %>%
          pull(error) %>% na.omit() %>% .^2 %>% mean() %>% sqrt()
      ),silent = TRUE)
    )
    
    if (!is.numeric(rmse_in)) {
      return(NULL)
    } else {
      # print(rmse_in)
      return(tibble(knots = x, rmse = rmse_in))
    }
  }
) %>%
  bind_rows() %>%
  na.omit()

gam_knots_TOP_CANDS
optimal_knots_r1_TOP_CANDS <- gam_knots_TOP_CANDS %>% filter(rmse <= quantile(rmse,0.25,na.rm=T)) %>% pull(knots) %>% mean %>% round
optimal_knots_r1_TOP_CANDS

# optimal knots for lower-polling candidates?
gam_knots_BOTTOM_CANDS <- lapply(
  seq(2, max_knots),
  function(x) {
    # x = 5
    # print(x)
    suppressWarnings(
      rmse_in <- try((
        polls_r1_clone %>%
          group_by(cand) %>% 
          filter(median(pct) <= 0.1) %>%
          nest() %>%
          mutate(model = data %>% 
                   map(~gam(pct ~ s(end_date.num, k=x), data = ., weights = weight))) %>% 
          mutate(trend = map2(model, data, predict)) %>% 
          unnest(c(trend, data)) %>%
          mutate(trend = case_when(trend > 1 ~ 1, 
                                   trend < 0 ~ 0,
                                   TRUE ~ trend),
                 error = trend - lead(pct)) %>%
          pull(error) %>% na.omit() %>% .^2 %>% mean() %>% sqrt()
      ),silent = TRUE)
    )
    
    if (!is.numeric(rmse_in)) {
      return(NULL)
    } else {
      # print(rmse_in)
      return(tibble(knots = x, rmse = rmse_in))
    }
  }
) %>%
  bind_rows() %>%
  na.omit()

gam_knots_BOTTOM_CANDS
optimal_knots_r1_BOTTOM_CANDS <- gam_knots_BOTTOM_CANDS %>% filter(rmse <= quantile(rmse,0.25,na.rm=T)) %>% pull(knots) %>% mean %>% round
optimal_knots_r1_BOTTOM_CANDS


# nested smoothing for each category
trends_r1 = 
  # get date x candidate pairs for every day
  expand_grid(cand = unique(polls_r1_clone$cand),
              end_date = seq.Date(min(polls_r1_clone$end_date),
                                  RUN_DATE,'day')) %>%
  left_join(polls_r1_clone,  by = c("cand", "end_date")) %>%
  mutate(end_date.num = as.numeric(end_date)) %>%
  # add predictions from spline
  group_by(cand) %>% 
  mutate(optimal_knots_this_cand = ifelse(mean(pct, na.rm=T) >= 0.1, 
                                          optimal_knots_r1_TOP_CANDS, 
                                          optimal_knots_r1_BOTTOM_CANDS)) %>%
  nest() %>%
  mutate(model = data %>% 
           map(~gam(pct ~ s(end_date.num, k=unique(.$optimal_knots_this_cand)), data = na.omit(.), weights = weight))) %>% 
  mutate(trend = map2(model, data, predict)) %>% 
  unnest(c(trend, data)) %>%
  select(-model)  %>%
  ungroup() 

# remove other candidate
trends_r1 = trends_r1 %>%
  filter(cand != 'other_cand_dummy')

err_r1_polls = trends_r1$trend - trends_r1$pct
err_r1_polls = sqrt(mean(err_r1_polls^2, na.rm=T))

# add mean-reversion and error inflation in trend based on days since last poll
trends_r1 = trends_r1 %>%
  left_join(trends_r1 %>%
              group_by(cand) %>%
              filter(!is.na(polling_firm)) %>%
              summarise(date_of_last_poll = max(end_date)),
            by = 'cand') %>%
  mutate(days_since_last_poll = pmax(0, end_date - date_of_last_poll)) %>%
  group_by(cand) %>%
  mutate(ma = zoo::rollapply(data = pct, width = 100, 
                             FUN = function(x){
                               return(mean(tail(na.omit(x), 5)))
                             }, partial = T,align='right'),
         trend = 
           (trend * exp(days_since_last_poll*-0.03)) + 
           (ma  * (1  - exp(days_since_last_poll*-0.03)) ),
           trend_se = ifelse(days_since_last_poll > 0,
                             err_r1_polls + (err_r1_polls * ((days_since_last_poll^(1/20) ) - 1)  ),
                             err_r1_polls) ) %>%
  ungroup() 

trends_r1[trends_r1$cand == 'melenchon',] %>%
  ggplot(.) +
  geom_point(aes(x = end_date, y = pct)) +
  geom_line(aes(x = end_date, y = ma, col = 'moving avg')) +
  geom_line(aes(x = end_date, y = trend, col = 'trend')) 

# do corrections and calculate upper/lower
trends_r1 = trends_r1 %>%
  # if any days have multiple entries, remove them
  group_by(cand, end_date, end_date.num) %>%
  summarise(trend = median(trend,na.rm=T),
            trend_se = median(trend_se, na.rm=T)) %>%
  ungroup() %>%
  # distinct, and calc
  distinct() %>%
  mutate(upper = trend + trend_se * conf_interval,
         lower = trend - trend_se * conf_interval) %>%
  mutate_at(c('trend','upper','lower'),
            function(dat){
              case_when(dat < 0 ~ 0,
                        dat > 1 ~ 1,
                        TRUE ~ dat)
            })  


# pull averages for sims
avgs_r1 = trends_r1 %>%
  group_by(cand) %>%
  filter(end_date == max(end_date)) %>%
  ungroup() %>%
  arrange(desc(trend)) %>%
  mutate(trend = trend / sum(trend))

avgs_r1

gg1 = trends_r1 %>%
  mutate(cand_label = ifelse(cand %in% avgs_r1$cand[1:5], cand, 'other')) %>%
  ggplot(., aes(x=end_date, y=trend, group=cand,col=cand_label)) + 
  geom_point(data = polls_r1 %>%
               mutate(cand_label = ifelse(cand %in% avgs_r1$cand[1:5], cand, 'other')),
             aes(y=pct),shape=1) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=cand_label),col=NA,alpha=0.2) +
  theme_minimal() +
  theme(legend.position = 'top') +
  scale_color_manual(values = c('macron' = '#F1C40F',
                                'le_pen' = '#2980B9',
                                'pecresse' = '#85C1E9',
                                'zemmour' = '#99A3A4',
                                'melenchon' = '#EC7063',
                                'other' = '#E5E7E9')) +
  scale_fill_manual(values = c('macron' = '#F1C40F',
                                'le_pen' = '#2980B9',
                                'pecresse' = '#85C1E9',
                                'zemmour' = '#99A3A4',
                               'melenchon' = '#EC7063',
                                'other' = '#E5E7E9'))



# get averages for round 2 ------------------------------------------------
# optimize k for rmse of each party's vote share
# looping through values of k and adding predictions, calculating error
polls_r2_clone <- polls_r2
gam_knots_r2 <- lapply(
  seq(2, max_knots),
  function(x) {
    # x = 5
    # print(x)
    suppressWarnings(
      rmse_in <- try((
        polls_r2_clone %>%
          group_by(matchup) %>% 
          filter(n() >= 10) %>%
          nest() %>%
          mutate(model = data %>% 
                   map(~gam(cand1_pct ~ s(end_date.num, k=x), data = ., weights = weight))) %>% 
          mutate(trend = map2(model, data, predict)) %>% 
          unnest(c(trend, data)) %>%
          mutate(trend = case_when(trend > 1 ~ 1, 
                                   trend < 0 ~ 0,
                                   TRUE ~ trend),
                 error = trend - lead(cand1_pct)) %>%
          pull(error) %>% na.omit() %>% .^2 %>% mean() %>% sqrt()
      ),silent = TRUE)
    )
    
    
    if (!is.numeric(rmse_in)) {
      return(NULL)
    } else {
      # print(rmse_in)
      return(tibble(knots = x, rmse = rmse_in))
    }
  }
) %>%
  bind_rows() %>%
  na.omit()

gam_knots_r2
optimal_knots_r2 <- gam_knots_r2 %>% filter(rmse <= quantile(rmse,0.25,na.rm=T)) %>% pull(knots) %>% mean %>% round
optimal_knots_r2

# nested smoothing for each category
trends_r2 = 
  # get date x matchupidate pairs for every day
  polls_r2_clone %>% 
  select(matchup,cand1,cand2) %>%
  group_by(matchup,cand1,cand2) %>%
  expand_grid(end_date = seq.Date(min(polls_r2_clone$end_date),
                                  RUN_DATE,'day')) %>%
  distinct() %>%
  left_join(polls_r2_clone,  by = c('matchup','cand1','cand2','end_date')) %>%
  mutate(end_date.num = as.numeric(end_date)) 

# IF MORE THAN 10 POLLS,
# add predictions from spline
trends_r2 = polls_r2_clone %>% 
  # wrangling
  group_by(matchup) %>% 
  filter(n() >= 10) %>%
  select(matchup,cand1,cand2) %>%
  group_by(matchup,cand1,cand2) %>%
  expand_grid(end_date = seq.Date(min(polls_r2_clone$end_date),
                                  RUN_DATE,'day')) %>%
  distinct() %>%
  left_join(polls_r2_clone,  by = c('matchup','cand1','cand2','end_date')) %>%
  mutate(end_date.num = as.numeric(end_date)) %>%
  # predict
  group_by(matchup) %>% 
  nest() %>%
  mutate(model = data %>% 
           map(~gam(cand1_pct ~ s(end_date.num, k=optimal_knots_r2), data = na.omit(.), weights = weight))) %>% 
  mutate(cand1_trend = map2(model, data, predict)) %>% 
  unnest(c(cand1_trend, data)) %>%
  select(-model)  %>%
  ungroup() %>%
  # IF < 10 & > 3, KALMAN
  bind_rows(
    polls_r2_clone %>% 
      group_by(matchup) %>% 
      filter(n() < 10 & n() >= 3) %>%
      select(matchup,cand1,cand2) %>%
      group_by(matchup,cand1,cand2) %>%
      expand_grid(end_date = seq.Date(min(polls_r2_clone$end_date),
                                      RUN_DATE,'day')) %>%
      distinct() %>%
      left_join(polls_r2_clone,  by = c('matchup','cand1','cand2','end_date')) %>%
      mutate(end_date.num = as.numeric(end_date)) %>%
      # predict
      group_by(matchup) %>% 
      mutate(cand1_trend = imputeTS::na_kalman(cand1_pct))
  ) %>%
  # ELSE, SIMPLE AVERAGEE
  bind_rows(
    polls_r2_clone %>% 
      group_by(matchup) %>% 
      filter(n() < 3) %>%
      select(matchup,cand1,cand2) %>%
      group_by(matchup,cand1,cand2) %>%
      expand_grid(end_date = seq.Date(min(polls_r2_clone$end_date),
                                      RUN_DATE,'day')) %>%
      distinct() %>%
      left_join(polls_r2_clone,  by = c('matchup','cand1','cand2','end_date')) %>%
      mutate(end_date.num = as.numeric(end_date)) %>%
      # predict
      group_by(matchup) %>% 
      mutate(cand1_trend = mean(cand1_pct, na.rm=T))
  ) %>%
  
  # clean it all up
  arrange(matchup,end_date)
  
# if matchup had not been polled yet, avg should be set to NA
trends_r2 = trends_r2 %>%
  left_join(
    polls_r2 %>% group_by(matchup) %>% summarise(first_poll = min(end_date))
  ) %>%
  filter(end_date >= first_poll) %>%
  select(-first_poll)

# calculate RMSE so we can draw MOE ribbons
err_r2_polls = trends_r2$cand1_trend - trends_r2$cand1_pct
err_r2_polls = sqrt(mean(err_r2_polls^2, na.rm=T))

# add mean-reversion and error inflation in trend based on days since last poll
trends_r2 = trends_r2 %>%
  left_join(trends_r2 %>%
              group_by(matchup) %>%
              filter(!is.na(polling_firm)) %>%
              summarise(date_of_last_poll = max(end_date)),
            by = 'matchup') %>%
  mutate(days_since_last_poll = pmax(0, end_date - date_of_last_poll)) %>%
  group_by(matchup) %>%
  mutate(cand1_ma = zoo::rollapply(data = cand1_pct, width = 300, 
                             FUN = function(x){
                               return(mean(tail(na.omit(x), 5)))
                             }, partial = T,align='right'),
         cand1_trend = 
           (cand1_trend * exp(days_since_last_poll*-0.05)) + 
           (cand1_ma  * (1  - exp(days_since_last_poll*-0.05)) ),
         trend_se = ifelse(days_since_last_poll > 0,
                           err_r2_polls + (err_r2_polls * ((days_since_last_poll^(1/10) ) - 1)  ),
                           err_r2_polls) ) %>%
  ungroup() 



# do corrections and calculate upper/lower
trends_r2 = trends_r2 %>%
  # if any dupes, average out
  group_by(matchup, cand1, cand2, end_date, end_date.num) %>%
  summarise(cand1_trend = median(cand1_trend),
            trend_se = median(trend_se)) %>%
  ungroup() %>%
  # mutations
  distinct() %>%
  mutate(cand1_upper = cand1_trend + trend_se * conf_interval,
         cand1_lower = cand1_trend - trend_se * conf_interval,
         
         cand2_trend = 1 - cand1_trend,
         cand2_upper = 1 - cand1_lower,
         cand2_lower = 1 - cand1_upper) %>%
  mutate_at(c('cand1_trend','cand1_upper','cand1_lower',
              'cand2_trend','cand2_upper','cand2_lower'),
            function(dat){
              case_when(dat < 0 ~ 0,
                        dat > 1 ~ 1,
                        TRUE ~ dat)
            }) 

# plot for EDA
gg2 = ggplot(trends_r2, aes(x=end_date)) + 
  geom_point(data = polls_r2, aes(y=cand1_pct,col=cand1),shape=1) +
  geom_point(data = polls_r2, aes(y=cand2_pct,col=cand2),shape=1) +
  geom_line(aes(y=cand1_trend,col=cand1), size=1) +
  geom_line(aes(y=cand2_trend,col=cand2), size=1) +
  geom_ribbon(aes(ymin = cand1_lower, ymax = cand1_upper,fill=cand1),col=NA,alpha=0.2) +
  geom_ribbon(aes(ymin = cand2_lower, ymax = cand2_upper,fill=cand2),col=NA,alpha=0.2) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~matchup) +
  scale_color_manual(values = c('macron' = '#F1C40F',
                                'le_pen' = '#2980B9',
                                'pecresse' = '#85C1E9',
                                'zemmour' = '#99A3A4',
                                'melenchon' = '#EC7063',
                                'other' = '#E5E7E9')) +
  scale_fill_manual(values = c('macron' = '#F1C40F',
                               'le_pen' = '#2980B9',
                               'pecresse' = '#85C1E9',
                               'zemmour' = '#99A3A4',
                               'melenchon' = '#EC7063',
                               'other' = '#E5E7E9'))

grid.arrange(gg1, gg2, heights = c(6, 5)) # plot for observation while testing

gg2 + 
  coord_cartesian(xlim=c(RUN_DATE - 60, RUN_DATE+2))

# extract final values for simulations
avgs_r2 = trends_r2 %>%
  group_by(matchup) %>%
  filter(end_date == max(end_date)) %>%
  ungroup() %>%
  arrange(desc(cand1_trend))

# create dataset containing pairs of every candidate
pairs = t(combn(avgs_r1$cand,2)) %>% 
  as.data.frame %>%
  set_names(., c('cand1','cand2')) %>%
  as_tibble

# create matchup indicator. if already polled, use that one. or else we default by r1 polling
pairs = pairs %>%
  mutate(matchup = case_when(
    sprintf('%sVV%s',cand1,cand2) %in% polls_r2$matchup ~ sprintf('%sVV%s',cand1,cand2),
    sprintf('%sVV%s',cand2,cand1) %in% polls_r2$matchup ~ sprintf('%sVV%s',cand2,cand1),
    TRUE ~ sprintf('%sVV%s',cand1,cand2)
    )
  ) %>%
  left_join(avgs_r2 %>%
              dplyr::select(matchup,cand1_trend, cand2_trend),
            by = "matchup") %>%
  select(-c(cand1,cand2)) %>%
  mutate(matchup2 = matchup) %>%
  separate(matchup2, into=c('cand1','cand2'),sep='VV')

# for all other matchups with no polling, 50-50, and add indicator
pairs = pairs %>%
  mutate(is_matchup_polled = ifelse(is.na(cand1_trend),F,T))
pairs[is.na(pairs)] = 0.5
pairs = pairs %>%
  arrange(!is_matchup_polled)

# for important matchups where we have a prior from sophie...
# ONLY IF we don't have polling already
# Pécresse - Mélenchon  60;40
pairs[pairs$matchup == 'pecresseVVmelenchon',]$cand1_trend = 0.60
pairs[pairs$matchup == 'pecresseVVmelenchon',]$cand2_trend = 0.40

# macron x unpolled (in that order)
pairs[!pairs$is_matchup_polled & pairs$cand1 == 'macron',]$cand1_trend = 0.55
pairs[!pairs$is_matchup_polled & pairs$cand1 == 'macron',]$cand2_trend = 0.45

# unpolled x macron (in that order)
pairs[!pairs$is_matchup_polled & pairs$cand2 == 'macron',]$cand1_trend = 0.45
pairs[!pairs$is_matchup_polled & pairs$cand2 == 'macron',]$cand2_trend = 0.55

# just make sure everything everywhere adds up to 100
pairs = pairs %>%
  mutate(tot = cand1_trend + cand2_trend,
         cand1_trend = cand1_trend / tot,
         cand2_trend = cand2_trend / tot) %>%
  dplyr::select(-tot)


# get historical error curves ---------------------------------------------
# a separate file trains the models
# we are just going to read in what we need
r1_error = read_rds('output-data/historical_r1_error_over_time.rds')
r2_error = read_rds('output-data/historical_r2_error_over_time.rds')


tibble(days_until_ed = 1:200,
       r1_error = r1_error,
       r2_error = r2_error) %>%
  ggplot(., aes(x=days_until_ed)) +
  geom_line(aes(y=r1_error,col='Round 1 error')) +
  geom_line(aes(y=r2_error,col='Round 2 error')) 

# extract for today
r1_error_dted = r1_error[ymd('2022-04-10') - RUN_DATE]
r2_error_dted = r1_error[ymd('2022-04-24') - RUN_DATE]

# simulate round 1 --------------------------------------------------------
print("Simulating round 1...")
# extract correlation in the polls
cand_cor_r1 = trends_r1 %>%
  dplyr::select(end_date, cand, trend) %>%
  spread(cand, trend) %>%
  dplyr::select(-end_date) %>%
  cor

cand_cor_r1 %>%
  as_tibble() %>%
  mutate(cand1 = rownames(cand_cor_r1)) %>%
  gather(cand2, cor, 1:(ncol(.)-1)) %>%
  filter(cand1 %in% avgs_r1$cand[1:8],
         cand2 %in% avgs_r1$cand[1:8]) %>%
  ggplot(aes(x=cand1,y=cand2,col=cor,fill=cor)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_color_viridis_c()

# max cor = 0.5
cand_cor_r1 = cand_cor_r1 * 0.75
diag(cand_cor_r1) = 1

cand_cor_r1 = lqmm::make.positive.definite(cand_cor_r1)

# # normal mv t:
r1_sims = mvtnorm::rmvt(
  n = num_sims_r1, df = 4, sigma = cand_cor_r1,
) * sqrt(r1_error_dted^2 + err_r1_polls^2) 


r1_sims.mat = as.matrix(r1_sims)

colnames(r1_sims.mat) = colnames(cand_cor_r1)


# add error to trend to generate vote for each trial
r1_sims = r1_sims.mat %>%
  as_tibble() %>%
  mutate(trial = row_number()) %>%
  gather(cand, error, 1:(ncol(.)-1)) %>%
  left_join(avgs_r1 %>%
              dplyr::select(cand, trend)) %>%
  mutate(
    # first try is to do this
    vote = trend + error,
    vote = pmin(1, vote),
    vote = pmax(0, vote),
    
    # or, logit conversions
    #trend = logit(trend),
    #vote = trend + error*4, # a gelman thing?
    #trend = inv.logit(trend),
    #vote = inv.logit(vote)
    
    # add beta-distributed noise for anyone at 0, so we avoid any zeros....
    vote = ifelse(vote == 0, vote + rbeta(n(),1,100),vote),
  ) %>%
  # sum to 1
  group_by(trial) %>%
  mutate(trend = trend / sum(trend)) %>%
  ungroup()

# look at sims
r1_sims %>%
  filter(cand %in% avgs_r1$cand[1:10]) %>%
  group_by(cand) %>%
  #summarise(sd_vote = sd(vote))
  ggplot(.,aes(x=vote,fill = cand, col = cand)) + 
  geom_vline(data = avgs_r1 %>% filter(cand %in% avgs_r1$cand[1:10]),
             aes(xintercept=trend, col = cand)) +
  geom_density(alpha = 0.5) +
  #geom_histogram(binwidth=0.02, alpha=0.5, position = "identity") +
  coord_cartesian(xlim=c(0,0.5))

r1_sims %>%
  group_by(trial) %>%
  arrange(trial,desc(vote)) %>%
  mutate(make_r2 = row_number() <= 2) %>%
  group_by(cand) %>%
  summarise(prob_make_r2 = mean(make_r2))  %>%
  arrange(desc(prob_make_r2))

r1_sims %>% 
  group_by(trial) %>%
  arrange(trial,desc(vote)) %>%
  filter(row_number() <= 2) %>%
  # add matchup indicator
  mutate(matchup = sprintf('%sVV%s',cand[1],cand[2]),
         matchup = ifelse(!matchup %in% pairs$matchup,
                          sprintf('%sVV%s',cand[2],cand[1]),
                          matchup)) %>%
  ungroup() %>%
  count(matchup) %>%
  mutate(prob_matchup = n/sum(n)) %>%
  arrange(desc(prob_matchup))


# round 1 predictions with uncertainty
r1_sims %>% 
  group_by(cand) %>%
  summarise(median = median(vote),
            upper = quantile(vote, conf_interval_upper_quantile),
            lower = quantile(vote, conf_interval_lower_quantile)) %>%
  arrange(desc(median))


# simulate round 2, nested-------------------------------------------------
print("Simulating round 2...")

# get a data frame of matchups from the r1 results
r2_sims = r1_sims %>% 
  group_by(trial) %>%
  arrange(trial,desc(vote)) %>%
  filter(row_number() <= 2) %>%
  # add matchup indicator
  mutate(matchup = sprintf('%sVV%s',cand[1],cand[2]),
         matchup = ifelse(!matchup %in% pairs$matchup,
                          sprintf('%sVV%s',cand[2],cand[1]),
                          matchup)) %>%
  # only get one observation per trial
  dplyr::select(r1_trial = trial,
         matchup = matchup) %>%
  distinct() %>%
  # join the pairs data, so we have the trends and can add sims
  left_join(pairs, by = "matchup") 

# join the vote shares to a data frame of r1 and r2 sim numbers
r2_sims = tibble(r1_trial = rep(1:num_sims_r1,num_sims_r2)) %>%
  arrange(r1_trial) %>%
  mutate(r2_trial = rep(1:num_sims_r2,num_sims_r1)) %>%
  left_join(r2_sims, by = "r1_trial")

# generate num_sims_r1 * num_sims_r2 errors and add to cand1 trend
r2_sims$cand1_error = rt(
  n = num_sims_r2 * num_sims_r1, df = 4
) 

# multiply by error, including 1.2x as much for non-polled matchups
r2_sims$cand1_error = r2_sims$cand1_error * sqrt(r2_error_dted^2 + err_r2_polls^2) # r2_error_dted

r2_sims = r2_sims %>%
  mutate(cand1_error = ifelse(is_matchup_polled, cand1_error, cand1_error* 1.2))

# negate for other candidate
r2_sims$cand2_error = r2_sims$cand1_error * -1

# generate vote by adding trend and error...
r2_sims = r2_sims %>%
  mutate(
    # forcing
    cand1_vote = cand1_trend + cand1_error,
    cand1_vote = pmin(1, cand1_vote),
    cand1_vote = pmax(0, cand1_vote),
    
    cand2_vote = cand2_trend + cand2_error,
    cand2_vote = pmin(1, cand2_vote),
    cand2_vote = pmax(0, cand2_vote),
    
    # logit space
    # or, logit conversions
    #trend = logit(trend),
    #vote = trend + error*4, # a gelman thing?
    #trend = inv.logit(trend),
    #vote = inv.logit(vote),
  )

r2_sims %>% filter(matchup == 'pecresseVVle_pen') %>%
  select(r1_trial,r2_trial,cand1,cand2,cand1_vote,cand2_vote) %>% 
  summarise(mean(cand1_vote > cand2_vote))



# ... then convert to long format
r2_sims = bind_rows(
  # cand1
  r2_sims %>% 
    dplyr::select(r1_trial, r2_trial, matchup,
           match_cand1 = cand1,
           match_cand2 = cand2,
           cand = cand1,
           trend = cand1_trend,
           error = cand1_error,
           vote = cand1_vote,
    ) %>% distinct(),
  # cand2
  r2_sims %>% 
    dplyr::select(r1_trial, r2_trial, matchup,
           match_cand1 = cand1,
           match_cand2 = cand2,
           cand = cand2,
           trend = cand2_trend,
           error = cand2_error,
           vote = cand2_vote,
    ) %>% distinct()
)

r2_sims

r2_sims %>% filter(matchup == 'pecresseVVle_pen') %>%
  select(r1_trial,r2_trial,cand,vote) %>% 
  spread(cand,vote) %>%
  summarise(mean(pecresse > le_pen))



# beepr::beep(2)

# convert to a "lazy" data table to track operations before performing them. 
# this dramatically speeds things up.
# wrangling
r2_sims = r2_sims %>%  # take lazy df
  lazy_dt() %>%
  arrange(r1_trial,r2_trial,desc(vote)) %>% # track operation, then do it
  as_tibble()


# just look at win percentages
r2_sims %>%
  lazy_dt() %>%
  group_by(r1_trial, r2_trial) %>%
  filter(vote == max(vote)) %>%
  ungroup() %>%
  count(cand) %>%
  arrange(desc(n)) %>%
  mutate(pct = n / sum(n)) %>%
  as_tibble()


# top matchups are... ?
top_matchups = r2_sims %>%
  lazy_dt() %>%
  group_by(r1_trial, r2_trial) %>%
  mutate(winner = cand[1]) %>%
  ungroup() %>%
  count(matchup, name = 'prob_matchup') %>%
  mutate(prob_matchup = prob_matchup / sum(prob_matchup)) %>%
  arrange(desc(prob_matchup)) %>%
  filter(row_number() <= 6) %>%
  as_tibble()

top_matchups 


# write a bunch of output -------------------------------------------------
print("Writing output...")

# chart 1: round 1 polls so far, and trends so far
polls_r1 %>%
  write_csv(., 'output-data/site-sync/round_one_polls.csv')

trends_r1 %>%
  mutate_at(c(3:6), as.numeric) %>% # getting weird error without this
  write_csv(., 'output-data/site-sync/round_one_trends.csv')

# chart 2: round 1 predictions with uncertainty
r1_sims %>% 
  group_by(cand) %>%
  summarise(median = median(vote),
            upper = quantile(vote, conf_interval_upper_quantile),
            lower = quantile(vote, conf_interval_lower_quantile)) %>%
  arrange(desc(median)) %>%
  write_csv(., 'output-data/site-sync/round_one_predictions_and_uncertainty.csv')

r1_sims %>% 
  group_by(cand,
           vote = round(vote/0.005)*0.005*100) %>%
  summarise(prob = n()) %>%
  mutate(prob = prob / sum(prob)) %>%
  spread(vote, prob) %>%
  write_csv('output-data/site-sync/round_one_predictions_fuzzy_bars.csv')

# chart 3: round 2 polls so far, and trends so far
polls_r2 %>%
  write_csv(., 'output-data/site-sync/round_two_polls.csv')

trends_r2 %>%
  mutate_at(c(5:11), as.numeric) %>% # getting weird error without this
  write_csv(., 'output-data/site-sync/round_two_trends.csv')

# chart 4: round 2 predictions with uncertainty
r2_sims %>% 
  group_by(matchup) %>%
  mutate(matchup_prob = n() / nrow(r2_sims)) %>%
  group_by(matchup,cand) %>%
  summarise(matchup_prob = unique(matchup_prob),
            median = median(vote),
            upper = quantile(vote, conf_interval_upper_quantile),
            lower = quantile(vote, conf_interval_lower_quantile)) %>%
  arrange(desc(matchup_prob)) %>%
  write_csv(., 'output-data/site-sync/round_two_predictions_and_uncertainty.csv')


# make the cone chart
round_two_trends_and_projection_cone = trends_r2 %>%
  bind_rows(
    r2_sims %>% 
      group_by(matchup,cand) %>%
      summarise(
        trend = median(vote),
        upper = quantile(vote, conf_interval_upper_quantile),
        lower = quantile(vote, conf_interval_lower_quantile)
      ) %>%
      group_by(matchup) %>%
      arrange(matchup,desc(trend)) %>%
      mutate(cand1 = cand[1],
             cand2 = cand[2],
             end_date = ymd('2022-04-24'),
             end_date.num = as.numeric(end_date),
             cand1_trend = trend[1],
             cand1_upper = upper[1],
             cand1_lower = lower[1],
             cand2_trend = trend[2],
             cand2_upper = upper[2],
             cand2_lower = lower[2]) %>%
      select(matchup,cand1,cand2,end_date,end_date.num,
             cand1_trend,cand1_upper,cand1_lower,
             cand2_trend,cand2_upper,cand2_lower)
  ) 

round_two_trends_and_projection_cone %>%
  filter(matchup == 'macronVVpecresse') %>%
  ggplot(.,aes(x=end_date)) +
  geom_point(data = polls_r2 %>% filter(matchup == 'macronVVpecresse'),
             aes(y=cand1_pct,col=cand1),shape=1) +
  geom_point(data = polls_r2 %>% filter(matchup == 'macronVVpecresse'),
             aes(y=cand2_pct,col=cand2),shape=1) +
  geom_line(aes(y=cand1_trend,col=cand1), size=1) +
  geom_line(aes(y=cand2_trend,col=cand2), size=1) +
  geom_ribbon(aes(ymin = cand1_lower, ymax = cand1_upper,fill=cand1),col=NA,alpha=0.2) +
  geom_ribbon(aes(ymin = cand2_lower, ymax = cand2_upper,fill=cand2),col=NA,alpha=0.2) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~matchup) +
  scale_color_manual(values = c('macron' = '#F1C40F',
                                'le_pen' = '#2980B9',
                                'pecresse' = '#85C1E9',
                                'zemmour' = '#99A3A4',
                                'melenchon' = '#EC7063',
                                'other' = '#E5E7E9')) +
  scale_fill_manual(values = c('macron' = '#F1C40F',
                               'le_pen' = '#2980B9',
                               'pecresse' = '#85C1E9',
                               'zemmour' = '#99A3A4',
                               'melenchon' = '#EC7063',
                               'other' = '#E5E7E9'))


round_two_trends_and_projection_cone %>%
  write_csv(.,'output-data/site-sync/round_two_trends_and_projection_cone.csv')


# chart 5: sankey (w/ all associated probs)
probmake_round_2 = r1_sims %>%
  lazy_dt() %>%
  group_by(trial) %>%
  arrange(trial, desc(vote)) %>%
  mutate(position = row_number()) %>%
  group_by(cand) %>%
  summarise(prob_make_r2 = mean(position <= 2)) %>%
  arrange(desc(prob_make_r2)) %>%
  as_tibble() 
write_csv(probmake_round_2, 'output-data/site-sync/sankey_part1_prob_make_round_2.csv')


prob_each_matchup = r2_sims %>%
  lazy_dt() %>%
  group_by(r1_trial, r2_trial) %>%
  mutate(winner = cand[1]) %>%
  ungroup() %>% # doesn't work in data.table ??
  dplyr::select(cand, matchup, winner, match_cand1, match_cand2)  %>% 
  count(matchup, match_cand1, match_cand2, name = 'prob_matchup') %>%
  ungroup() %>%
  mutate(prob_matchup = prob_matchup / sum(prob_matchup)) %>%
  arrange(desc(prob_matchup)) %>%
  as_tibble()
write_csv(prob_each_matchup, 'output-data/site-sync/sankey_part2_prob_of_each_matchup.csv')


prob_winning_each_cand_matchup = r2_sims %>%  # conditional probs
  lazy_dt() %>%
  group_by(r1_trial, r2_trial) %>%
  mutate(winner = cand[1]) %>%
  ungroup() %>%
  dplyr::select(cand, matchup, winner)  %>%
  group_by(matchup, cand) %>%
  summarise(prob_winning_this_matchup = mean(cand == winner)) %>%
  as_tibble()
write_csv(prob_winning_each_cand_matchup, 'output-data/site-sync/sankey_part3_cand_prob_winning_each_matchup.csv')


prob_winning_each_cand_matchup %>%
  filter(matchup %in% top_matchups$matchup) %>%
  left_join(top_matchups)  %>%
  arrange(desc(prob_matchup))

# table(?): overall probability of winning
sankey = r2_sims %>% 
  lazy_dt() %>%
  group_by(r1_trial, r2_trial) %>%
  mutate(winner = cand[1]) %>%
  ungroup() %>%
  dplyr::select(cand, matchup, winner) %>%
  as_tibble() 

# prob of making and winning round two
prob_first_second_round_1 = r1_sims %>%
  lazy_dt() %>%
  group_by(trial) %>%
  arrange(trial, desc(vote)) %>%
  mutate(position = row_number()) %>%
  group_by(cand) %>%
  summarise(prob_first_place_r1 = mean(position == 1),
            prob_second_place_r1 = mean(position == 2))  %>%
  arrange(desc(prob_first_place_r1)) %>%
  as_tibble()


overall_probs = sankey %>% 
  lazy_dt() %>%
  count(cand, name = 'prob_making_r2') %>%
  ungroup() %>%
  mutate(prob_making_r2 = (prob_making_r2 / sum(prob_making_r2))*2) %>%
  arrange(desc(prob_making_r2)) %>%
  left_join(
    sankey %>% 
      count(cand = winner, name = 'prob_winning_r2') %>%
      ungroup() %>%
      mutate(prob_winning_r2 = prob_winning_r2 / sum(prob_winning_r2)) %>%
      arrange(desc(prob_winning_r2))
  ) %>% 
  arrange(desc(prob_winning_r2)) %>%
  left_join(prob_first_second_round_1) %>%
  #filter(prob_winning_r2 >= 0.001) %>%
  mutate_at(c(2:5),
            function(x){round(x,4)}) %>% 
  as_tibble()
write_csv(overall_probs, 'output-data/site-sync/overall-prob-winning-r1-r2.csv')

# raw simulations for experimental vis
# just r1 first...

# ... add r2 now:
# this bind takes quite some time
# so just output 1000 trials, randomly selected
sims_for_left_join = r1_sims %>%
  filter(trial %in% round(runif(1000, 1, max(r1_sims$trial))) ) %>%
  dplyr::select(r1_trial = trial,
         cand,
         vote) %>% 
  spread(cand, vote) %>%
  as.data.table()

sims_for_right_join = r2_sims %>%
  lazy_dt() %>%
  filter(r1_trial %in% sims_for_left_join$r1_trial, # only keep 1000 of r1 sims
         r2_trial < max(r2_sims$r2_trial) * 0.5 # cut r2 sims in half
         ) %>%
  group_by(r1_trial, r2_trial) %>%
  mutate(r2_winner = cand[1],
         r2_loser = cand[2],
         r2_winner_vote = vote[1],
         r2_loser_vote = vote[2]) %>%
  dplyr::select(r1_trial, r2_trial,
         r2_matchup = matchup,
         r2_winner, r2_loser, r2_winner_vote, r2_loser_vote) %>%
  as.data.table()

grouped_sims = sims_for_right_join[sims_for_left_join, on = "r1_trial"]

# only _write_ 1,000 sims
grouped_sims %>%
  write_rds(., 'output-data/site-sync/raw_r1_r2_combined_simulations.rds',compress='gz')

# all done?
# beepr::beep(2)
end_time = Sys.time()
print(sprintf('elapsed time is %s minutes',round(end_time - start_time,1)))
print(overall_probs)

#} # this bracket ends the for loop if you're running on multiple days

paste0('{"timestamp":',as.integer(Sys.time()),'}') %>% cat(file='output-data/site-sync/timestamp.json')

