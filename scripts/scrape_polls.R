library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)


# read polls for round 1 --------------------------------------------------
polls_r1 = read_csv('https://raw.githubusercontent.com/nsppolls/nsppolls/master/presidentielle.csv') %>% 
  filter(tour == 'Premier tour',
         fin_enquete >= ymd('2021-08-01')) %>%
  select(end_date = fin_enquete,
         polling_firm = nom_institut,
         sample_size = echantillon,
         candidate_variation = hypothese,
         cand = candidat,
         pct = intentions
         )

# cleaning up column names and data types
polls_r1 = polls_r1 %>%
  dplyr::select(polling_firm,
                end_date, 
                candidate_variation,
                sample_size,
                everything()) %>%
  spread(cand, pct) %>%
  clean_names 

# HARD CODE candidates we want to include
names(polls_r1)

polls_r1 = polls_r1 %>%
  dplyr::select(1:4,
                arthaud = nathalie_arthaud,
                poutou = philippe_poutou,
                roussel = fabien_roussel,
                melenchon = jean_luc_melenchon,
                hidalgo = anne_hidalgo,
                taubira = christiane_taubira,
                # montebourg = arnaud_montebourg, # withdrawn
                jadot = yannick_jadot,
                macron = emmanuel_macron,
                lassalle = jean_lassalle,
                pecresse = valerie_pecresse,
                dupont_aignan = nicolas_dupont_aignan,
                le_pen = marine_le_pen,
                # philippot = florian_philippot, # not clear running?
                zemmour = eric_zemmour,
                # asselineau = francois_asselineau
  ) 


# for duplicates entries, pick polls with the most candidates (after removing those who aren't running)
polls_r1$missing_columns = rowSums(is.na(polls_r1))
polls_r1$missing_sum = rowSums(polls_r1[,5:(ncol(polls_r1)-1)],na.rm=T)

polls_r1 = polls_r1 %>%
  group_by(polling_firm, end_date, sample_size) %>%
  # first filter by missing columns
  filter(missing_columns == min(missing_columns)) %>%
  # then pick the poll with higher sum
  filter(missing_sum == max(missing_sum)) %>%
  # if still a tie, pick first entry
  filter(row_number() == min(row_number())) %>%
  # and remove polls that sum above 110 or dramatically under 100
  filter(missing_sum <= 101 & missing_sum >= 80) %>%
  ungroup() %>%
  select(-c(missing_columns, missing_sum)) 

# long format
polls_r1 = polls_r1 %>%
  gather(cand,pct,5:ncol(.)) 

# wrangling some stray columns...
polls_r1 = polls_r1 %>%
  mutate(pct = pct / 100,
         end_date.num = as.numeric(end_date),
         weight = sqrt(sample_size / mean(sample_size, na.rm=T)),
         weight = ifelse(is.na(weight),mean(weight, na.rm=T),weight)) %>%
  arrange(end_date,cand) %>%
  ungroup() %>%
  select(end_date, end_date.num, polling_firm, sample_size, weight, cand, pct) %>%
  group_by(cand) %>%
  mutate(pct = imputeTS::na_ma(pct,k = 3,weighting = 'linear')) %>%
  filter(end_date >= ymd('2021-09-01'))

# remove NAs
polls_r1 = polls_r1 %>% 
  ungroup() %>%
  na.omit() %>%
  distinct() %>%
  arrange(end_date, polling_firm)

# create fake 'other' candidate to hold percentages for those who have dropped out
# this is necessary because we make sure %ages sum == 1 in the next scripts
# polls_r1 = polls_r1 %>%
#   group_by(end_date, end_date.num, polling_firm, sample_size, weight) %>%
#   mutate(other_cand_dummy = 1 - sum(pct)) %>% 
#   spread(cand, pct)  %>%
#   gather(cand,pct,6:ncol(.)) 

# get polls for round 2 ---------------------------------------------------
# read in data
polls_r2 = read_csv('https://raw.githubusercontent.com/nsppolls/nsppolls/master/presidentielle.csv') %>% 
  filter(tour == 'Deuxième tour',
         fin_enquete >= ymd('2021-08-01')) %>%
  select(end_date = fin_enquete,
         polling_firm = nom_institut,
         sample_size = echantillon,
         matchup = hypothese,
         cand = candidat,
         pct = intentions
  ) 

# remove hypotheses with candidates we know we don't want
# bertrand; barnier
polls_r2 = polls_r2 %>%
  filter(!grepl('bertrand|barnier',tolower(matchup)))

# HARD CODE MATCHUPS
recode_matchups = function(x){
  y = gsub('Hypothèse ','',x)
  y = gsub('Hytpothèse ','',y)
  y = tolower(y)
  y = gsub(" / ",'VV',y)
  y = gsub(" ",'_',y)
  y = stringi::stri_trans_general(y,"Latin-ASCII")
  return(y)
}
unique(polls_r2$matchup)
recode_matchups(polls_r2$matchup)
recode_candidates = function(x){
  return(
    case_when(grepl('Pécresse',x) ~ 'pecresse',
              grepl('Macron',x) ~ 'macron',
              grepl('Le Pen',x) ~ 'le_pen',
              grepl('Zemmour',x) ~ 'zemmour',
              grepl('Mélenchon',x) ~ 'melenchon'
              )
  )
}

polls_r2 = polls_r2 %>%
  # recode names and matchups
  mutate(matchup = recode_matchups(matchup),
         cand = recode_candidates(cand)) %>%
  # correct some miscodings
  mutate(matchup = case_when(matchup == 'le_penVVmacron' ~ 'macronVVle_pen',
                             matchup == 'le_penVVpecresse' ~ 'pecresseVVle_pen',
                             TRUE ~ matchup)) %>%
  # if no matchup, add one
  group_by(end_date, polling_firm, sample_size) %>%
  mutate(matchup = case_when(is.na(matchup) &
                               sprintf('%sVV%s',cand[1],cand[2]) %in% .$matchup ~
                               sprintf('%sVV%s',cand[1],cand[2]),
                             is.na(matchup) &
                               !sprintf('%sVV%s',cand[1],cand[2]) %in% .$matchup ~
                               sprintf('%sVV%s',cand[2],cand[1]),
                             TRUE ~ matchup
                          )) %>% 
  group_by(end_date, polling_firm, sample_size, matchup, cand) %>%
  mutate(cand_number = match(cand, unlist( str_split(matchup,'VV') ))
         )

unique(polls_r2$matchup)

# remove any blanks, then spread wide
polls_r2 = polls_r2 %>%
  group_by(end_date, polling_firm, sample_size, matchup) %>%
  filter(!any(is.na(cand_number))) %>% 
  arrange(end_date, polling_firm, sample_size, matchup, cand_number) %>%
  mutate(cand1 = cand[1],
         cand2 = cand[2],
         cand1_pct = pct[1],
         cand2_pct = pct[2]) %>%
  select(polling_firm, end_date, sample_size, 
         matchup, cand1, cand2, 
         cand1_pct, cand2_pct) %>%
  distinct() %>%
  ungroup()

# wrangling
polls_r2 = polls_r2 %>%
  mutate(cand1_pct = as.numeric(gsub('%|<','',cand1_pct)) / 100,
         cand2_pct = as.numeric(gsub('%|<','',cand2_pct)) / 100,
         end_date.num = as.numeric(end_date),
         weight = sqrt(sample_size / mean(sample_size, na.rm=T)),
         weight = ifelse(is.na(weight),mean(weight, na.rm=T),weight)) %>%
  filter(end_date >= ymd('2021-09-01')) %>%
  ungroup() %>%
  select(end_date, end_date.num, polling_firm, sample_size, weight,
         matchup, cand1, cand2, cand1_pct, cand2_pct) %>%
  arrange(end_date,polling_firm)



# save these --------------------------------------------------------------
# recent outp
write_csv(polls_r1, 'output-data/raw_polls_round_one_current.csv')
write_csv(polls_r2, 'output-data/raw_polls_round_two_current.csv')


