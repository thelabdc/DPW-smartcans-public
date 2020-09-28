rm(list = ls())

library(jsonlite)
library(lubridate)
library(randomizr)
library(rstan)
library(tidyverse)

DIR <- '~/repo/DPW-smartcans-public'
setwd(DIR)


df_fills <- read_csv('data/littercan_fills.csv')

# Get can level data
df_cans <- df_fills %>%
  select(id = MeId, Address, Latitude, Longitude) %>%
  unique() %>%
  mutate(
    can_id = 1:n(),
    address_street = unlist(lapply(strsplit(Address, split = ' '), function(x) paste(x[2:length(x)], collapse = ' '))),
    address_number = unlist(lapply(strsplit(Address, split = ' '), function(x) x[1])),
    even_odd       = ifelse(as.numeric(address_number) %% 2 == 0, 'even', 'odd'),
    address_street = gsub('U Street Northwest', 'U St NW', address_street),
    address_street = gsub('Nannie Helen Burroughs Ave NE', 'Nannie Helen Burroughs Ave', address_street),
    address_street = trimws(address_street, which = 'both'),
    street_block   = ifelse(nchar(address_number) == 1, substring(address_number, 1, 1), substring(address_number, 1, nchar(address_number) - 2)))


# Block-level randomization
# First requires proper grouping
df_cans <- within(df_cans, {
  area <- address_street
  area[address_street == 'Benning Rd SE']                                  <- 'Benning Rd NE/SE'
  area[address_street == 'Benning Rd NE' & as.numeric(street_block) > 26]  <- 'Benning Rd NE/SE'
  area[address_street == 'H St NE']                                        <- 'H St NE/Benning Rd NE' 
  area[address_street == 'Benning Rd NE' & as.numeric(street_block) <= 26] <- 'H St NE/Benning Rd NE'
  area[grepl('[0-9]{1,2}th St NE', address_street)] <- 'H St NE/Benning Rd NE'
  area[address_street == '3rd St NE']               <- 'H St NE/Benning Rd NE'
  area[address_street == 'E Capitol St NE']         <- 'E Capitol St'
  area[address_street == 'E Capitol St SE']         <- 'E Capitol St'
  area[address_street == 'Central Ave NE']          <- 'E Capitol St' 
  area[address_street == '49th St SE']              <- 'E Capitol St'
  area[address_street == '55th St NE']              <- 'Nannie Helen Burroughs Ave'
  area[address_street == 'Grant St NE']             <- 'Nannie Helen Burroughs Ave'
  area[address_street == 'Rodman St NW']            <- 'Wisconsin Ave NW'
  area[address_street == 'Idaho Ave NW']            <- 'Wisconsin Ave NW'
  area[address_street == 'Ellicott St NW']          <- 'Wisconsin Ave NW'
  area[address_street == 'Yuma St NW']              <- 'Wisconsin Ave NW'
})

set.seed(999)
blocksides <- df_cans %>% 
  select(area, address_street, street_block, even_odd) %>%
  unique() %>%
  mutate(block_id = 1:n())

Z <- block_ra(blocksides$area)
blocksides$Z <- Z 

df_final <- df_cans %>%
  left_join(blocksides) %>%
  mutate(block_side = paste(address_street, street_block, even_odd, sep = '_')) %>%
  select(id, can_id, Address, block_side, block_id, Latitude, Longitude, area, Z)


# get daily max fill levels
daily_max <- df_fills %>% 
  mutate(date = date(RecordedDateTime)) %>% 
  group_by(id = MeId, date) %>% 
  summarize(max_fill = max(CalculatedPercentFull)) %>%
  left_join(df_final)


create_fake_data <- function(data, days, diff){
  # take can-level data, and generate some fake observations
  # -- data (dataframe): dataframe at the can level containing treatment indicator (named Z)
  # -- days (int): How many days will the experiment run?
  # -- diff (int): What's the average % diff between treatment and control cans?
  
  new_dfs    <- list()
  
  fill_avg   <- mean(daily_max$max_fill)
  fill_sd    <- sd(daily_max$max_fill)
  
  n_treated  <- data %>% filter(Z == 1) %>% nrow()
  n_control  <- data %>% filter(Z == 0) %>% nrow()
  
  # give control group random fill levels that correspond with the average currently found in the data
  # and treatment group random fill levels with current average + diff
  for(i in 1:days){
    fake_data <- within(data, {
      day                        <- i
      max_fill                   <- NA
      max_fill[Z == 0]   <- round(rnorm(n = n_control, mean = fill_avg, sd = fill_sd), 3) / 100
      max_fill[Z == 1]   <- round(rnorm(n = n_treated, mean = fill_avg + diff, sd = fill_sd), 3) / 100
    })
    
    new_dfs[[i]] <- fake_data
  }
  
  full_data <- do.call('rbind', new_dfs) 
  
  return(full_data)
}


create_fake_data_count <- function(data, days, diff){
  # take block-level data, and generate some fake on litter counts
  # -- data (dataframe): dataframe at the can level containing treatment indicator (named Z)
  # -- days (int): How many days will the experiment run?
  # -- diff (int): What's the average % diff between treatment and control cans?
  
  new_dfs    <- list()
  
  n_treated  <- data %>% filter(Z == 1) %>% nrow()
  n_control  <- data %>% filter(Z == 0) %>% nrow()
  
  # give control group random fill levels that correspond with the average currently found in the data
  # and treatment group random fill levels with current average + diff
  for(i in 1:days){
    fake_data <- within(data, {
      day                        <- i
      litter_count               <- NA
      litter_count[Z == 0]       <- rpois(n = n_control, lambda = 5)
      litter_count[Z == 1]       <- rpois(n = n_treated, lambda = 5 + diff)
    })
    
    new_dfs[[i]] <- fake_data
  }
  
  full_data <- do.call('rbind', new_dfs) 
  
  return(full_data)
}


estimate_model_linear <- function(data, days, diff){
  #  Estimate models using rstan 
  #  Set parameters to pass to create_fake_data() function
  # -- data (dataframe): dataframe at the can level containing treatment indicator (named Z)
  # -- days (int): How many days will the experiment run?
  # -- diff (int): What's the average % diff between treatment and control cans?
  
  df_test  <- create_fake_data(data, days, diff)
  df_can   <- df_test %>% select(can_id, block_id) %>% unique()
  df_block <- df_test %>% select(block_id, Z) %>% unique()
  
  model <- stan(file='../stan_hlm.stan', 
                data=list(
                  N = nrow(df_test),
                  J = length(unique(df_test$can_id)),
                  K = length(unique(df_test$block_id)),
                  CanID = df_test$can_id,
                  BlockID = df_block$block_id,
                  CansToBlocks = df_can$block_id,
                  fill = df_test$max_fill,
                  treatment = df_block$Z
                ), 
                iter=800, 
                chains=4,
                cores=4)
  
  
}


estimate_model_count <- function(data, days, diff){
  #  Estimate models using rstan 
  #  Set parameters to pass to create_fake_data() function
  # -- data (dataframe): dataframe at the can level containing treatment indicator (named Z)
  # -- days (int): How many days will the experiment run?
  # -- diff (int): What's the average % diff between treatment and control cans?
  
  df_test  <- create_fake_data_count(data, days, diff)
  df_block <- df_test %>% select(block_id, Z) %>% unique()
  
  
  #int<lower=1> N; // number of observations
  #int<lower=1> K; // number of block sides
  
  #int<lower=1, upper=K> DayBlockSide[N]; // id of block-side of individual observation
  #int<lower=1, upper=K> BlockID[K]; // Block-side IDs
  
  #int<lower=0> count[N];              // response variable (litter count)
  #int<lower=0, upper=1> treatment[K]; // treatment indicator (at block-side level)
  
  model <- stan(file='stan_hlm_poisson.stan', 
                data=list(
                  N = nrow(df_test),
                  J = nrow(df_block),
                  BlockID = df_test$block_id,
                  count = df_test$litter_count,
                  treatment = df_block$Z
                ), 
                iter=600, 
                chains=3,
                cores=4)
  
  
}

days_diffs <- expand.grid(days = c(7, 14, 21), diffs = c(1, 3, 5, 7))

models_output <- list()
for(i in 1:nrow(days_diffs)){
  days = days_diffs[i, 'days']
  diff = days_diffs[i, 'diffs']
  
  models_output[[i]] <- list()
  models_output[[i]]$days <- days
  models_output[[i]]$diff <- diff
  
  df <- data.frame()
  for(j in 1:50){
    output   <- estimate_model(df_final, days = days, diff = diff)
    beta_trt <- summary(output, pars = 'beta_trt')$summary
    df <- rbind(df, beta_trt)
  }
  
  models_output[[i]]$output <- df
  
}

days_diffs$power <- rep(NA, nrow(days_diffs))
days_diffs$mean_of_means <- rep(NA, nrow(days_diffs))
for(i in 1:nrow(days_diffs)){
  df    <- models_output[[i]]$output
  power <- sum(ifelse(df$`2.5%` > 0, 1, 0)) / 50
  days_diffs$power[i] <- power
  days_diffs$mean_of_means[i] <- mean(df$mean)
}

power <- ggplot(days_diffs, aes(x = diffs, y = power, group = as.factor(days), color = as.factor(days))) + 
  geom_point() +
  geom_line() +
  labs(title = 'Simulated Power Calculation',
       x = 'Difference in Mean Fill Percentage Between Treatment and Control Group',
       y = 'Estimated Power',
       colour = 'Number of Days in Field')


png('power_calc.png', width = 600, height = 400)
power
dev.off()
