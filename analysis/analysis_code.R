rm(list = ls())

library(AER)
library(gridExtra)
library(irr)
library(jsonlite)
library(MASS)
library(rstan)
library(tidyverse)

REPO_DIR   <- 'C:/users/bill.egar/repo/DPW-smartcans-public/'
setwd(REPO_DIR)

# Read data ------------------------------------------------------------------------------------------  
df_fills <- read_csv(file.path(REPO_DIR, 'data', 'littercan_fills.csv'))

# read in csv of field litter counts - non-anonymized file in Box
df_litter <- read_csv(file.path(REPO_DIR, 'data', 'completed_litter_counts_anonymized.csv'))

# read in littercan randomization csv
df_cans <- read_csv(file.path(REPO_DIR, 'data', 'littercans_randomized.csv'))


# Fill level analysis ------------------------------------------------------------------------------------------ 
# create daily max fill data by day/littercan
daily_max <- df_fills %>%
  group_by(MeId, date_reading = lubridate::date(RecordedDateTime)) %>%
  summarize(max_fill = max(CalculatedPercentFull)) %>%
  mutate(overfill = ifelse(max_fill > 100, 1, 0))

# join randomization roster onto fill data
# remove cans for which we don't have any treatment assignment recorded (must've been some additions?)
df_can_day <- daily_max %>%
  left_join(df_cans %>%
              filter(MeId %in% df_fills$MeId) %>%
              mutate(can_id = 1:n()), 
            by = 'MeId') %>%
  filter(!is.na(Z)) %>%
  mutate(max_fill = max_fill / 100) %>%
  ungroup()

df_can <- df_can_day %>%
  select(can_id, block_id) %>%
  unique()

df_block <- df_can_day %>%
  select(block_id, Z) %>%
  unique() %>%
  # because of discrepancy in original randomization list, the block_id field doesnt behave as it should. 
  # originally 196 block-sides that were randomized, but 177 found in the data.
  # So make a new one.
  mutate(block_id_clean = 1:n())
  
# join clean block id field back onto can level data
df_can <- df_can %>%
  left_join(df_block, by = 'block_id')

# plot avg fill by day/area/treatment group
df_can_day %>% 
  group_by(area, Z, date_reading) %>% 
  summarize(avg = mean(max_fill)) %>% 
  ungroup() %>%
  mutate(Z = ifelse(Z == 1, 'Treatment', 'Control')) %>%
  ggplot(aes(x = date_reading, y = avg, linetype = Z)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~ area, ncol = 2) + 
  labs(x = 'Date',
       y = 'Average Daily Max Fill Pct.',
       linetype = 'Assignment') +
  theme_classic()


# plot avg fill by day/treatment group
df_can_day %>% 
  group_by(Z, date_reading) %>% 
  summarize(avg = mean(max_fill)) %>% 
  ungroup() %>%
  mutate(Z = ifelse(Z == 1, 'Treatment', 'Control')) %>%
  ggplot(aes(x = date_reading, y = avg, linetype = Z)) + 
  geom_line() + 
  geom_point() + 
  labs(x = 'Date',
       y = 'Average Daily Max Fill Pct.',
       linetype = 'Assignment') +
  theme_classic()

# estimate model
model_fill <- stan(file = file.path(REPO_DIR, 'analysis', 'stan_hlm.stan'), 
                   data = list(
                     N = nrow(df_can_day),
                     J = nrow(df_can),
                     K = nrow(df_block),
                     CanID = df_can_day$can_id,
                     BlockID = df_block$block_id_clean,
                     CansToBlocks = df_can$block_id_clean,
                     fill = df_can_day$max_fill,
                     treatment = df_block$Z
                   ), 
                   iter=1000, 
                   chains=2, 
                   cores=2,
                   pars = 'beta_trt',
                   include = TRUE)

beta_trt_summary_fill <- summary(model_fill, pars = 'beta_trt')$summary
beta_trt_draws_fill   <- rstan::extract(model_fill)

beta_trt_plot_fill <- ggplot(data.frame(beta_trt_draws_fill), aes(x = beta_trt)) + 
  geom_density(fill = 'white') +
  labs(x = 'Coefficient on Treatment Effect',
       y = 'Density',
       title = 'Fill Level Model')

d <- ggplot_build(beta_trt_plot_fill)$data[[1]]

plot_null_effect_fill <- beta_trt_plot_fill + 
  geom_area(data = subset(d, x >= beta_trt_summary_fill[, '2.5%'] & x <= beta_trt_summary_fill[, '97.5%']), 
            aes(x=x, y=y), 
            fill="blue",
            alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = 2, size = 1) + 
  xlim(-0.5, 0.55) +
  ylim(0, 4.5) +
  theme_classic()


# Litter Count analysis ------------------------------------------------------------------------------------------ 
# discarding the first count on E. Capitol Street due to weird counts observed
df_block_litter_count <- df_litter %>%
  filter(!(area == 'E Capitol St' & which_count == 'first')) %>%
  group_by(block, even_odd) %>%
  summarize(litter_count = round(mean(litter_count), 0),
            day = unique(as.character(date)),
            nov11 = as.numeric(day == '11/11/17'),
            nov27 = as.numeric(day == '11/27/17'),
            dec01 = as.numeric(day == '12/01/17'),
            dec08 = as.numeric(day == '12/8/17'),
            dec12 = as.numeric(day == '12/12/17')) %>%
  ungroup()

litter_counts <- df_cans %>%
  mutate(block = paste(street_block_full, address_street)) %>%
  select_('area', 'block', 'even_odd', 'Z') %>%
  unique() %>%
  left_join(df_block_litter_count, by = c('block', 'even_odd')) %>%
  filter(!is.na(litter_count))

pois <- glm(litter_count ~ Z + day, data = litter_counts, family = 'poisson')
dispersiontest(pois)

nbreg <- glm.nb(litter_count ~ Z + area, data = litter_counts)

model_count <- stan(file = file.path(REPO_DIR, 'analysis', 'stan_simple_negbin.stan'), 
                    data=list(
                      N = nrow(litter_counts),
                      count = litter_counts$litter_count,
                      treatment = litter_counts$Z,
                      nov27 = litter_counts$nov27,
                      dec01 = litter_counts$dec01,
                      dec08 = litter_counts$dec08,
                      dec12 = litter_counts$dec12
                    ), 
                    iter = 1000, 
                    chains = 2,
                    cores = 2)

print(get_seed(model_count))
beta_trt_summary_count <- summary(model_count, pars = 'beta_trt')$summary
beta_trt_draws_count   <- rstan::extract(model_count)

beta_trt_plot_count <- ggplot(data.frame(beta_trt_draws_count), aes(x = beta_trt)) + 
  geom_density(fill='white') +
  labs(x = 'Coefficient on Treatment Effect',
       y = 'Density',
       title = 'Litter Count Model')

d <- ggplot_build(beta_trt_plot_count)$data[[1]]

plot_null_effect_counts <- beta_trt_plot_count + 
  geom_area(data = subset(d, x >= beta_trt_summary_count[, '2.5%'] & x <= beta_trt_summary_count[, '97.5%']), 
            aes(x=x, y=y), 
            fill="blue",
            alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = 2, size = 1) + 
  xlim(-0.5, 0.55) +
  ylim(0, 4.5) +
  theme_classic()

# combine the null effect plots
grid.arrange(plot_null_effect_fill, plot_null_effect_counts)

# inter-rater reliability
df_litter_wide <- df_litter %>%
  filter(!(area == 'E Capitol St' & which_count == 'first')) %>%
  unite(blockside, area, block, even_odd) %>%
  dplyr::select(-date, -which_count) %>%
  spread(counter, litter_count) %>%
  separate(blockside, sep = '_', into = c('area', 'block', 'even_odd'))

df_litter_wide %>%
  ggplot(aes(x = A, y = B)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(~ area)

df_litter_wide %>%
  summarize(reliability = cor(A, B))

icc_all <- icc(df_litter_wide %>% dplyr::select(A, B))$value

iccs <- data.frame(
  area = unique(df_litter_wide$area),
  icc  = rep(NA, length(unique(df_litter_wide$area))),
  corr = rep(NA, length(unique(df_litter_wide$area)))
)

for(i in 1:nrow(iccs)){
  area <- iccs$area[i]
  iccs$icc[i]   <- icc(df_litter_wide[df_litter_wide$area == area, ] %>% dplyr::select(A, B))$value
  iccs$corr[i]  <- cor(df_litter_wide$A[df_litter_wide$area == area], df_litter_wide$B[df_litter_wide$area == area])
}

iccs %>%
  mutate(icc = round(icc, 3),
         corr = round(corr, 3)) %>%
  rename(Area = area,
         ICC = icc,
         `Pearson Correlation` = corr) %>%
  write_csv(file.path(REPO_DIR, 'data', 'icc_by_area.csv'))

# generate basic summary tables -----------------------------------

# fill level
bind_rows(
  
  # by area
  df_can_day %>%
    group_by(area) %>%
    summarize(`Treatment Obs.` = sum(Z),
              `Control Obs.` = sum(Z == 0),
              `Total Obs.` = n()) %>%
    left_join(df_can_day %>% 
                group_by(area, Z) %>% 
                summarize(avg = round(mean(max_fill), 3)) %>% 
                spread(Z, avg)),
  # all
  df_can_day %>% 
    group_by(Z) %>% 
    summarize(avg = round(mean(max_fill), 3)) %>% 
    spread(Z, avg) %>%
    mutate(`Treatment Obs.` = sum(df_can_day$Z),
           `Control Obs.`   = sum(df_can_day$Z == 0),
           `Total Obs.`     = nrow(df_can_day),
           area = 'All')
) %>%
  mutate(Difference = `1` - `0`,
         Difference = round(Difference, 3)) %>%
  rename(Area = area,
         `Avg. in Control` = `0`,
         `Avg. in Treatment` = `1`) %>%
  write_csv(file.path(REPO_DIR, 'data', 'avg_fill_table.csv'))


# litter counts
bind_rows(
  
  # by area
  litter_counts %>%
    group_by(area) %>%
    summarize(`Treatment Obs.` = sum(Z),
              `Control Obs.` = sum(Z == 0),
              `Total Obs.` = n()) %>%
    left_join(litter_counts %>% 
                group_by(area, Z) %>% 
                summarize(avg = round(mean(litter_count), 1)) %>% 
                spread(Z, avg)),
  # all
  litter_counts %>% 
    group_by(Z) %>% 
    summarize(avg = round(mean(litter_count), 1)) %>% 
    spread(Z, avg) %>%
    mutate(`Treatment Obs.` = sum(litter_counts$Z),
           `Control Obs.`   = sum(litter_counts$Z == 0),
           `Total Obs.`     = nrow(litter_counts),
           area = 'All')
) %>%
  mutate(Difference = `1` - `0`,
         Difference = round(Difference, 3)) %>%
  rename(Area = area,
         `Avg. in Control` = `0`,
         `Avg. in Treatment` = `1`) %>%
  write_csv(file.path(REPO_DIR, 'data', 'avg_count_table.csv'))
