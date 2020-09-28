rm(list = ls())

library(jsonlite)
library(lubridate)
library(randomizr)
library(tidyverse)

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
    street_block   = ifelse(nchar(address_number) == 2, substring(address_number, 1, 1), substring(address_number, 1, nchar(address_number) - 2)),
    street_block_full = ifelse(nchar(address_number) == 2, paste0(street_block, '0'), paste0(street_block, '00')))


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
  select(area, address_street, street_block_full, even_odd) %>%
  unique() %>%
  mutate(block_id = 1:n())

Z <- block_ra(blocksides$area)
blocksides$Z <- Z 

df_final <- df_cans %>%
  left_join(blocksides) %>%
  mutate(block_side = paste(address_street, street_block_full, even_odd, sep = '_')) %>%
  select(id, can_id, Address, address_street, street_block_full, block_side, block_id, even_odd, Latitude, Longitude, area, Z) 

cans_output <- df_final %>%
  mutate(block_side = paste(street_block_full, address_street, even_odd, sep = ' ')) %>%
  #filter(Z == 1) %>%
  arrange(address_street, street_block_full, even_odd)
  
blocks_output <- blocksides %>%
  mutate(block_side = paste(street_block_full, address_street, even_odd, sep = ' ')) %>%
  #filter(Z == 1) %>%
  arrange(address_street, street_block_full, even_odd)

write.csv(cans_output, 'littercans_randomized.csv', row.names = FALSE)
write.csv(blocks_output, 'blocksides_randomized.csv', row.names = FALSE)
