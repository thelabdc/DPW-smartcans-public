# DPW-smartcans-public

Litter is a pervasive problem in Washington, DC and in cities throughout the world. Through a randomized control trial, the Department of Public Works and The Lab @ DC tested whether we could reduce litter by placing signs with research-based messages on public litter cans. We found that the signs did not, on average, reduce visible litter on the street or increase the amount of garbage in the litter cans.  We did find some suggestive evidence that the signs could decrease visible litter in particularly high-litter areas.

This repository consists of three directories:

- analysis
|- analysis_code.R
|- stan_hlm_poisson.stan
|- stan_hlm.stan
|- stan_simple_negbin.stan
|- stan_simple_poisson.stan

- data
|- completed_litter_counts_anonymized.csv
|- littercan_fills.csv
|- littercans_randomized.csv

- power_calcs
|- power_calcs.R

- randomization
|- generate_randomization.R

## Reproducing the results.
The original data consists of two files. One is not shared because of privacy issues. Instead, that dataset is made public by replacing the field "counter" (which contains the name of the employee doing the counting) with anonymized values ("counter1", "counter2", "counter3", etc.). The result of that is the file data/completed_litter_counts_anonymized.csv. The other original data file (data/littercan_fills.csv) is included here as is.

Power calculations are run by power_calcs/power_calcs.R, starting from data/littercan_fills.csv.

Randomization was carried out in randomization/generate_randomization.csv, which reads data/littercan_fills.csv and outputs data/littercans_randomized.csv.

The program analysis/analysis_code.R carries out the analysis by using all three data files included in the data directory and the stan files in the analysis folder. Its outputs are saved as tables in csv files.



