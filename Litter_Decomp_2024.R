# This file cleans the L1 DEAD mass loss data file and performs initial mass remaining calculations
# written Nov 4-6, 2021 HLT
# Edited Jun 19, 2022 ENN
# revamped 2024-03-23 HLT

library(tidyverse)
library(here) # see https://malco.io/articles/2018-11-05-why-should-i-use-the-here-package-when-i-m-already-using-projects

# Import data, the CSV file...
Litter <- read.csv(here("Decomposition_16012024.csv"))
# Clean up df by removing final column (calculated variable that has missing values and will not be directly needed)
Litter <- Litter |>
  mutate(
    Ash.remaining = NULL,
  )

# convert character columns to factors
Litter$Species <- factor(Litter$Species)
Litter$Litter_substrate <- factor(Litter$Litter_substrate)
Litter$microsite <- factor(Litter$microsite)

# Transport Loss Corrections ------------------------------------------------------

Litter_T0 <- Litter %>%  # new df that includes only T0 bags, for transport loss calculations
    filter(Month == 0, na.rm = TRUE)

Litter_T0 <- Litter_T0 %>%      # create new variable, tranport loss (litter loss in T0 bags, g)
  mutate(trsptloss_g = Initial_mass - Lbag_weight)

Litter_T0 <- Litter_T0 %>%      # create new variable, tranport loss (%)
  mutate(trsptloss_pct = trsptloss_g / Initial_mass * 100)

# calculate means for transport losses for each litter substrates/type
T0means <- Litter_T0 |>
  group_by(Litter_substrate) |>
  summarize(  N    = sum(!is.na(trsptloss_pct)),
              mean_trsptloss_pct = mean(trsptloss_pct, na.rm=TRUE),
              sd_trsptloss_pct   = sd(trsptloss_pct, na.rm=TRUE),
              se_trsptloss_pct   = sd_trsptloss_pct / sqrt(N)
  )
print(T0means)  

# explore transport loss data - the distribution is perplexing 
hist(Litter_T0$trsptloss_pct)
# check for normality of distribution of transport losses
qqnorm(Litter_T0$trsptloss_pct)
qqline(Litter_T0$trsptloss_pct) # visualize the data - if normal the data should fall fairly 

### Given the oddity of transport loss distribution, I don't see a complelling arguement for a transport loss correction 
# create new variable final litter mass, corrected for transport loss (final_litter_corr)
# apply a species-specific transport loss correction using mutate and multiple if else conditions
#Litter <- mutate(Litter, Initial_mass_corr = ifelse(Litter_substrate == "Grass", Initial_mass * 0.9920,
#                                                            ifelse(Litter_substrate == "Shrub", Initial_mass * 1.000,"NA")))

# create new variable, pctrem_corr (percent mass remaining, using the transport-corrected initial litter value)
#Litter$Initial_mass_corr <- as.numeric(as.character(Litter$Initial_mass_corr)) # convert to numeric 
#Litter <- Litter %>%
#  mutate(pctrem_corr = Lbag_weight/Initial_mass_corr*100)


# Calculate ash-free masses and ash-free mass remaining -----------------------------------------

# calculate percent ash of the litter for all litterbags
### HT: Note that percent ash seems very low for time 0 -- these should be double-checked to increase our confidence
Litter <- Litter |>
  mutate(
    sample_mass = Sample_cruc_mass - Crucible_mass,
    ash_mass = Ash_cruc_mass - Crucible_mass,
    pct_ash = (ash_mass/sample_mass)*100
  )

# pull the mean pct_ash values from the time 0 means for each of two litter substrates
pct_ash_means <- Litter |>
  group_by(Litter_substrate, Month) |>
  summarise(
    N    = sum(!is.na(pct_ash)),
    pct_ash = mean(pct_ash, na.rm=TRUE),
    sd_pct_ash   = sd(pct_ash, na.rm=TRUE),
    se_pct_ash   = sd_pct_ash / sqrt(N))

# define new variables for the inital percent ash for grass and shrub litter
grass_initial_pctash <- as.numeric(pct_ash_means[1,4])
shrub_initial_pctash <- as.numeric(pct_ash_means[5,4])

# calculate ash-free initial and final litter masses
Litter <- Litter |>
  mutate(
    Initial_mass_ash_free = ifelse(Litter_substrate == "Grass", 
                                   Initial_mass * (100 - grass_initial_pctash) / 100, 
                                   ifelse(Litter_substrate == "Shrub", 
                                          Initial_mass * (100 - shrub_initial_pctash) / 100, 
                                          NA)), # use NA for other cases
    Final_mass_ash_free = Lbag_weight * (100 - pct_ash) / 100,
    Pct_mass_remaining_ashfree = Final_mass_ash_free/Initial_mass_ash_free *100
  ) 


