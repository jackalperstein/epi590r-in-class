nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

library(tidyverse)
# This will not always work because we all have different directories
setwd("C:/Users/jacka/OneDrive - Emory University/Class Files/Fall 23/EPI 590R - R Bootcamp/Repositories/epi590r-in-class/data/raw")
nlsy <- read_csv("nlsy.csv",
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1,
								 col_names = nlsy_cols)

library(dplyr)
nlsy2 <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

nlsy3 <- na.omit(nlsy2)

setwd("../clean/")
write_rds(nlsy3, "nlsy-complete-cases.rds")
