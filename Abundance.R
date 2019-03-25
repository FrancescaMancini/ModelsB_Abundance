####################################################################
## Task 4.2 Models B - total abundance of pollinating insects
## Author: Francesca Mancini
## Date created: 2019-03-18
## Date modified:
####################################################################

library(dplyr)
library(BRCmap)
library(tidyr)


FIT_public <- read.csv("P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\data outputs current versions\\tblEXPORT_PublicFITCount.csv", 
                 header = T, stringsAsFactors = F)

str(FIT_public)

# change NAs to 0s
FIT_public <- FIT_public %>%
  mutate(bumblebees = replace_na(bumblebees, 0),
         honeybees = replace_na(honeybees, 0),
         solitary_bees = replace_na(solitary_bees, 0),
         wasps = replace_na(wasps, 0),
         hoverflies = replace_na(hoverflies, 0),
         other_flies = replace_na(other_flies, 0),
         butterflies_moths = replace_na(butterflies_moths, 0),
         beetles = replace_na(beetles, 0),
         insects_small = replace_na(insects_small, 0),
         insects_other = replace_na(insects_other, 0),
         all_insects_total = replace_na(all_insects_total, 0))

FIT_public <- FIT_public %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y %H:%M"),
         JulDate = as.numeric(format(date, "%j")),
         year = format(date,"%Y"),
         site_1km = as.factor(reformat_gr(FIT_public$sample_gridref, prec_out = 1000)),
         site_1km_num = as.numeric(site_1km),
         country = as.factor(country),
         flower_new = as.factor(case_when(target_flower_corrected == "Other - please describe below" ~ "Other",
                                TRUE ~ "Target")),
         flower_context = as.factor(flower_context),
         wind_speed = as.factor(wind_speed))

FIT_public_2018 <- subset(FIT_public, year == "2018")

# some observations do not have a grid reference in the sample_gridref column but lat and long
# this results into NAs when conb=verting to 1 Km precision
# ignore for now then ask Claire

FIT_public_2018 <- FIT_public_2018[!is.na(FIT_public_2018$site_1km),]


# GAMM for all insects ----



