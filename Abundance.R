####################################################################
## Task 4.2 Models B - total abundance of pollinating insects
## Author: Francesca Mancini
## Date created: 2019-03-18
## Date modified: 2019-03-27
####################################################################

library(plyr)
library(dplyr)
library(BRCmap)
library(tidyr)
library(gamm4)
library(ggplot2)
library(openxlsx)



FIT_public <- read.csv("P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\data outputs current versions\\tblEXPORT_PublicFITCount.csv", 
                 header = T, stringsAsFactors = F)

habitat_class <- read.xlsx("C:\\Users\\framan\\OneDrive - NERC\\PoMS\\ModelsB_Abundance\\FITcount_flower_habitat category_summary.xlsx",
                           sheet = "habitat both")

flower_class <- read.xlsx("C:\\Users\\framan\\OneDrive - NERC\\PoMS\\ModelsB_Abundance\\FITcount_flower_habitat category_summary.xlsx",
                           sheet = "flowers both")

str(FIT_public)

FIT_public$habitat <- tolower(FIT_public$habitat)
FIT_public$habitat_other_detail <- tolower(FIT_public$habitat_other_detail)

str(habitat_class)
names(habitat_class)[3] <- "habitat_class"
habitat_class$habitat <- tolower(habitat_class$habitat)
habitat_class <- distinct(habitat_class, habitat, .keep_all = TRUE)
#habitat_class <- habitat_class[,-1]

str(flower_class)
names(flower_class)[2] <- "flower_class"
flower_class <- distinct(flower_class)

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
         wind_speed = as.factor(wind_speed)) %>%
  left_join(flower_class, by = "target_flower_family") %>%
  left_join(habitat_class, by = "habitat") %>%
  left_join(habitat_class, by = c("habitat_other_detail" = "habitat")) %>%
  mutate(habitat_class = coalesce(habitat_class.x, habitat_class.y)) %>%
  select(-one_of(c("habitat_class.x", "habitat_class.y", "type.x", "type.y")))

str(FIT_public)

# some tests to make sure we have correclty classified all the observations
summary(as.factor(FIT_public$habitat_class))
# all obs are classified as either U, A or N

summary(as.factor(FIT_public$flower_class))
# the classification failed for 2 observations
FIT_public[which(is.na(FIT_public$flower_class)), "target_flower_family"]
# one of these is missing the target_flower family and the other I think is a typo
# ignore for now


FIT_public_2018 <- subset(FIT_public, year == "2018")

# some observations do not have a grid reference in the sample_gridref column but lat and long
# this results into NAs when conb=verting to 1 Km precision
# ignore for now then ask Claire

FIT_public_2018 <- FIT_public_2018[!is.na(FIT_public_2018$site_1km),]

table(FIT_public_2018$country)
# very few observations for Northern Ireland and the Isle of Man
# exclude these for now and just fit the model to GB

FIT_public_2018_GB <- subset(FIT_public_2018, country %in% c("England", "Wales", "Scotland"))

str(FIT_public_2018_GB)

FIT_public_2018_GB$flower_class <- as.factor(FIT_public_2018_GB$flower_class)
FIT_public_2018_GB$habitat_class <- as.factor(FIT_public_2018_GB$habitat_class)

## Data exploration ----

JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = all_insects_total)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = all_insects_total)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()


abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = all_insects_total)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = all_insects_total)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

## GAMM for all insects ----

FIT_public_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country, k = 4) + flower_class +
                            floral_unit_count + flower_context + wind_speed + habitat_class,
                          family = poisson, random = ~(1|country/site_1km), 
                          data = FIT_public_2018_GB)

FIT_public_gamm_1$mer
summary(FIT_public_gamm_1$gam)

plot(FIT_public_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_gamm_1$gam, type = "deviance")

plot.gam(FIT_public_gamm_1$gam, all.terms = TRUE)


## Bumblebees ----

## Date exploration

Bumbleb_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = bumblebees)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

Bumbleb_JulDate_by_country

Bumbleb_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = bumblebees)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

Bumbleb_abund_by_wind

Bumbleb_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = bumblebees)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

Bumbleb_abund_by_flower

Bumbleb_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = bumblebees)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

Bumbleb_abund_by_context

## GAMM for bumblebees ----

FIT_public_Bumbleb_gamm_1 <- gamm4(bumblebees ~ s(JulDate, by = country) + flower_new +
                             floral_unit_count + flower_context + wind_speed,
                           family = poisson, random = ~(1|country/site_1km), 
                           data = FIT_public_2018_GB)

FIT_public_Bumbleb_gamm_1$mer
summary(FIT_public_Bumbleb_gamm_1$gam)

plot(FIT_public_Bumbleb_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_Bumbleb_gamm_1$gam, type = "deviance")


## Honeybees ----

honeyb_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = honeybees)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

honeyb_JulDate_by_country

honeyb_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = honeybees)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

honeyb_abund_by_wind

honeyb_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = honeybees)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

honeyb_abund_by_flower

honeyb_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = honeybees)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

honeyb_abund_by_context

## GAMM for honeybees ----

FIT_public_honeyb_gamm_1 <- gamm4(honeybees ~ s(JulDate, by = country) + flower_new +
                                     floral_unit_count + flower_context + wind_speed,
                                   family = poisson, random = ~(1|country/site_1km), 
                                   data = FIT_public_2018_GB)

FIT_public_honeyb_gamm_1$mer
summary(FIT_public_honeyb_gamm_1$gam)

plot(FIT_public_honeyb_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_honeyb_gamm_1$gam, type = "deviance")

# Clearly too many 0s!!!

FIT_public_honeyb_gam_2 <- gam(honeybees ~ s(JulDate, by = country) + flower_new +
                                    floral_unit_count + flower_context + wind_speed,
                                  family = ziP(), 
                                  data = FIT_public_2018_GB)

#FIT_public_honeyb_gamm_2$mer
#summary(FIT_public_honeyb_gamm_2$gam)

plot(FIT_public_honeyb_gam_2, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_honeyb_gam_2, type = "deviance")

# this looks a bit better but we don't have the random effect
# I couldn't find a way to fit a zero inflated mixed effects gam

## Solitary bees ----

solib_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = solitary_bees)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

solib_JulDate_by_country

solib_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = solitary_bees)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

solib_abund_by_wind

solib_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = solitary_bees)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

solib_abund_by_flower

solib_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = solitary_bees)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

solib_abund_by_context

## GAMM for solibees ----

FIT_public_solib_gamm_1 <- gamm4(solitary_bees ~ s(JulDate, by = country) + flower_new +
                                    floral_unit_count + flower_context + wind_speed,
                                  family = poisson, random = ~(1|country/site_1km), 
                                  data = FIT_public_2018_GB)

FIT_public_solib_gamm_1$mer
summary(FIT_public_solib_gamm_1$gam)

plot(FIT_public_solib_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_solib_gamm_1$gam, type = "deviance")

# Clearly too many 0s!!!

FIT_public_solib_gam_2 <- gam(solitary_bees ~ s(JulDate, by = country) + flower_new +
                                 floral_unit_count + flower_context + wind_speed,
                               family = ziP(), 
                               data = FIT_public_2018_GB)
summary(FIT_public_solib_gam_2)

plot(FIT_public_solib_gam_2, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_solib_gam_2, type = "deviance")


# scary warning!

## Wasps


wasps_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = wasps)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

wasps_JulDate_by_country

wasps_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = wasps)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

wasps_abund_by_wind

wasps_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = wasps)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

wasps_abund_by_flower

wasps_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = wasps)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

wasps_abund_by_context

## GAMM for solibees ----

FIT_public_wasps_gamm_1 <- gamm4(wasps ~ s(JulDate, by = country) + flower_new +
                                   floral_unit_count + flower_context + wind_speed,
                                 family = poisson, random = ~(1|country/site_1km), 
                                 data = FIT_public_2018_GB)

FIT_public_wasps_gamm_1$mer
summary(FIT_public_wasps_gamm_1$gam)

plot(FIT_public_wasps_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_wasps_gamm_1$gam, type = "deviance")

# Clearly too many 0s!!!

FIT_public_wasps_gam_2 <- gam(wasps ~ s(JulDate, by = country) + flower_new +
                                floral_unit_count + flower_context + wind_speed,
                              family = ziP(), 
                              data = FIT_public_2018_GB)
summary(FIT_public_wasps_gam_2)

plot(FIT_public_wasps_gam_2, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_wasps_gam_2, type = "deviance")

# scary warning again!

## Hoverflies

hoverflies_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = hoverflies)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

hoverflies_JulDate_by_country

hoverflies_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = hoverflies)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

hoverflies_abund_by_wind

hoverflies_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = hoverflies)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

hoverflies_abund_by_flower

hoverflies_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = hoverflies)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

hoverflies_abund_by_context

## GAMM for hoverflies ----

FIT_public_hoverflies_gamm_1 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_new +
                                   floral_unit_count + flower_context + wind_speed,
                                 family = poisson, random = ~(1|country/site_1km), 
                                 data = FIT_public_2018_GB)

FIT_public_hoverflies_gamm_1$mer
summary(FIT_public_hoverflies_gamm_1$gam)

plot(FIT_public_hoverflies_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_hoverflies_gamm_1$gam, type = "deviance")


## Flies ----

other_flies_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = other_flies)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

other_flies_JulDate_by_country

other_flies_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = other_flies)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

other_flies_abund_by_wind

other_flies_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = other_flies)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

other_flies_abund_by_flower

other_flies_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = other_flies)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

other_flies_abund_by_context

## GAMM for other_flies ----

FIT_public_other_flies_gamm_1 <- gamm4(other_flies ~ s(JulDate, by = country) + flower_new +
                                        floral_unit_count + flower_context + wind_speed,
                                      family = poisson, random = ~(1|country/site_1km), 
                                      data = FIT_public_2018_GB)

FIT_public_other_flies_gamm_1$mer
summary(FIT_public_other_flies_gamm_1$gam)

plot(FIT_public_other_flies_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_other_flies_gamm_1$gam, type = "deviance")

# convergence warning!

## Butterflies and moths


butterflies_moths_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = butterflies_moths)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

butterflies_moths_JulDate_by_country

butterflies_moths_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = butterflies_moths)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

butterflies_moths_abund_by_wind

butterflies_moths_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = butterflies_moths)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

butterflies_moths_abund_by_flower

butterflies_moths_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = butterflies_moths)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

butterflies_moths_abund_by_context

## GAMM for butterflies_moths ----

FIT_public_butterflies_moths_gamm_1 <- gamm4(butterflies_moths ~ s(JulDate, by = country) + flower_new +
                                         floral_unit_count + flower_context + wind_speed,
                                       family = poisson, random = ~(1|country/site_1km), 
                                       data = FIT_public_2018_GB)

FIT_public_butterflies_moths_gamm_1$mer
summary(FIT_public_butterflies_moths_gamm_1$gam)

plot(FIT_public_butterflies_moths_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_butterflies_moths_gamm_1$gam, type = "deviance")


## Beetles

beetles_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = beetles)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

beetles_JulDate_by_country

beetles_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = beetles)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

beetles_abund_by_wind

beetles_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = beetles)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

beetles_abund_by_flower

beetles_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = beetles)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

beetles_abund_by_context

## GAMM for beetles ----

FIT_public_beetles_gamm_1 <- gamm4(beetles ~ s(JulDate, by = country) + flower_new +
                                               floral_unit_count + flower_context + wind_speed,
                                             family = poisson, random = ~(1|country/site_1km), 
                                             data = FIT_public_2018_GB)

FIT_public_beetles_gamm_1$mer
summary(FIT_public_beetles_gamm_1$gam)

plot(FIT_public_beetles_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_beetles_gamm_1$gam, type = "deviance")


## Small insects

insects_small_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = insects_small)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

insects_small_JulDate_by_country

insects_small_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = insects_small)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

insects_small_abund_by_wind

insects_small_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = insects_small)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

insects_small_abund_by_flower

insects_small_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = insects_small)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

insects_small_abund_by_context

## GAMM for insects_small ----

FIT_public_insects_small_gamm_1 <- gamm4(insects_small ~ s(JulDate, by = country) + flower_new +
                                     floral_unit_count + flower_context + wind_speed,
                                   family = poisson, random = ~(1|country/site_1km), 
                                   data = FIT_public_2018_GB)

FIT_public_insects_small_gamm_1$mer
summary(FIT_public_insects_small_gamm_1$gam)

plot(FIT_public_insects_small_gamm_1$gam, pages = 1)


par(mfrow = c(2,2))
gam.check(FIT_public_insects_small_gamm_1$gam, type = "deviance")
