####################################################################
## Task 4.2 Models B - total abundance of pollinating insects
## Author: Francesca Mancini
## Date created: 2019-03-18
## Date modified: 2019-04-11
####################################################################


library(dplyr)
library(BRCmap)
library(tidyr)
library(gamm4)
library(ggplot2)
library(openxlsx)

## Public FIT counts ----

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
         flower_context = replace(flower_context, which(flower_context == "Not recorded"), NA),
         flower_context = factor(flower_context, order = TRUE,
                                 levels = c("More or less isolated", 
                                            "Growing in a larger patch of the same flower", 
                                            "Growing in a larger patch of many different flowers")),
         wind_speed = factor(wind_speed, order = TRUE, 
                             levels = c("Leaves still/moving occasionally",
                                        "Leaves moving gently all the time",
                                        "Leaves moving strongly"))) %>%
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


# write.csv(FIT_public_2018_GB,
#           file = "P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\Data processed\\FIT_public_2018_GB.csv",
#           row.names = FALSE)


FIT_public_2018_GB <- read.csv("P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\Data processed\\FIT_public_2018_GB.csv",
                              header = TRUE)
FIT_public_2018_GB$flower_context <- factor(FIT_public_2018_GB$flower_context,
                                           ordered = T, levels = c("More or less isolated",
                                                                   "Growing in a larger patch of the same flower",
                                                                  "Growing in a larger patch of many different flowers"))
FIT_public_2018_GB$wind_speed <- factor(FIT_public_2018_GB$wind_speed,
                                       ordered = T, levels = c("Leaves still/moving occasionally",
                                                               "Leaves moving gently all the time",
                                                               "Leaves moving strongly"))
FIT_public_2018_GB$sunshine <- factor(FIT_public_2018_GB$sunshine,
                                        ordered = T, levels = c("Entirely in sunshine",
                                                                "Partly in sun and partly shaded",
                                                                "Entirely shaded"))

FIT_public_2018_GB$flower_class <- as.factor(FIT_public_2018_GB$flower_class)

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

FIT_public_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_class +
                            floral_unit_count + flower_context + wind_speed + 
                            sunshine + habitat_class + country,
                            family = poisson, random = ~(1|country/site_1km), 
                            data = FIT_public_2018_GB)

FIT_public_gamm_1$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_gamm_1$gam)


FIT_public_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + flower_class +
                             floral_unit_count + flower_context + wind_speed + 
                             sunshine + habitat_class + country,
                           family = poisson, random = ~(1|country/site_1km), 
                           data = FIT_public_2018_GB)

FIT_public_gamm_2$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_gamm_2$gam)

AIC(FIT_public_gamm_1$mer)
AIC(FIT_public_gamm_2$mer)

anova(FIT_public_gamm_1$gam)


FIT_public_gamm_3 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_class +
                             floral_unit_count + flower_context +  
                             sunshine + habitat_class + country,
                           family = poisson, random = ~(1|country/site_1km), 
                           data = FIT_public_2018_GB)


FIT_public_gamm_3$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_gamm_3$gam)

anova(FIT_public_gamm_3$gam)

par(mfrow = c(2,2))
gam.check(FIT_public_gamm_3$gam, type = "deviance")

plot.gam(FIT_public_gamm_3$gam, all.terms = TRUE)

library(mgcViz)
all_insects_gamm1_viz <- getViz(FIT_public_gamm_1$gam)

all_insects_smoother_En <- plot(sm(all_insects_gamm1_viz, 1))
all_insects_smoother_Sc <- plot(sm(all_insects_gamm1_viz, 2))
all_insects_smoother_Wa <- plot(sm(all_insects_gamm1_viz, 3))



all_insects_smoother_En + 
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


all_insects_smoother_Sc + 
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

all_insects_smoother_Wa + 
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()


print(plot(all_insects_gamm1_viz, allTerms = T), pages = 1) 

## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_public_2018_GB <- FIT_public_2018_GB %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


bee_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = all_bees)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

bee_JulDate_by_country

bee_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = wind_speed, y = all_bees)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

bee_abund_by_wind

bee_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_new, y = all_bees)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

bee_abund_by_flower

bee_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
  geom_boxplot(aes(x = flower_context, y = all_bees)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

bee_abund_by_context

## GAMM for all_bees ----

FIT_public_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                                floral_unit_count + flower_context + wind_speed + 
                                 sunshine + habitat_class + country,
                                family = poisson, random = ~(1|country/site_1km), 
                                data = FIT_public_2018_GB)


FIT_public_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + flower_class +
                                  floral_unit_count + flower_context + wind_speed + 
                                  sunshine + habitat_class + country,
                                  family = poisson, random = ~(1|country/site_1km), 
                                  data = FIT_public_2018_GB)

AIC(FIT_public_bee_gamm_1$mer, FIT_public_bee_gamm_2$mer)


anova(FIT_public_bee_gamm_2$gam)

FIT_public_bee_gamm_3 <- gamm4(all_bees ~ s(JulDate) + flower_context +  
                                 sunshine + habitat_class + country,
                               family = poisson, random = ~(1|country/site_1km), 
                               data = FIT_public_2018_GB)

anova(FIT_public_bee_gamm_3$gam)


par(mfrow = c(2,2))
gam.check(FIT_public_bee_gamm_3$gam, type = "deviance")

FIT_public_bee_gamm_3$mer
summary(FIT_public_bee_gamm_3$gam)

plot(FIT_public_bee_gamm_3$gam, pages = 1)

plot.gam(FIT_public_bee_gamm_3$gam, all.terms = TRUE, pages = 1)



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

FIT_public_hoverflies_gamm <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                      floral_unit_count + flower_context + wind_speed + 
                                      sunshine + habitat_class + country,
                                      family = poisson, random = ~(1|country/site_1km), 
                                      data = FIT_public_2018_GB)

FIT_public_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + flower_class +
                                      floral_unit_count + flower_context + wind_speed + 
                                      sunshine + habitat_class + country,
                                    family = poisson, random = ~(1|country/site_1km), 
                                    data = FIT_public_2018_GB)

AIC(FIT_public_hoverflies_gamm$mer, FIT_public_hoverflies_gamm_2$mer)

anova(FIT_public_hoverflies_gamm$gam)

FIT_public_hoverflies_gamm_3 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                        floral_unit_count + flower_context + wind_speed,
                                      family = poisson, random = ~(1|country/site_1km), 
                                      data = FIT_public_2018_GB)



FIT_public_hoverflies_gamm_3$mer
summary(FIT_public_hoverflies_gamm_3$gam)

par(mfrow = c(2,2))
gam.check(FIT_public_hoverflies_gamm_3$gam, type = "deviance")

plot.gam(FIT_public_hoverflies_gamm_3$gam, all.terms = TRUE, pages = 1)

# ## Bumblebees ----
# 
# ## Date exploration
# 
# Bumbleb_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = bumblebees)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# Bumbleb_JulDate_by_country
# 
# Bumbleb_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = bumblebees)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# Bumbleb_abund_by_wind
# 
# Bumbleb_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = bumblebees)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# Bumbleb_abund_by_flower
# 
# Bumbleb_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = bumblebees)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# Bumbleb_abund_by_context
# 
# ## GAMM for bumblebees ----
# 
# FIT_public_Bumbleb_gamm_1 <- gamm4(bumblebees ~ s(JulDate, by = country) + flower_class +
#                                      floral_unit_count + flower_context + wind_speed + habitat_class,
#                                    family = poisson, random = ~(1|country/site_1km), 
#                                    data = FIT_public_2018_GB)
# 
# FIT_public_Bumbleb_gamm_1$mer
# summary(FIT_public_Bumbleb_gamm_1$gam)
# 
# plot(FIT_public_Bumbleb_gamm_1$gam, pages = 1)
# 
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_Bumbleb_gamm_1$gam, type = "deviance")
# 
# 
# FIT_public_Bumbleb_gamm_2 <- gamm4(bumblebees ~ s(JulDate) + flower_class +
#                                      floral_unit_count + flower_context + wind_speed + habitat_class,
#                                    family = poisson, random = ~(1|country/site_1km), 
#                                    data = FIT_public_2018_GB)
# 
# FIT_public_Bumbleb_gamm_2$mer
# summary(FIT_public_Bumbleb_gamm_2$gam)
# 
# plot(FIT_public_Bumbleb_gamm_2$gam, pages = 1)
# 
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_Bumbleb_gamm_2$gam, type = "deviance")
# 
# plot.gam(FIT_public_Bumbleb_gamm_2$gam, all.terms = TRUE)
# 
# ## Honeybees ----
# 
# honeyb_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = honeybees)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# honeyb_JulDate_by_country
# 
# honeyb_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = honeybees)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# honeyb_abund_by_wind
# 
# honeyb_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = honeybees)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# honeyb_abund_by_flower
# 
# honeyb_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = honeybees)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# honeyb_abund_by_context
# 
# ## GAMM for honeybees ----
# 
# FIT_public_honeyb_gamm_1 <- gamm4(honeybees ~ s(JulDate, by = country) + flower_class +
#                                     floral_unit_count + flower_context + wind_speed + habitat_class,
#                                   family = poisson, random = ~(1|country/site_1km), 
#                                   data = FIT_public_2018_GB)
# 
# FIT_public_honeyb_gamm_1$mer
# summary(FIT_public_honeyb_gamm_1$gam)
# 
# plot(FIT_public_honeyb_gamm_1$gam, pages = 1)
# 
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_honeyb_gamm_1$gam, type = "deviance")
# 
# # Clearly too many 0s!!!
# 
# FIT_public_honeyb_gam_2 <- gam(honeybees ~ s(JulDate, by = country) + flower_new +
#                                  floral_unit_count + flower_context + wind_speed,
#                                family = ziP(), 
#                                data = FIT_public_2018_GB)
# 
# #FIT_public_honeyb_gamm_2$mer
# #summary(FIT_public_honeyb_gamm_2$gam)
# 
# plot(FIT_public_honeyb_gam_2, pages = 1)
# 
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_honeyb_gam_2, type = "deviance")
# 
# plot.gam(FIT_public_honeyb_gam_2, all.terms = TRUE)
# 
# # this looks a bit better but we don't have the random effect
# # I couldn't find a way to fit a zero inflated mixed effects gam
# 
# ## Solitary bees ----
# 
# solib_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = solitary_bees)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# solib_JulDate_by_country
# 
# solib_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = solitary_bees)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# solib_abund_by_wind
# 
# solib_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = solitary_bees)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# solib_abund_by_flower
# 
# solib_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = solitary_bees)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# solib_abund_by_context
# 
# ## GAMM for solibees ----
# 
# FIT_public_solib_gamm_1 <- gamm4(solitary_bees ~ s(JulDate, by = country) + flower_class +
#                                    floral_unit_count + flower_context + wind_speed + habitat_class,
#                                  family = poisson, random = ~(1|country/site_1km), 
#                                  data = FIT_public_2018_GB)
# 
# FIT_public_solib_gamm_1$mer
# summary(FIT_public_solib_gamm_1$gam)
# 
# plot(FIT_public_solib_gamm_1$gam, pages = 1)
# 
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_solib_gamm_1$gam, type = "deviance")
# 
# # Clearly too many 0s!!!
# FIT_public_solib_gam <- gam(solitary_bees ~ s(JulDate, by = country) + flower_class +
#                               floral_unit_count + flower_context + wind_speed + habitat_class,
#                             family = ziP(), data = FIT_public_2018_GB)
# 
# summary(FIT_public_solib_gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_solib_gam, type = "deviance")
# 
# 
# 
# # scary warning!
# 
# ## Wasps
# 
# 
# wasps_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = wasps)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# wasps_JulDate_by_country
# 
# wasps_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = wasps)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# wasps_abund_by_wind
# 
# wasps_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = wasps)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# wasps_abund_by_flower
# 
# wasps_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = wasps)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# wasps_abund_by_context
# 
# ## GAMM for wasps ----
# 
# FIT_public_wasps_gamm <- gamm4(wasps ~ s(JulDate, by = country) + flower_class +
#                                  floral_unit_count + flower_context + wind_speed + habitat_class,
#                                family = poisson, random = ~(1|country/site_1km), 
#                                data = FIT_public_2018_GB)
# 
# FIT_public_wasps_gamm$mer
# summary(FIT_public_wasps_gamm$gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_wasps_gamm$gam, type = "deviance")
# 
# plot.gam(FIT_public_wasps_gamm$gam, all.terms = TRUE)
# 
# # Clearly too many 0s!!!
# 
# FIT_public_wasps_gam <- gam(wasps ~ s(JulDate, by = country) + flower_class +
#                               floral_unit_count + flower_context + wind_speed + habitat_class,
#                             family = ziP(), data = FIT_public_2018_GB)
# summary(FIT_public_wasps_gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_wasps_gam, type = "deviance")
# 
# 
# # scary warning again!
# 
# 
# ## Flies ----
# 
# other_flies_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = other_flies)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# other_flies_JulDate_by_country
# 
# other_flies_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = other_flies)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# other_flies_abund_by_wind
# 
# other_flies_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = other_flies)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# other_flies_abund_by_flower
# 
# other_flies_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = other_flies)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# other_flies_abund_by_context
# 
# ## GAMM for other_flies ----
# 
# FIT_public_other_flies_gamm <- gamm4(other_flies ~ s(JulDate, by = country) + flower_class +
#                                        floral_unit_count + flower_context + wind_speed + habitat_class,
#                                      family = poisson, random = ~(1|country/site_1km), 
#                                      data = FIT_public_2018_GB)
# 
# FIT_public_other_flies_gamm$mer
# summary(FIT_public_other_flies_gamm$gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_other_flies_gamm$gam, type = "deviance")
# 
# plot.gam(FIT_public_other_flies_gamm$gam, all.terms = TRUE)
# 
# # convergence warning!
# 
# ## Butterflies and moths
# 
# 
# butterflies_moths_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = butterflies_moths)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# butterflies_moths_JulDate_by_country
# 
# butterflies_moths_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = butterflies_moths)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# butterflies_moths_abund_by_wind
# 
# butterflies_moths_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = butterflies_moths)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# butterflies_moths_abund_by_flower
# 
# butterflies_moths_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = butterflies_moths)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# butterflies_moths_abund_by_context
# 
# ## GAMM for butterflies_moths ----
# 
# 
# FIT_public_butterflies_moths_gamm <- gamm4(butterflies_moths ~ s(JulDate, by = country) + flower_class +
#                                              floral_unit_count + flower_context + wind_speed + habitat_class,
#                                            family = poisson, random = ~(1|country/site_1km), 
#                                            data = FIT_public_2018_GB)
# 
# FIT_public_butterflies_moths_gamm$mer
# summary(FIT_public_butterflies_moths_gamm$gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_butterflies_moths_gamm$gam, type = "deviance")
# 
# plot.gam(FIT_public_butterflies_moths_gamm$gam, all.terms = TRUE)
# 
# 
# ## Beetles
# 
# beetles_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = beetles)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# beetles_JulDate_by_country
# 
# beetles_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = beetles)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# beetles_abund_by_wind
# 
# beetles_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = beetles)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# beetles_abund_by_flower
# 
# beetles_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = beetles)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# beetles_abund_by_context
# 
# ## GAMM for beetles ----
# 
# FIT_public_beetles_gamm <- gamm4(beetles ~ s(JulDate, by = country) + flower_class +
#                                    floral_unit_count + flower_context + wind_speed + habitat_class,
#                                  family = poisson, random = ~(1|country/site_1km), 
#                                  data = FIT_public_2018_GB)
# 
# FIT_public_beetles_gamm$mer
# summary(FIT_public_beetles_gamm$gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_beetles_gamm$gam, type = "deviance")
# 
# plot.gam(FIT_public_beetles_gamm$gam, all.terms = TRUE)
# 
# ## Small insects
# 
# insects_small_JulDate_by_country <- ggplot(data = FIT_public_2018_GB, aes(x = JulDate, y = insects_small)) +
#   geom_point(color = "goldenrod") +
#   geom_smooth(color = "darkblue") +
#   facet_wrap(~country) +
#   xlab("Julian date") +
#   ylab("All insects abundance") +
#   theme_bw()
# 
# insects_small_JulDate_by_country
# 
# insects_small_abund_by_wind <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = wind_speed, y = insects_small)) +
#   xlab("Wind speed") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# insects_small_abund_by_wind
# 
# insects_small_abund_by_flower <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_new, y = insects_small)) +
#   xlab("Flower type") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme_bw()
# 
# insects_small_abund_by_flower
# 
# insects_small_abund_by_context <- ggplot(data = FIT_public_2018_GB) +
#   geom_boxplot(aes(x = flower_context, y = insects_small)) +
#   xlab("Flower context") +
#   ylab("All insects abundance") +
#   facet_wrap(~country) +
#   theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
#         panel.background = element_blank())
# 
# insects_small_abund_by_context
# 
# ## GAMM for insects_small ----
# 
# FIT_public_insects_small_gamm <- gamm4(insects_small ~ s(JulDate, by = country) + flower_class +
#                                          floral_unit_count + flower_context + wind_speed + habitat_class,
#                                        family = poisson, random = ~(1|country/site_1km), 
#                                        data = FIT_public_2018_GB)
# 
# FIT_public_insects_small_gamm$mer
# summary(FIT_public_insects_small_gamm$gam)
# 
# par(mfrow = c(2,2))
# gam.check(FIT_public_insects_small_gamm$gam, type = "deviance")
# 
# plot.gam(FIT_public_insects_small_gamm$gam, all.terms = TRUE)
# 


## 1 Km FIT counts ----

FIT_1Km <- read.csv("P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\data outputs current versions\\tblEXPORT_1kmFITCount.csv",
                    header = T, stringsAsFactors = FALSE)


str(FIT_1Km)

FIT_1Km$habitat <- tolower(FIT_1Km$habitat)
FIT_1Km$habitat_other_detail <- tolower(FIT_1Km$habitat_other_detail)

str(habitat_class)
names(habitat_class)[3] <- "habitat_class"
habitat_class$habitat <- tolower(habitat_class$habitat)
habitat_class <- distinct(habitat_class, habitat, .keep_all = TRUE)
#habitat_class <- habitat_class[,-1]

str(flower_class)
names(flower_class)[2] <- "flower_class"
flower_class <- distinct(flower_class)

# change NAs to 0s
FIT_1Km <- FIT_1Km %>%
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

FIT_1Km <- FIT_1Km %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y %H:%M"),
         JulDate = as.numeric(format(date, "%j")),
         year = as.factor(format(date,"%Y")),
         site_1km = as.factor(reformat_gr(FIT_1Km$sample_gridref, prec_out = 1000)),
         site_1km_num = as.numeric(site_1km),
         country = as.factor(country),
         flower_new = as.factor(case_when(target_flower_corrected == "Other - please describe below" ~ "Other",
                                          TRUE ~ "Target")),
         flower_context = replace(flower_context, which(flower_context == "Not recorded"), NA),
         flower_context = factor(flower_context, order = TRUE,
                                 levels = c("More or less isolated", 
                                            "Growing in a larger patch of the same flower", 
                                            "Growing in a larger patch of many different flowers")),
         wind_speed = factor(wind_speed, order = TRUE, 
                             levels = c("Leaves still/moving occasionally",
                                        "Leaves moving gently all the time",
                                        "Leaves moving strongly"))) %>%
  left_join(flower_class, by = "target_flower_family") %>%
  left_join(habitat_class, by = "habitat") %>%
  left_join(habitat_class, by = c("habitat_other_detail" = "habitat")) %>%
  mutate(habitat_class = coalesce(habitat_class.x, habitat_class.y)) %>%
  select(-one_of(c("habitat_class.x", "habitat_class.y", "type.x", "type.y")))

str(FIT_1Km)

# some tests to make sure we have correclty classified all the observations
summary(as.factor(FIT_1Km$habitat_class))
# all obs are classified as either U, A or N

summary(as.factor(FIT_1Km$flower_class))


table(FIT_1Km$country)

FIT_1Km$flower_class <- as.factor(FIT_1Km$flower_class)
FIT_1Km$habitat_class <- as.factor(FIT_1Km$habitat_class)


write.csv(FIT_1Km,
         file = "P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\Data processed\\FIT_1Km_processed.csv",
         row.names = FALSE)


FIT_1Km <- read.csv("P:\\NEC06214_UK Pollinator Monitoring and Research Partnership\\Data and analysis\\Data processed\\FIT_1Km_processed.csv",
                    header = TRUE)

FIT_1Km$flower_context <- factor(FIT_1Km$flower_context,
                                 ordered = T, levels = c("More or less isolated",
                                                         "Growing in a larger patch of the same flower",
                                                         "Growing in a larger patch of many different flowers"))
FIT_1Km$wind_speed <- factor(FIT_1Km$wind_speed,
                             ordered = T, levels = c("Leaves still/moving occasionally",
                                                     "Leaves moving gently all the time",
                                                     "Leaves moving strongly"))

FIT_1Km$sunshine <- factor(FIT_1Km$sunshine,
                             ordered = T, levels = c("Entirely in sunshine",
                                                     "Partly in sun and partly shaded",
                                                     "Entirely shaded"))


FIT_1Km$flower_class <- as.factor(FIT_1Km$flower_class)

FIT_1Km$year <- as.factor(FIT_1Km$year)


# ## Data exploration ----

JulDate_by_country <- ggplot(data = FIT_1Km, aes(x = JulDate, y = all_insects_total)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country + year) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

JulDate_by_country

abund_by_wind <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = wind_speed, y = all_insects_total)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

abund_by_wind

abund_by_flower <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_new, y = all_insects_total)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

abund_by_flower

abund_by_context <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_context, y = all_insects_total)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

abund_by_context

## GAMM for all insects ----

FIT_1Km_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_class +
                        floral_unit_count + flower_context + wind_speed + 
                        sunshine + habitat_class + country + year, 
                        family = poisson, random = ~(1|country/site_1km), data = FIT_1Km)

plot(FIT_1Km_gamm_1$gam, pages = 1)

FIT_1Km_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + flower_class +
                        floral_unit_count + flower_context + wind_speed + 
                        sunshine + habitat_class + country + year, 
                        family = poisson, random = ~(1|country/site_1km), data = FIT_1Km)

AIC(FIT_1Km_gamm_1$mer, FIT_1Km_gamm_2$mer)


anova(FIT_1Km_gamm_1$gam)


FIT_1Km_gamm_1$mer
summary(FIT_1Km_gamm_1$gam)


par(mfrow = c(2,2))
gam.check(FIT_1Km_gamm_1$gam, type = "deviance")

plot.gam(FIT_1Km_gamm_1$gam, all.terms = TRUE, pages = 1)




## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_1Km <- FIT_1Km %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


bee_JulDate_by_country <- ggplot(data = FIT_1Km, aes(x = JulDate, y = all_bees)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

bee_JulDate_by_country

bee_abund_by_wind <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = wind_speed, y = all_bees)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

bee_abund_by_wind

bee_abund_by_flower <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_new, y = all_bees)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

bee_abund_by_flower

bee_abund_by_context <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_context, y = all_bees)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

bee_abund_by_context

## GAMM for all_bees ----

FIT_1Km_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                            floral_unit_count + flower_context + wind_speed + 
                            sunshine + habitat_class + country + year, 
                            family = poisson, random = ~(1|country/site_1km), data = FIT_1Km)

plot(FIT_1Km_bee_gamm_1$gam, pages = 1)

FIT_1Km_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + flower_class +
                              floral_unit_count + flower_context + wind_speed + 
                              sunshine + habitat_class + country + year, 
                            family = poisson, random = ~(1|country/site_1km), data = FIT_1Km)

AIC(FIT_1Km_bee_gamm_1$mer, FIT_1Km_bee_gamm_2$mer)

anova(FIT_1Km_bee_gamm_1$gam)

FIT_1Km_bee_gamm_3 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                            flower_context + wind_speed + habitat_class + country, 
                            family = poisson, random = ~(1|country/site_1km), data = FIT_1Km)



FIT_1Km_bee_gamm_3$mer
summary(FIT_1Km_bee_gamm_3$gam)



par(mfrow = c(2,2))
gam.check(FIT_1Km_bee_gamm_3$gam, type = "deviance")

plot.gam(FIT_1Km_bee_gamm_3$gam, all.terms = T, pages = 1)

## Hoverflies ----

hoverflies_JulDate_by_country <- ggplot(data = FIT_1Km, aes(x = JulDate, y = hoverflies)) +
  geom_point(color = "goldenrod") +
  geom_smooth(color = "darkblue") +
  facet_wrap(~country) +
  xlab("Julian date") +
  ylab("All insects abundance") +
  theme_bw()

hoverflies_JulDate_by_country

hoverflies_abund_by_wind <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = wind_speed, y = hoverflies)) +
  xlab("Wind speed") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

hoverflies_abund_by_wind

hoverflies_abund_by_flower <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_class, y = hoverflies)) +
  xlab("Flower type") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme_bw()

hoverflies_abund_by_flower

hoverflies_abund_by_context <- ggplot(data = FIT_1Km) +
  geom_boxplot(aes(x = flower_context, y = hoverflies)) +
  xlab("Flower context") +
  ylab("All insects abundance") +
  facet_wrap(~country) +
  theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1),
        panel.background = element_blank())

hoverflies_abund_by_context

## GAMM for hoverflies ----

FIT_1Km_hoverflies_gamm <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                 floral_unit_count + flower_context + wind_speed + 
                                 sunshine + habitat_class + country + year, 
                                 family = poisson, random = ~(1|country/site_1km), 
                                 data = FIT_1Km)

plot(FIT_1Km_hoverflies_gamm$gam, pages = 1)

FIT_1Km_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + flower_class +
                                   floral_unit_count + flower_context + wind_speed + 
                                   sunshine + habitat_class + country + year, 
                                   family = poisson, random = ~(1|country/site_1km), 
                                   data = FIT_1Km)

AIC(FIT_1Km_hoverflies_gamm$mer, FIT_1Km_hoverflies_gamm_2$mer)

anova(FIT_1Km_hoverflies_gamm$gam)

FIT_1Km_hoverflies_gamm$mer
summary(FIT_1Km_hoverflies_gamm$gam)

par(mfrow = c(2,2))
gam.check(FIT_1Km_hoverflies_gamm$gam, type = "deviance")

plot.gam(FIT_1Km_hoverflies_gamm$gam, all.terms = TRUE, pages = 1)



# The models fit to the two datasets seem to return very similar results.
# One effect seems to be consistently differen:
# In public FIT counts urban habitat usually has the highest number of insects
# while in the 1 Km FIT counts seminatural habitat is the habitat class with more insects.
# It is possible this result is an artifact of the number of counts conducted 
# in the different habitat classes in the two surveys.

Public_FIT_habitat <- ggplot(data = FIT_public_2018_GB) +
  geom_bar(aes(x = habitat_class), fill = "darkblue", show.legend=FALSE) +
  xlab("Habitat type") +
  ylab("Public FIT counts") #+
  # theme(axis.title.x=element_text(size=4),  # don't display x and y axes labels, titles and tickmarks
  #       axis.text.x=element_text(angle=50, vjust=1, hjust=1),
  #       axis.title.y=element_text(size=4),
  #       axis.ticks = element_line(size = 0.1),
  #       text=element_text(size=4),
  #       panel.background = element_blank())

OneKm_FIT_habitat <- ggplot(data = FIT_1Km) +
  geom_bar(aes(x = habitat_class, fill = year), position = "dodge", show.legend=TRUE) +
  scale_fill_manual(values = c("darkgoldenrod1", "darkblue"), guide = guide_legend(title = "Year")) +
  xlab("Habitat type") +
  ylab("1Km FIT counts") #+
  # theme(axis.title.x=element_text(size=4),  
  #       axis.text.x=element_text(angle=50, vjust=1, hjust=1),
  #       axis.title.y=element_text(size=4),
  #       axis.ticks = element_line(size = 0.1),
  #       text=element_text(size=4),
  #       panel.background = element_blank(),
  #       legend.text=element_text(size=4),
  #       legend.key.size = unit(0.2, "cm"))

source("../Descriptive Stats/multiplot.R")

multiplot(Public_FIT_habitat, OneKm_FIT_habitat)

# Indeed when plotting the number of counts conducted per habitat type,
# most public FIT counts were conducted in urban habitat, while
# most 1Km FIT counts were conducted in seminatural habitat.