####################################################################
## Task 4.2 Models B - total abundance of pollinating insects
## Author: Francesca Mancini
## Date created: 2019-03-18
## Date modified: 2019-04-16
####################################################################


library(dplyr)
library(BRCmap)
library(tidyr)
library(gamm4)
library(ggplot2)
library(openxlsx)
library(mgcViz)

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


## GAMM for all insects ----

FIT_public_allInsects_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + 
                                        flower_class + floral_unit_count + flower_context + 
                                        wind_speed + sunshine + habitat_class + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)

FIT_public_allInsects_gamm_1$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_allInsects_gamm_1$gam)


FIT_public_allInsects_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + flower_class +
                                        floral_unit_count + flower_context + wind_speed + 
                                        sunshine + habitat_class + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)


FIT_public_gamm_2$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_gamm_2$gam)

AIC(FIT_public_allInsects_gamm_1$mer, FIT_public_allInsects_gamm_2$mer)

anova(FIT_public_allInsects_gamm_1$gam)

FIT_public_allInsects_gamm_3 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_class +
                                        floral_unit_count + flower_context +  
                                        sunshine + habitat_class + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)


FIT_public_allInsects_gamm_3$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_allInsects_gamm_3$gam)


par(mfrow = c(2,2))
gam.check(FIT_public_allInsects_gamm_3$gam, type = "deviance")

png("./Plots/FIT_public_allInsects_smoothers.png", 
    width = 185, height = 150, unit = "mm", res = 300)
par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_public_allInsects_gamm_3$gam,  
         shift = FIT_public_allInsects_gamm_3$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Total insect count", shade = TRUE, shade.col = "grey",
         cex.lab = 1.5, cex.axis = 1.2)
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.2)
mtext("Scotland", side = 3, line = -2, outer = TRUE, adj = 0.55)
mtext("Wales", side = 3, line = -2, outer = TRUE, adj = 0.9)
dev.off()


all_insects_gamm_viz <- getViz(FIT_public_allInsects_gamm_3$gam)


all_insects_flower_class <- plot(pterm(all_insects_gamm_viz, 1))
all_insects_flower_count <- plot(pterm(all_insects_gamm_viz, 2))
all_insects_flower_context <- plot(pterm(all_insects_gamm_viz, 3))

all_insects_sunshine <- plot(pterm(all_insects_gamm_viz, 4))

all_insects_habitat <- plot(pterm(all_insects_gamm_viz, 5))


Flower_class <- all_insects_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Open", "Close")) +
  theme_classic()

png("./Plots/FIT_public_allInsects_flowerClass.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()
  
Flower_count <- all_insects_flower_count + 
  l_fitLine(colour = "darkgoldenrod2", size = 1) + 
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
  xlab("Floral unit count") +
  ylab("Effect on total insect count") +
  theme_classic() 

png("./Plots/FIT_public_allInsects_flowerCount.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_count
dev.off()


Flower_context <- all_insects_flower_context + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Flower context") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("More or less isolated", 
                              "Growing in a larger patch of the same flower",
                              "Growing in a larger patch of many different flowers"), 
                   labels = c("Isolated", 
                              "Large patch\n(same flower)", 
                              "Large patch\n(many flowers)")) +
  theme_classic() 

png("./Plots/FIT_public_allInsects_flowerContext.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()

Sunshine <- all_insects_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("Entirely in sunshine", 
                              "Entirely shaded", 
                              "Partly in sun and partly shaded"), 
                   labels = c("In sunshine", 
                              "Shaded", 
                              "Partly")) +
  theme_classic() 

png("./Plots/FIT_public_allInsects_sunshine.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()



Habitat <- all_insects_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("A", "N", "U"), 
                   labels = c("Agricultural", 
                              "Semi-natural", 
                              "Urban")) +
  theme_classic() 

png("./Plots/FIT_public_allInsects_habitat.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


png("./Plots/FIT_public_allInsects_parametric.png", 
    width = 180, height = 160, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_count, Flower_context,
          Sunshine, Habitat, ncol = 2, top = "")
dev.off()

## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_public_2018_GB <- FIT_public_2018_GB %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


## GAMM for all_bees ----

FIT_public_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                                floral_unit_count + flower_context + wind_speed + 
                                 sunshine + habitat_class + country,
                                family = poisson, random = ~(1|site_1km), 
                                data = FIT_public_2018_GB)


FIT_public_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + flower_class +
                                  floral_unit_count + flower_context + wind_speed + 
                                  sunshine + habitat_class + country,
                                  family = poisson, random = ~(1|site_1km), 
                                  data = FIT_public_2018_GB)


AIC(FIT_public_bee_gamm_1$mer, FIT_public_bee_gamm_2$mer)


anova(FIT_public_bee_gamm_2$gam)

FIT_public_bee_gamm_3 <- gamm4(all_bees ~ s(JulDate) + flower_context +  
                                 sunshine + habitat_class + country,
                                 family = poisson, random = ~(1|site_1km), 
                                 data = FIT_public_2018_GB)


par(mfrow = c(2,2))
gam.check(FIT_public_bee_gamm_3$gam, type = "deviance")

FIT_public_bee_gamm_3$mer
summary(FIT_public_bee_gamm_3$gam)



png("./Plots/FIT_public_bees_smoother.png", 
    width = 100, height = 80, unit = "mm", res = 300)
par(mar=c(5,5,1,1), oma=c(2,1,1,1), mgp = c(3,1,0))
plot.gam(FIT_public_bee_gamm_3$gam,  
         shift = FIT_public_bee_gamm_3$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Bee count", shade = TRUE, shade.col = "grey",
         cex.lab = 0.8, cex.axis = 0.5)
dev.off()


bees_gamm_viz <- getViz(FIT_public_bee_gamm_3$gam)


# bees_smoother <- plot(sm(bees_gamm_viz, 1))

bees_flower_context <- plot(pterm(bees_gamm_viz, 1))

bees_sunshine <- plot(pterm(bees_gamm_viz, 2))

bees_habitat <- plot(pterm(bees_gamm_viz, 3))

bees_country <- plot(pterm(bees_gamm_viz, 4))


Flower_context <- bees_flower_context + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower context") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("More or less isolated", 
                              "Growing in a larger patch of the same flower",
                              "Growing in a larger patch of many different flowers"), 
                   labels = c("Isolated", 
                              "Large patch\n(same flower)", 
                              "Large patch\n(many flowers)")) +
  theme_classic()

png("./Plots/FIT_public_bees_flowerContext.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


Sunshine <- bees_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("Entirely in sunshine", 
                              "Entirely shaded", 
                              "Partly in sun and partly shaded"), 
                   labels = c("In sunshine", 
                              "Shaded", 
                              "Partly")) +
  theme_classic()

png("./Plots/FIT_public_bees_Sunshine.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()



Habitat <- bees_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("A", "N", "U"), 
                   labels = c("Agricultural", 
                              "Semi-natural", 
                              "Urban")) +
  theme_classic()

png("./Plots/FIT_public_bees_Habitat.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


Country <- bees_country + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Country") +
  ylab("Effect on bee count") +
  theme_classic()

png("./Plots/FIT_public_bees_Country.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Country
dev.off()


png("./Plots/FIT_public_bees_parametric.png", 
    width = 180, height = 160, unit = "mm", res = 300)
gridPrint(Flower_context, Sunshine, Habitat, Country, ncol = 2)
dev.off()


## Hoverflies

## GAMM for hoverflies ----

FIT_public_hoverflies_gamm_1 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                        floral_unit_count + flower_context + wind_speed + 
                                        sunshine + habitat_class + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)


FIT_public_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + flower_class +
                                        floral_unit_count + flower_context + wind_speed + 
                                        sunshine + habitat_class + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)


AIC(FIT_public_hoverflies_gamm_1$mer, FIT_public_hoverflies_gamm_2$mer)

anova(FIT_public_hoverflies_gamm_1$gam)

FIT_public_hoverflies_gamm_3 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                        floral_unit_count + flower_context,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public_2018_GB)



FIT_public_hoverflies_gamm_3$mer
summary(FIT_public_hoverflies_gamm_3$gam)

par(mfrow = c(2,2))
gam.check(FIT_public_hoverflies_gamm_3$gam, type = "deviance")

png("./Plots/FIT_public_hoverflies_smoothers.png", 
    width = 185, height = 150, unit = "mm", res = 300)
par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_public_hoverflies_gamm_3$gam,  
         shift = FIT_public_allInsects_gamm_3$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Hoverfly count", shade = TRUE, shade.col = "grey",
         cex.lab = 1.5, cex.axis = 1.2)
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.2)
mtext("Scotland", side = 3, line = -2, outer = TRUE, adj = 0.55)
mtext("Wales", side = 3, line = -2, outer = TRUE, adj = 0.9)
dev.off()


hoverflies_gamm_viz <- getViz(FIT_public_hoverflies_gamm_3$gam)

hoverflies_flower_class <- plot(pterm(all_insects_gamm_viz, 1))
hoverflies_flower_count <- plot(pterm(all_insects_gamm_viz, 2))
hoverflies_flower_context <- plot(pterm(all_insects_gamm_viz, 3))



Flower_class <- hoverflies_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Open", "Close")) +
  theme_classic()

png("./Plots/FIT_public_hoverflies_flowerClass.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


Flower_count <- hoverflies_flower_count + 
  l_fitLine(colour = "darkgoldenrod2", size = 1) + 
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
  xlab("Floral unit count") +
  ylab("Effect on hoverfly count") +
  theme_classic()

png("./Plots/FIT_public_hoverflies_flowerCount.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_count
dev.off()


Flower_context <- hoverflies_flower_context + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower context") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("More or less isolated", 
                              "Growing in a larger patch of the same flower",
                              "Growing in a larger patch of many different flowers"), 
                   labels = c("Isolated", 
                              "Large patch\n(same flower)", 
                              "Large patch\n(many flowers)")) +
  theme_classic()

png("./Plots/FIT_public_hoverflies_flowerContext.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


png("./Plots/FIT_public_hoverflies_parametric.png", 
    width = 235, height = 80, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_count, Flower_context,ncol = 3)
dev.off()



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

## GAMM for all insects ----

FIT_1Km_allInsects_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_class +
                                    floral_unit_count + flower_context + wind_speed + 
                                    sunshine + habitat_class + country + year, 
                                    family = poisson, random = ~(1|site_1km), data = FIT_1Km)


FIT_1Km_allInsects_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + flower_class +
                                    floral_unit_count + flower_context + wind_speed + 
                                    sunshine + habitat_class + country + year, 
                                    family = poisson, random = ~(1|site_1km), data = FIT_1Km)


AIC(FIT_1Km_allInsects_gamm_1$mer, FIT_1Km_allInsects_gamm_2$mer)


anova(FIT_1Km_allInsects_gamm_1$gam)


FIT_1Km_allInsects_gamm_1$mer
summary(FIT_1Km_allInsects_gamm_1$gam)


par(mfrow = c(2,2))
gam.check(FIT_1Km_allInsects_gamm_1$gam, type = "deviance")

png("./Plots/FIT_1Km_allInsects_smoothers.png", 
    width = 190, height = 150, unit = "mm", res = 300)
par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_1Km_allInsects_gamm_1$gam,  
         shift = FIT_1Km_allInsects_gamm_1$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Total insect count", shade = TRUE, shade.col = "grey",
         cex.lab = 1.5, cex.axis = 1.2)
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.2)
mtext("Scotland", side = 3, line = -2, outer = TRUE, adj = 0.55)
mtext("Wales", side = 3, line = -2, outer = TRUE, adj = 0.9)
dev.off()


all_insects_1km_gamm_viz <- getViz(FIT_1Km_allInsects_gamm_1$gam)



all_insects_1km_flower_class <- plot(pterm(all_insects_1km_gamm_viz, 1))
all_insects_1km_flower_count <- plot(pterm(all_insects_1km_gamm_viz, 2))
all_insects_1km_flower_context <- plot(pterm(all_insects_1km_gamm_viz, 3))

all_insects_1km_wind <- plot(pterm(all_insects_1km_gamm_viz, 4))
all_insects_1km_sunshine <- plot(pterm(all_insects_1km_gamm_viz, 5))

all_insects_1km_habitat <- plot(pterm(all_insects_1km_gamm_viz, 6))

all_insects_1km_year <- plot(pterm(all_insects_1km_gamm_viz, 8))


Flower_class <- all_insects_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Open", "Close")) +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_flowerClass.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


Flower_count <- all_insects_1km_flower_count + 
  l_fitLine(colour = "darkgoldenrod2", size = 1) + 
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
  xlab("Floral unit count") +
  ylab("Effect on total insect count") +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_flowerCount.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_count
dev.off()


Flower_context <- all_insects_1km_flower_context + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower context") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("More or less isolated", 
                              "Growing in a larger patch of the same flower",
                              "Growing in a larger patch of many different flowers"), 
                   labels = c("Isolated", 
                              "Large patch\n(same flower)", 
                              "Large patch\n(many flowers)")) +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_flowerContext.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


Wind <- all_insects_1km_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"), 
                   labels = c("Low", 
                              "Medium", 
                              "High")) +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_Wind.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
dev.off()

Sunshine <- all_insects_1km_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("Entirely in sunshine", 
                              "Partly in sun and partly shaded",
                              "Entirely shaded"), 
                   labels = c("In sunshine", 
                              "Partly", 
                              "Shaded")) +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_Sunshine.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()


Habitat <- all_insects_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("A", "N", "U"), 
                   labels = c("Agricultural", 
                              "Semi-natural", 
                              "Urban")) +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_Habitat.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()



Year <- all_insects_1km_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on total insect count") +
  theme_classic()

png("./Plots/FIT_1Km_allInsects_Year.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


png("./Plots/FIT_1Km_allInsects_parametric.png", 
    width = 180, height = 200, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_count, Flower_context,
          Wind, Sunshine, Habitat, Year, ncol = 2, top = "")
dev.off()


## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_1Km <- FIT_1Km %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


## GAMM for all_bees ----

FIT_1Km_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                            floral_unit_count + flower_context + wind_speed + 
                            sunshine + habitat_class + country + year, 
                            family = poisson, random = ~(1|site_1km), data = FIT_1Km)


FIT_1Km_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + flower_class +
                              floral_unit_count + flower_context + wind_speed + 
                              sunshine + habitat_class + country + year, 
                            family = poisson, random = ~(1|site_1km), data = FIT_1Km)


AIC(FIT_1Km_bee_gamm_1$mer, FIT_1Km_bee_gamm_2$mer)

anova(FIT_1Km_bee_gamm_1$gam)

FIT_1Km_bee_gamm_3 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_class +
                            flower_context + wind_speed + habitat_class + country, 
                            family = poisson, random = ~(1|site_1km), data = FIT_1Km)



FIT_1Km_bee_gamm_3$mer
summary(FIT_1Km_bee_gamm_3$gam)



par(mfrow = c(2,2))
gam.check(FIT_1Km_bee_gamm_3$gam, type = "deviance")

png("./Plots/FIT_1Km_bees_smoothers.png", 
    width = 190, height = 150, unit = "mm", res = 300)
par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_1Km_bee_gamm_3$gam,  
         shift = FIT_1Km_bee_gamm_3$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Bee count", shade = TRUE, shade.col = "grey",
         cex.lab = 1.5, cex.axis = 1.2)
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.2)
mtext("Scotland", side = 3, line = -2, outer = TRUE, adj = 0.55)
mtext("Wales", side = 3, line = -2, outer = TRUE, adj = 0.9)
dev.off()


bees_1km_gamm_viz <- getViz(FIT_1Km_bee_gamm_3$gam)


bees_1km_flower_class <- plot(pterm(bees_1km_gamm_viz, 1))
bees_1km_flower_context <- plot(pterm(bees_1km_gamm_viz, 2))

bees_1km_wind <- plot(pterm(bees_1km_gamm_viz, 3))

bees_1km_habitat <- plot(pterm(bees_1km_gamm_viz, 4))




Flower_class <- bees_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Open", "Close")) +
  theme_classic()

png("./Plots/FIT_1Km_bees_flowerClass.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()

Flower_context <- bees_1km_flower_context + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower context") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("More or less isolated", 
                              "Growing in a larger patch of the same flower",
                              "Growing in a larger patch of many different flowers"), 
                   labels = c("Isolated", 
                              "Large patch\n(same flower)", 
                              "Large patch\n(many flowers)")) +
  theme_classic()

png("./Plots/FIT_1Km_bees_flowerContext.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


Wind <- bees_1km_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"), 
                   labels = c("Low", 
                              "Medium", 
                              "High")) +
  theme_classic()

png("./Plots/FIT_1Km_bees_wind.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
dev.off()



Habitat <- all_insects_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("A", "N", "U"), 
                   labels = c("Agricultural", 
                              "Semi-natural", 
                              "Urban")) +
  theme_classic()

png("./Plots/FIT_1Km_bees_Habitat.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()

png("./Plots/FIT_1Km_bees_parametric.png", 
    width = 180, height = 180, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_context,
          Wind, Habitat, ncol = 2, top = "")
dev.off()



## Hoverflies ----

## GAMM for hoverflies ----

FIT_1Km_hoverflies_gamm <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +
                                 floral_unit_count + flower_context + wind_speed + 
                                 sunshine + habitat_class + country + year, 
                                 family = poisson, random = ~(1|site_1km), 
                                 data = FIT_1Km)


FIT_1Km_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + flower_class +
                                   floral_unit_count + flower_context + wind_speed + 
                                   sunshine + habitat_class + country + year, 
                                   family = poisson, random = ~(1|site_1km), 
                                   data = FIT_1Km)


AIC(FIT_1Km_hoverflies_gamm$mer, FIT_1Km_hoverflies_gamm_2$mer)

anova(FIT_1Km_hoverflies_gamm$gam)

FIT_1Km_hoverflies_gamm_3 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_class +  
                                     sunshine + habitat_class + country + year, 
                                   family = poisson, random = ~(1|site_1km), 
                                   data = FIT_1Km)


FIT_1Km_hoverflies_gamm_3$mer
summary(FIT_1Km_hoverflies_gamm_3$gam)

par(mfrow = c(2,2))
gam.check(FIT_1Km_hoverflies_gamm_3$gam, type = "deviance")

png("./Plots/FIT_1Km_hoverflies_smoothers.png", 
    width = 190, height = 150, unit = "mm", res = 300)
par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_1Km_hoverflies_gamm_3$gam,  
         shift = FIT_1Km_hoverflies_gamm_3$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Hoverfly count", shade = TRUE, shade.col = "grey",
         cex.lab = 1.5, cex.axis = 1.2, ylim = c(0,50))
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.2)
mtext("Scotland", side = 3, line = -2, outer = TRUE, adj = 0.55)
mtext("Wales", side = 3, line = -2, outer = TRUE, adj = 0.9)
dev.off()

hover_1km_gamm_viz <- getViz(FIT_1Km_hoverflies_gamm_3$gam)


hover_1km_flower_class <- plot(pterm(hover_1km_gamm_viz, 1))

hover_1km_sunshine <- plot(pterm(hover_1km_gamm_viz, 2))

hover_1km_habitat <- plot(pterm(hover_1km_gamm_viz, 3))

hover_1km_year <- plot(pterm(hover_1km_gamm_viz, 5))


Flower_class <- hover_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("0", "1"), labels = c("Open", "Close")) +
  theme_classic()

png("./Plots/FIT_1Km_hoverflies_flowerClass.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


Sunshine <- hover_1km_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("Entirely in sunshine", 
                              "Partly in sun and partly shaded",
                              "Entirely shaded"), 
                   labels = c("In sunshine", 
                              "Partly", 
                              "Shaded")) +
  theme_classic()

png("./Plots/FIT_1Km_hoverflies_sunshine.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()


Habitat <- hover_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("A", "N", "U"), 
                   labels = c("Agricultural", 
                              "Semi-natural", 
                              "Urban")) +
  theme_classic()

png("./Plots/FIT_1Km_hoverflies_habitat.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


Year <- hover_1km_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on hoverfly count") +
  theme_classic()

png("./Plots/FIT_1Km_hoverflies_year.png", 
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


png("./Plots/FIT_1Km_hoverflies_parametric.png", 
    width = 180, height = 180, unit = "mm", res = 300)
gridPrint(Flower_class, Sunshine, 
          Habitat, Year, ncol = 2, top = "")
dev.off()


