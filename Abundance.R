####################################################################
## Task 4.2 Models B - total abundance of pollinating insects
## Author: Francesca Mancini
## Date created: 2019-03-18
## Date modified: 20121-03-03
####################################################################


library(dplyr)
library(BRCmap)
library(tidyr)
library(gamm4)
library(ggplot2)
# library(openxlsx)
library(mgcViz)

output_path <- "P:/NEC06214_UK Pollinator Monitoring and Research Partnership/Data and analysis/06 analysis/Analysis 2021 Francesca"

## Public FIT counts ----

# combined data 2017-2020

FIT_public <- read.csv("./FITp_combined.csv",
                       stringsAsFactors = FALSE)

FIT_public <- FIT_public %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y %H:%M"),
         JulDate = as.numeric(format(date, "%j")),
         site_1km = as.factor(reformat_gr(FIT_public$sample_gridref, prec_out = 1000)),
         site_1km_num = as.numeric(site_1km),
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
                                        "Leaves moving strongly")),
         sunshine = factor(sunshine, ordered = T, 
                           levels = c("Entirely in sunshine",
                                      "Partly in sun and partly shaded",
                                      "Entirely shaded"))) 

# some tests to make sure we have correclty classified all the observations
summary(as.factor(FIT_public$habitat_type))
# some observations have NA in habitat type because 
# neither main habitat nor habitat_other_detail was recorded

summary(as.factor(FIT_public$flower_structure))
# the classification failed for 2 observations
FIT_public[which(is.na(FIT_public$flower_structure)), 
           c("target_flower_corrected", "target_other_name_corrected","target_flower_family")]
# all the NAs in flower_structure are from observations where 
# target_other_name_corrected was either NA, unidentified, 
# not recorded, wildflowers or wildflower mix

 #exclude records from 2017
FIT_public <- subset(FIT_public, Year != "2017")

# some observations do not have a grid reference in the sample_gridref column but lat and long
# this results into NAs when conb=verting to 1 Km precision
# exclude them for now 

FIT_public <- FIT_public[!is.na(FIT_public$site_1km),]

table(FIT_public$country)
# aggregated Isle of Man obs to England

FIT_public <- FIT_public %>%
  mutate(country = case_when(country == "Isle of Man" ~ "England",
                             TRUE ~ country)) %>%
  mutate(country = as.factor(country),
         flower_structure = as.factor(flower_structure),
         habitat_type = as.factor(habitat_type),
         Year = as.factor(Year))





## GAMM for all insects ----

FIT_public_allInsects_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + Year +
                                        flower_structure + floral_unit_count + flower_context + 
                                        wind_speed + sunshine + habitat_type + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public)

FIT_public_allInsects_gamm_1$mer
# plot(ranef(FIT_public_allInsects_gamm_1$mer))
summary(FIT_public_allInsects_gamm_1$gam)


FIT_public_allInsects_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + Year + flower_structure +
                                        floral_unit_count + flower_context + wind_speed + 
                                        sunshine + habitat_type + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public)



AIC(FIT_public_allInsects_gamm_1$mer, FIT_public_allInsects_gamm_2$mer)

FIT_public_allInsects_gamm_2$mer
# plot(ranef(FIT_public_gamm_1$mer))
summary(FIT_public_allInsects_gamm_2$gam)
# all variables seem to have a significant effect on the number of insects counted

FIT_public_allInsects_gamm_final <- gamm4(all_insects_total ~ s(JulDate, by = country) + 
                                        Year + flower_structure +
                                        floral_unit_count + flower_context + wind_speed + 
                                        sunshine + habitat_type + country,
                                      family = poisson, random = ~(1|site_1km), 
                                      REML = TRUE, data = FIT_public)


FIT_public_allInsects_gamm_final$mer
# plot(ranef(FIT_public_gamm_1$mer))
sink(file.path(output_path, "FIT counts/FIT_public_allInsects_summary.txt"))
summary(FIT_public_allInsects_gamm_final$gam)
sink()

par(mfrow = c(2,2))
gam.check(FIT_public_allInsects_gamm_final$gam, type = "deviance")

png(file.path(output_path, "FIT counts/FIT_public_allInsects_smoothers.png"), 
    width = 155, height = 120, unit = "mm", res = 300)
par(mfrow = c(2,2)) #, mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_public_allInsects_gamm_final$gam,  
         shift = FIT_public_allInsects_gamm_final$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Total insect count", shade = TRUE, shade.col = "grey")
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.25, font = 2)
mtext("Northern Ireland", side = 3, line = -2, outer = TRUE, adj = 0.8, font = 2)
mtext("Scotland", side = 1, line = -13, outer = TRUE, adj = 0.25, font = 2)
mtext("Wales", side = 1, line = -13, outer = TRUE, adj = 0.75, font = 2)
dev.off()


all_insects_gamm_viz <- getViz(FIT_public_allInsects_gamm_final$gam)

all_insects_year <- plot(pterm(all_insects_gamm_viz, 1))
all_insects_flower_structure <- plot(pterm(all_insects_gamm_viz, 2))
all_insects_floral_count <- plot(pterm(all_insects_gamm_viz, 3))
all_insects_flower_context <- plot(pterm(all_insects_gamm_viz, 4))
all_insects_wind <- plot(pterm(all_insects_gamm_viz, 5))
all_insects_sunshine <- plot(pterm(all_insects_gamm_viz, 6))
all_insects_habitat <- plot(pterm(all_insects_gamm_viz, 7))


Year <- all_insects_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on total insect count") +
  theme_classic()

png(file.path(output_path, "Fit counts/FIT_public_allInsects_year.png"),
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


Flower_class <- all_insects_flower_structure + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(labels = c("Closed","Open")) +
  theme_classic()

# # welsh
# 
# Flower_class_welsh <- all_insects_flower_class + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
#   l_rug(alpha = 0.3) +
#   xlab("Math o flodyn") +
#   ylab("Effaith ar gyfanswm cyfrif pryfed") +
#   scale_x_discrete(breaks = c("0", "1"), labels = c("Agored", "Caeedig")) +
#   theme_classic()



png(file.path(output_path, "FIT counts/FIT_public_allInsects_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


  
Flower_count <- all_insects_floral_count +
  l_fitLine(colour = "darkgoldenrod2", size = 1) +
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) +
  xlab("Floral unit count") +
  ylab("Effect on total insect count") +
  theme_classic()

png(file.path(output_path, "Fit counts/FIT_public_allInsects_flowerCount.png"),
    width = 80, height = 80, unit = "mm", res = 300)
Flower_count
dev.off()


# # welsh
# 
# Flower_count_welsh <- all_insects_flower_count + 
#   l_fitLine(colour = "darkgoldenrod2", size = 1) + 
#   l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
#   xlab("Cyfrif uned flodeuol") +
#   ylab("Effaith ar gyfanswm cyfrif pryfed") +
#   theme_classic() 


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

png(file.path(output_path, "FIT counts/FIT_public_allInsects_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()

# # welsh
# 
# Flower_context_welsh <- all_insects_flower_context + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
#   l_rug(alpha = 0.3) +
#   xlab("Cyd-destun y blodyn") +
#   ylab("Effaith ar gyfanswm cyfrif pryfed") +
#   scale_x_discrete(breaks = c("More or less isolated", 
#                               "Growing in a larger patch of the same flower",
#                               "Growing in a larger patch of many different flowers"), 
#                    labels = c("Ar ei ben ei hun", 
#                               "Llain fawr\n(yr un blodyn)", 
#                               "Llain fawr\n(llawer o flodau)")) +
#   theme_classic() 
# 


Wind <- all_insects_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally",
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"),
                   labels = c("Leaves still",
                              "Leaves\nmoving\ngently",
                              "Leaves\nmoving\nstrongly")) +
  theme_classic() +
  theme(axis.text = element_text(size=9))

png(file.path(output_path, "FIT counts/FIT_public_allInsects_wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
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

png(file.path(output_path, "FIT counts/FIT_public_allInsects_sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()

# # welsh
# 
# Sunshine_welsh <- all_insects_sunshine + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
#   l_rug(alpha = 0.3) +
#   xlab("Heulwen yn ystod y cyfrif") +
#   ylab("Effaith ar gyfanswm cyfrif pryfed") +
#   scale_x_discrete(breaks = c("Entirely in sunshine", 
#                               "Entirely shaded", 
#                               "Partly in sun and partly shaded"), 
#                    labels = c("Yn yr heulwen", 
#                               "Yn y cysgod", 
#                               "Rhannol")) +
#   theme_classic() 


Habitat <- all_insects_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(labels = c("Agricultural",
                              "Gardens", 
                              "Semi-natural",
                              "Urban")) +
  theme_classic() 

png(file.path(output_path, "FIT counts/FIT_public_allInsects_habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()

# # welsh
# 
# Habitat_welsh <- all_insects_habitat + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o gynefin") +
#   ylab("Effaith ar gyfanswm cyfrif pryfed") +
#   scale_x_discrete(breaks = c("A", "N", "U"), 
#                    labels = c("Amaethyddol", 
#                               "Lled-naturiol", 
#                               "Trefol")) +
#   theme_classic() 
# 

png(file.path(output_path, "FIT counts/FIT_public_allInsects_parametric.png"), 
    width = 160, height = 220, unit = "mm", res = 300)
gridPrint(Year, Flower_class, Flower_count, Flower_context,
          Sunshine, Wind, Habitat, ncol = 2, top = "")
dev.off()

# # welsh plot
# 
# png("./Plots/FIT_public_allInsects_parametric_welsh.png", 
#     width = 180, height = 200, unit = "mm", res = 300)
# gridPrint(Flower_class_welsh, Flower_count_welsh, Flower_context_welsh,
#           Sunshine_welsh, Habitat_welsh, ncol = 2, top = "")
# dev.off()


## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_public <- FIT_public %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


## GAMM for all_bees ----

FIT_public_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + Year + 
                                 flower_structure + floral_unit_count + 
                                 flower_context + wind_speed + 
                                 sunshine + habitat_type + country,
                                family = poisson, random = ~(1|site_1km), 
                                data = FIT_public)


FIT_public_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + Year + 
                               flower_structure + floral_unit_count + 
                               flower_context + wind_speed + 
                               sunshine + habitat_type + country,
                               family = poisson, random = ~(1|site_1km), 
                               data = FIT_public)


AIC(FIT_public_bee_gamm_1$mer, FIT_public_bee_gamm_2$mer)


summary(FIT_public_bee_gamm_2$gam)

par(mfrow = c(2,2))
gam.check(FIT_public_bee_gamm_2$gam, type = "deviance")

FIT_public_bee_gamm_2$mer

sink(file.path(output_path, "FIT counts/FIT_public_bees_summary.txt"))
summary(FIT_public_bee_gamm_2$gam)
sink()


png(file.path(output_path, "FIT counts/FIT_public_bees_smoother.png"), 
    width = 100, height = 80, unit = "mm", res = 300)
par(mar=c(5,5,1,1), oma=c(2,1,1,1), mgp = c(3,1,0))
plot.gam(FIT_public_bee_gamm_2$gam,  
         shift = FIT_public_bee_gamm_2$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Bee count", shade = TRUE, shade.col = "grey",
         cex.lab = 0.8, cex.axis = 0.5)
dev.off()


bees_gamm_viz <- getViz(FIT_public_bee_gamm_2$gam)

bees_year <- plot(pterm(bees_gamm_viz, 1))
bees_flower_class <- plot(pterm(bees_gamm_viz, 2))
bees_floral_count <- plot(pterm(bees_gamm_viz, 3))
bees_flower_context <- plot(pterm(bees_gamm_viz, 4))
bees_wind <- plot(pterm(bees_gamm_viz, 5))
bees_sunshine <- plot(pterm(bees_gamm_viz, 6))
bees_habitat <- plot(pterm(bees_gamm_viz, 7))
bees_country <- plot(pterm(bees_gamm_viz, 8))

bees_Year <- bees_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on bee count") +
  theme_classic()

png(file.path(output_path, "Fit counts/FIT_public_bees_year.png"),
    width = 80, height = 80, unit = "mm", res = 300)
bees_Year
dev.off()


bees_flower_class_plot <- bees_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower class") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("open", "closed"), 
                   labels = c("Open","Closed")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_bees_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_flower_class_plot
dev.off()


bees_flower_count_plot <- bees_floral_count +
  l_fitLine(colour = "darkgoldenrod2", size = 1) +
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) +
  xlab("Floral unit count") +
  ylab("Effect on bee count") +
  theme_classic()

png(file.path(output_path, "Fit counts/FIT_public_bees_flowerCount.png"),
    width = 80, height = 80, unit = "mm", res = 300)
bees_flower_count_plot
dev.off()




bees_flower_context_plot <- bees_flower_context + 
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

png(file.path(output_path, "FIT counts/FIT_public_bees_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_flower_context_plot
dev.off()


# # welsh
# 
# Flower_context_welsh <- bees_flower_context + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyd-destun y blodyn") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("More or less isolated", 
#                               "Growing in a larger patch of the same flower",
#                               "Growing in a larger patch of many different flowers"), 
#                    labels = c("Ar ei ben ei hun", 
#                               "Llain fawr\n(yr un blodyn)", 
#                               "Llain fawr\n(llawer o flodau)")) +
#   theme_classic()
# 


bees_wind_plot <- bees_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally",
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"),
                   labels = c("Leaves still",
                              "Leaves\nmoving\ngently",
                              "Leaves\nmoving\nstrongly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_bees_Wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_wind_plot
dev.off()


bees_sunshine_plot <- bees_sunshine + 
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

png(file.path(output_path, "FIT counts/FIT_public_bees_Sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_sunshine_plot
dev.off()

# # welsh
# 
# Sunshine_welsh <- bees_sunshine + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Heulwen yn ystod y cyfrif") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("Entirely in sunshine", 
#                               "Entirely shaded", 
#                               "Partly in sun and partly shaded"), 
#                    labels = c("Yn yr heulwen", 
#                               "Yn y cysgod", 
#                               "Rhannol")) +
#   theme_classic()
# 

bees_habitat_plot <- bees_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on bee count") +
  scale_x_discrete(labels = c("Agricultural",
                              "Gardens", 
                              "Semi-natural",
                              "Urban")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_bees_Habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_habitat_plot
dev.off()

# # welsh
# 
# Habitat_welsh <- bees_habitat + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o gynefin") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("A", "N", "U"), 
#                    labels = c("Amaethyddol", 
#                               "Lled-naturiol", 
#                               "Trefol")) +
#   theme_classic()
# 

bees_country_plot <- bees_country + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Country") +
  ylab("Effect on bee count") +
  scale_x_discrete(labels = c("England", "Northern\nIreland",
                              "Scotland", "Wales")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_bees_Country.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
bees_country_plot
dev.off()

# # welsh
# 
# Country_welsh <- bees_country + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Gwlad") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("England", "Scotland", "Wales"), 
#                    labels = c("Lloegr", 
#                               "Yr Alban", 
#                               "Cymru"))+
#   theme_classic()
# 

png(file.path(output_path, "FIT counts/FIT_public_bees_parametric.png"), 
    width = 160, height = 220, unit = "mm", res = 300)
gridPrint(bees_Year, bees_flower_class_plot, bees_flower_count_plot,
          bees_flower_context_plot, bees_wind_plot, bees_sunshine_plot,
          bees_habitat_plot, bees_country_plot, ncol = 2)
dev.off()
# 
# # welsh
# 
# png("./Plots/FIT_public_bees_parametric_welsh.png", 
#     width = 180, height = 160, unit = "mm", res = 300)
# gridPrint(Flower_context_welsh, Sunshine_welsh, Habitat_welsh, Country_welsh, ncol = 2)
# dev.off()

## Hoverflies

## GAMM for hoverflies ----

FIT_public_hoverflies_gamm_1 <- gamm4(hoverflies ~ s(JulDate, by = country) + Year + 
                                        flower_structure + floral_unit_count + flower_context + 
                                        wind_speed + sunshine + habitat_type + country,
                                        family = poisson, random = ~(1|site_1km), 
                                        data = FIT_public)


FIT_public_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + Year + flower_structure + 
                                      floral_unit_count + flower_context + 
                                      wind_speed + sunshine + habitat_type + country,
                                      family = poisson, random = ~(1|site_1km), 
                                      data = FIT_public)


AIC(FIT_public_hoverflies_gamm_1$mer, FIT_public_hoverflies_gamm_2$mer)

summary(FIT_public_hoverflies_gamm_1$gam)
# all variables have a significant effect on hoverfly counts

FIT_public_hoverflies_gamm_1$mer

sink(file.path(output_path, "FIT counts/FIT_public_hoverflies_summary.txt"))
summary(FIT_public_hoverflies_gamm_1$gam)
sink()

par(mfrow = c(2,2))
gam.check(FIT_public_hoverflies_gamm_1$gam, type = "deviance")

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_smoothers.png"), 
    width = 155, height = 120, unit = "mm", res = 300)
par(mfrow = c(2,2))#, mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
plot.gam(FIT_public_hoverflies_gamm_1$gam,  
         shift = FIT_public_hoverflies_gamm_1$gam$coefficients[1], 
         trans = exp, xlab = "\nJulian Date\nApril 10 - October 27",
         ylab = "Hoverfly count", shade = TRUE, shade.col = "grey")
mtext("England", side = 3, line = -2, outer = TRUE, adj = 0.25, font = 2)
mtext("Northern Ireland", side = 3, line = -2, outer = TRUE, adj = 0.8, font = 2)
mtext("Scotland", side = 1, line = -13, outer = TRUE, adj = 0.25, font = 2)
mtext("Wales", side = 1, line = -13, outer = TRUE, adj = 0.75, font = 2)
dev.off()


hoverflies_gamm_viz <- getViz(FIT_public_hoverflies_gamm_1$gam)

hoverflies_year <- plot(pterm(hoverflies_gamm_viz, 1))
hoverflies_flower_class <- plot(pterm(hoverflies_gamm_viz, 2))
hoverflies_flower_count <- plot(pterm(hoverflies_gamm_viz, 3))
hoverflies_flower_context <- plot(pterm(hoverflies_gamm_viz, 4))
hoverflies_wind <- plot(pterm(hoverflies_gamm_viz, 5))
hoverflies_sunshine <- plot(pterm(hoverflies_gamm_viz, 6))
hoverflies_habitat <- plot(pterm(hoverflies_gamm_viz, 7))


hoverflies_year_plot <- hoverflies_year +
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on hoverfly count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_year.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_year_plot
dev.off()



hoverflies_flower_class_plot <- hoverflies_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("open", "closed"), labels = c("Open", "Closed")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_flower_class_plot
dev.off()


# # welsh
# 
# Flower_class_welsh <- hoverflies_flower_class + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o flodyn") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   scale_x_discrete(breaks = c("0", "1"), labels = c("Agored", "Caeedig ")) +
#   theme_classic()


hoverflies_flower_count_plot <- hoverflies_flower_count + 
  l_fitLine(colour = "darkgoldenrod2", size = 1) + 
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
  xlab("Floral unit count") +
  ylab("Effect on hoverfly count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_flowerCount.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_flower_count_plot
dev.off()

# # welsh
# 
# Flower_count_welsh <- hoverflies_flower_count + 
#   l_fitLine(colour = "darkgoldenrod2", size = 1) + 
#   l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
#   xlab("Cyfrif uned flodeuol") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   theme_classic()
# 

hoverflies_flower_context_plot <- hoverflies_flower_context + 
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

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_flower_context_plot
dev.off()

# # welsh
# 
# Flower_context_welsh <- hoverflies_flower_context + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyd-destun y blodyn") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   scale_x_discrete(breaks = c("More or less isolated", 
#                               "Growing in a larger patch of the same flower",
#                               "Growing in a larger patch of many different flowers"), 
#                    labels = c("Ar ei ben ei hun", 
#                               "Llain fawr\n(yr un blodyn)", 
#                               "Llain fawr\n(llawer o flodau)")) +
#   theme_classic()


hoverflies_wind_plot <- hoverflies_wind +
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally",
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"),
                   labels = c("Leaves still",
                              "Leaves\nmoving\ngently",
                              "Leaves\nmoving\nstrongly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_Wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_wind_plot
dev.off()


hoverflies_sunshine_plot <- hoverflies_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("Entirely in sunshine", 
                              "Entirely shaded", 
                              "Partly in sun and partly shaded"), 
                   labels = c("In sunshine", 
                              "Shaded", 
                              "Partly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_Sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_sunshine_plot
dev.off()


hoverflies_habitat_plot <- hoverflies_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(labels = c("Agricultural",
                              "Gardens", 
                              "Semi-natural",
                              "Urban")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_public_hoverflies_Habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
hoverflies_habitat_plot
dev.off()


png(file.path(output_path, "FIT counts/FIT_public_hoverflies_parametric.png"), 
    width = 160, height = 220, unit = "mm", res = 300)
gridPrint(hoverflies_year_plot, hoverflies_flower_class_plot, hoverflies_flower_count_plot,
          hoverflies_flower_context_plot, hoverflies_wind_plot, hoverflies_sunshine_plot,
          hoverflies_habitat_plot, ncol = 2)
dev.off()

# # welsh
# 
# png("./Plots/FIT_public_hoverflies_parametric_welsh.png", 
#     width = 250, height = 80, unit = "mm", res = 300)
# gridPrint(Flower_class_welsh, Flower_count_welsh, Flower_context_welsh,ncol = 3)
# dev.off()



## 1 Km FIT counts ----

# load combined 2017-2020 data
FIT_1km <- read.csv("./FIT1km_combined.csv",
                    stringsAsFactors = FALSE)



FIT_1km <- FIT_1km %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y %H:%M"),
         JulDate = as.numeric(format(date, "%j")),
         Year = as.factor(Year),
         site_1km = as.factor(reformat_gr(FIT_1km$X1km_square, prec_out = 1000)),
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
                                        "Leaves moving strongly")),
         sunshine = factor(sunshine, ordered = T, 
                           levels = c("Entirely in sunshine",
                                      "Partly in sun and partly shaded",
                                      "Entirely shaded")),
         cloud_cover = factor(cloud_cover, ordered = TRUE,
                              levels = c("All or mostly blue",
                                         "Half blue and half cloud",
                                         "All or mostly cloud")))

# some tests to make sure we have correclty classified all the observations
summary(as.factor(FIT_1km$habitat_type))
# all obs are classified as either agricultural, garden, semi-natural or urban

table(FIT_1km$flower_structure)
# some NAs
FIT_1km[which(is.na(FIT_1km$flower_structure)), 
           c("flower_structure", "target_flower_corrected", "target_other_name_corrected","target_flower_family")]

# these should have been correctly classified
FIT_1km[284, "flower_structure"] <- "open"
FIT_1km[482, "flower_structure"] <- "closed"
FIT_1km[c(699, 701, 703, 707), "flower_structure"] <- "closed"
# the rest are other ambiguous or missing

table(FIT_1km$country)

FIT_1km$flower_structure <- as.factor(FIT_1km$flower_structure)
FIT_1km$habitat_type <- as.factor(FIT_1km$habitat_type)


# ## Data exploration ----

## GAMM for all insects ----

FIT_1Km_allInsects_gamm_1 <- gamm4(all_insects_total ~ s(JulDate, by = country) + flower_structure +
                                    floral_unit_count + flower_context + wind_speed + 
                                    sunshine + habitat_type + country + Year, 
                                    family = poisson, random = ~(1|site_1km), data = FIT_1km)


FIT_1Km_allInsects_gamm_2 <- gamm4(all_insects_total ~ s(JulDate) + flower_structure +
                                     floral_unit_count + flower_context + wind_speed + 
                                     sunshine + habitat_type + country + Year, 
                                   family = poisson, random = ~(1|site_1km), data = FIT_1km)


AIC(FIT_1Km_allInsects_gamm_1$mer, FIT_1Km_allInsects_gamm_2$mer)


summary(FIT_1Km_allInsects_gamm_1$gam)
# all variables significant

sink(file.path(output_path, "FIT counts/FIT_1Km_allInsects_summary.txt"))
summary(FIT_1Km_allInsects_gamm_1$gam)
sink()

par(mfrow = c(2,2))
gam.check(FIT_1Km_allInsects_gamm_1$gam, type = "deviance")

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_smoothers.png"), 
    width = 190, height = 120, unit = "mm", res = 300)
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

# # welsh
# 
# png("./Plots/FIT_1Km_allInsects_smoothers_welsh.png", 
#     width = 190, height = 150, unit = "mm", res = 300)
# par(mfrow = c(1,3), mar=c(7,7,5,1.5), oma=c(2,2,1,1.5), mgp = c(5,1,0))
# plot.gam(FIT_1Km_allInsects_gamm_1$gam,  
#          shift = FIT_1Km_allInsects_gamm_1$gam$coefficients[1], 
#          trans = exp, xlab = "\nDyddiad Julian \nEbrill  10 - Hydref 27",
#          ylab = "Cyfanswm cyfrif pryfed", shade = TRUE, shade.col = "grey",
#          cex.lab = 1.5, cex.axis = 1.2)
# mtext("Lloegr", side = 3, line = -2, outer = TRUE, adj = 0.2)
# mtext("Yr Alban", side = 3, line = -2, outer = TRUE, adj = 0.55)
# mtext("Cymru", side = 3, line = -2, outer = TRUE, adj = 0.9)
# dev.off()

all_insects_1km_gamm_viz <- getViz(FIT_1Km_allInsects_gamm_1$gam)



all_insects_1km_flower_class <- plot(pterm(all_insects_1km_gamm_viz, 1))
all_insects_1km_flower_count <- plot(pterm(all_insects_1km_gamm_viz, 2))
all_insects_1km_flower_context <- plot(pterm(all_insects_1km_gamm_viz, 3))

all_insects_1km_wind <- plot(pterm(all_insects_1km_gamm_viz, 4))
all_insects_1km_sunshine <- plot(pterm(all_insects_1km_gamm_viz, 5))

all_insects_1km_habitat <- plot(pterm(all_insects_1km_gamm_viz, 6))

all_insects_1km_year <- plot(pterm(all_insects_1km_gamm_viz, 8))

# # open - close
# flower_type_welsh_labels <- c("Agored", "Caeedig ")
# # isolated - same flower - many flowers
# flower_context_welsh_labels <- c("Ar ei ben ei hun", 
#                                  "Llain fawr\n(yr un blodyn)",
#                                  "Llain fawr\n(llawer o flodau)")
# # low - medium - high
# wind_welsh_labels <- c("Isel", "Cymhedrol", "Uchel")
# # in sunshine - partly - shaded
# sunshine_welsh_labels <- c("Yn yr heulwen", "Rhannol", "Yn y cysgod")
# # agricultural - semi-natural - urban
# habitat_welsh_labels <- c("Amaethyddol", "Lled-naturiol", "Trefol ")



Flower_class <- all_insects_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("open", "closed"), labels = c("Open", "Closed")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


# # welsh
# 
# Flower_class_welsh <- all_insects_1km_flower_class + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o flodyn") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   scale_x_discrete(breaks = c("0", "1"), labels = flower_type_welsh_labels) +
#   theme_classic()


Flower_count <- all_insects_1km_flower_count + 
  l_fitLine(colour = "darkgoldenrod2", size = 1) + 
  l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
  xlab("Floral unit count") +
  ylab("Effect on total insect count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_flowerCount.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_count
dev.off()


# # welsh
# 
# Flower_count_welsh <- all_insects_1km_flower_count + 
#   l_fitLine(colour = "darkgoldenrod2", size = 1) + 
#   l_ciLine(mul = 5, colour = "darkgoldenrod", linetype = 2, size = 0.5) + 
#   xlab("Cyfrif uned flodeuol") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   theme_classic()


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

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


# # welsh
# 
# Flower_context_welsh <- all_insects_1km_flower_context + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyd-destun y blodyn") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   scale_x_discrete(breaks = c("More or less isolated", 
#                               "Growing in a larger patch of the same flower",
#                               "Growing in a larger patch of many different flowers"), 
#                    labels = flower_context_welsh_labels) +
#   theme_classic()


Wind <- all_insects_1km_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on total insect count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"), 
                   labels = c("Leaves still", 
                              "Leaves\nmoving\ngently", 
                              "Leaves\nmoving\nstrongly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_Wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
dev.off()


# # welsh
# 
# Wind_welsh <- all_insects_1km_wind + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyflymder y gwynt yn ystod y cyfrif") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
#                               "Leaves moving gently all the time",
#                               "Leaves moving strongly"), 
#                    labels = wind_welsh_labels) +
#   theme_classic()



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

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_Sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()


# # welsh
# 
# Sunshine_welsh <- all_insects_1km_sunshine + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Heulwen yn ystod y cyfrif") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   scale_x_discrete(breaks = c("Entirely in sunshine", 
#                               "Partly in sun and partly shaded",
#                               "Entirely shaded"), 
#                    labels = sunshine_welsh_labels) +
#   theme_classic()


Habitat <- all_insects_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on total insect count") +
  # scale_x_discrete(breaks = c("A", "N", "U"), 
  #                  labels = c("Agricultural", 
  #                             "Semi-natural", 
  #                             "Urban")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_Habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


# # welsh
# 
# Habitat_welsh <- all_insects_1km_habitat + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o gynefin") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   scale_x_discrete(breaks = c("A", "N", "U"), 
#                    labels = habitat_welsh_labels) +
#   theme_classic()
# 

Year <- all_insects_1km_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on total insect count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_Year.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


# # welsh
# 
# Year_welsh <- all_insects_1km_year + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Blwyddyn") +
#   ylab("Effaith ar gyfanswm pryfed") +
#   theme_classic()
# 

png(file.path(output_path, "FIT counts/FIT_1Km_allInsects_parametric.png"), 
    width = 160, height = 220, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_count, Flower_context,
          Wind, Sunshine, Habitat, Year, ncol = 2, top = "")
dev.off()

# # welsh
# 
# png("./Plots/FIT_1Km_allInsects_parametric_welsh.png", 
#     width = 180, height = 220, unit = "mm", res = 300)
# gridPrint(Flower_class_welsh, Flower_count_welsh, Flower_context_welsh,
#           Wind_welsh, Sunshine_welsh, Habitat_welsh, Year_welsh, ncol = 2, top = "")
# dev.off()

## All bees ----

# calculate the counts for all bees as a sum of all the different bee groups
FIT_1km <- FIT_1km %>%
  rowwise() %>%
  mutate(all_bees = sum(bumblebees, honeybees, solitary_bees))


## GAMM for all_bees ----

FIT_1Km_bee_gamm_1 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_structure +
                            floral_unit_count + flower_context + wind_speed + 
                            sunshine + habitat_type + country + Year, 
                            family = poisson, random = ~(1|site_1km), data = FIT_1km)


FIT_1Km_bee_gamm_2 <- gamm4(all_bees ~ s(JulDate) + flower_structure +
                              floral_unit_count + flower_context + wind_speed + 
                              sunshine + habitat_type + country + Year, 
                            family = poisson, random = ~(1|site_1km), data = FIT_1km)


AIC(FIT_1Km_bee_gamm_1$mer, FIT_1Km_bee_gamm_2$mer)

summary(FIT_1Km_bee_gamm_1$gam)
# floral_unit_count is not significant
# try to exclude the variable

FIT_1Km_bee_gamm_3 <- gamm4(all_bees ~ s(JulDate, by = country) + flower_structure +
                               flower_context + wind_speed + 
                               sunshine + habitat_type + country + Year, 
                               family = poisson, random = ~(1|site_1km), 
                               data = FIT_1km)

AIC(FIT_1Km_bee_gamm_1$mer, FIT_1Km_bee_gamm_3$mer)
# the DAIC is within two of the full model
# so the best model is the most parsimonious one
summary(FIT_1Km_bee_gamm_3$gam)


sink(file.path(output_path, "FIT counts/FIT_1Km_bees_summary.txt"))
summary(FIT_1Km_bee_gamm_3$gam)
sink()


par(mfrow = c(2,2))
gam.check(FIT_1Km_bee_gamm_3$gam, type = "deviance")

png(file.path(output_path, "FIT counts/FIT_1Km_bees_smoothers.png"), 
    width = 190, height = 120, unit = "mm", res = 300)
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
bees_1km_sunshine <- plot(pterm(bees_1km_gamm_viz, 4))
bees_1km_habitat <- plot(pterm(bees_1km_gamm_viz, 5))
bees_1km_year <- plot(pterm(bees_1km_gamm_viz, 7))




Flower_class <- bees_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("open", "closed"), labels = c("Open", "Closed")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_bees_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


# # welsh
# 
# Flower_class_welsh <- bees_1km_flower_class + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + # shape =  if you want to change the symbol
#   l_rug(alpha = 0.3) +
#   xlab("Math o flodyn") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("0", "1"), labels = flower_type_welsh_labels) +
#   theme_classic()


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

png(file.path(output_path, "FIT counts/FIT_1Km_bees_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()


# # welsh
# 
# Flower_context_welsh <- bees_1km_flower_context + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyd-destun y blodyn") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("More or less isolated", 
#                               "Growing in a larger patch of the same flower",
#                               "Growing in a larger patch of many different flowers"), 
#                    labels = flower_context_welsh_labels) +
#   theme_classic()


Wind <- bees_1km_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"), 
                   labels = c("Leaves still", 
                              "Leaves\nmoving\ngently", 
                              "Leaves\nmoving\nstrongly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_bees_wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
dev.off()


# # welsh
# 
# Wind_welsh <- bees_1km_wind + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Cyflymder y gwynt yn ystod y cyfrif") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
#                               "Leaves moving gently all the time",
#                               "Leaves moving strongly"), 
#                    labels = wind_welsh_labels) +
#   theme_classic()


Sunshine <- bees_1km_sunshine + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Sunshine during count") +
  ylab("Effect on bee count") +
  scale_x_discrete(breaks = c("Entirely in sunshine",
                              "Partly in sun and partly shaded",
                              "Entirely shaded"),
                   labels = c("In sunshine",
                              "Partly",
                              "Shaded")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_bees_Sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()


Habitat <- bees_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on bee count") +
  # scale_x_discrete(breaks = c("A", "N", "U"), 
  #                  labels = c("Agricultural", 
  #                             "Semi-natural", 
  #                             "Urban")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_bees_Habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


# # welsh
# 
# Habitat_welsh <- all_insects_1km_habitat + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o gynefin") +
#   ylab("Effaith ar gyfrif gwenyn") +
#   scale_x_discrete(breaks = c("A", "N", "U"), 
#                    labels = habitat_welsh_labels) +
#   theme_classic()
# 


Year <- bees_1km_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on bee count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_bees_Year.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


png(file.path(output_path, "FIT counts/FIT_1Km_bees_parametric.png"), 
    width = 160, height = 200, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_context,
          Wind, Sunshine, Habitat, 
          Year, ncol = 2, top = "")
dev.off()


# # welsh
# 
# png("./Plots/FIT_1Km_bees_parametric_welsh.png", 
#     width = 180, height = 180, unit = "mm", res = 300)
# gridPrint(Flower_class_welsh, Flower_context_welsh,
#           Wind_welsh, Habitat_welsh, ncol = 2, top = "")
# dev.off()


## Hoverflies ----

## GAMM for hoverflies ----

FIT_1Km_hoverflies_gamm <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_structure +
                                 floral_unit_count + flower_context + wind_speed + 
                                 sunshine + habitat_type + country + Year, 
                                 family = poisson, random = ~(1|site_1km), 
                                 data = FIT_1km)


FIT_1Km_hoverflies_gamm_2 <- gamm4(hoverflies ~ s(JulDate) + flower_structure +
                                     floral_unit_count + flower_context + wind_speed + 
                                     sunshine + habitat_type + country + Year, 
                                   family = poisson, random = ~(1|site_1km), 
                                   data = FIT_1km)


AIC(FIT_1Km_hoverflies_gamm$mer, FIT_1Km_hoverflies_gamm_2$mer)

summary(FIT_1Km_hoverflies_gamm$gam)

FIT_1Km_hoverflies_gamm_3 <- gamm4(hoverflies ~ s(JulDate, by = country) + flower_structure +
                                   flower_context + wind_speed + 
                                   sunshine + habitat_type + country + Year, 
                                   family = poisson, random = ~(1|site_1km), 
                                   data = FIT_1km)

AIC(FIT_1Km_hoverflies_gamm$mer, FIT_1Km_hoverflies_gamm_3$mer)


summary(FIT_1Km_hoverflies_gamm_3$gam)

sink(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_summary.txt"))
summary(FIT_1Km_hoverflies_gamm_3$gam)
sink()

par(mfrow = c(2,2))
gam.check(FIT_1Km_hoverflies_gamm_3$gam, type = "deviance")

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_smoothers.png"), 
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
hover_1km_flower_context <- plot(pterm(hover_1km_gamm_viz, 2))
hover_1km_wind <- plot(pterm(hover_1km_gamm_viz, 3))
hover_1km_sunshine <- plot(pterm(hover_1km_gamm_viz, 4))
hover_1km_habitat <- plot(pterm(hover_1km_gamm_viz, 5))
hover_1km_year <- plot(pterm(hover_1km_gamm_viz, 7))


Flower_class <- hover_1km_flower_class + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Flower type") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("open", "closed"), labels = c("Open", "Closed")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_flowerClass.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_class
dev.off()


# # welsh
# 
# Flower_class_welsh <- hover_1km_flower_class + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o flodyn") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   scale_x_discrete(breaks = c("0", "1"), labels = flower_type_welsh_labels) +
#   theme_classic()


Flower_context <- hover_1km_flower_context + 
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

png(file.path(output_path, "FIT counts/FIT_1Km_hoverfly_flowerContext.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Flower_context
dev.off()



Wind <- hover_1km_wind + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Wind speed during count") +
  ylab("Effect on hoverfly count") +
  scale_x_discrete(breaks = c("Leaves still/moving occasionally", 
                              "Leaves moving gently all the time",
                              "Leaves moving strongly"), 
                   labels = c("Leaves still", 
                              "Leaves\nmoving\ngently", 
                              "Leaves\nmoving\nstrongly")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_wind.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Wind
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

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_sunshine.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Sunshine
dev.off()


# # welsh
# 
# Sunshine_welsh <- hover_1km_sunshine + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Heulwen yn ystod y cyfrif") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   scale_x_discrete(breaks = c("Entirely in sunshine", 
#                               "Entirely shaded",
#                               "Partly in sun and partly shaded"), 
#                    labels = c("Yn yr heulwen", "Yn y cysgod", "Rhannol")) +
#   theme_classic()
# 

Habitat <- hover_1km_habitat + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Habitat type") +
  ylab("Effect on hoverfly count") +
  # scale_x_discrete(breaks = c("A", "N", "U"), 
  #                  labels = c("Agricultural", 
  #                             "Semi-natural", 
  #                             "Urban")) +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_habitat.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Habitat
dev.off()


# # welsh
# 
# Habitat_welsh <- hover_1km_habitat + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Math o gynefin") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   scale_x_discrete(breaks = c("A", "N", "U"), 
#                    labels = habitat_welsh_labels) +
#   theme_classic()
# 

Year <- hover_1km_year + 
  l_ciBar(colour = "darkgoldenrod", size = 1) +   
  l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
  l_rug(alpha = 0.3) +
  xlab("Year") +
  ylab("Effect on hoverfly count") +
  theme_classic()

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_year.png"), 
    width = 80, height = 80, unit = "mm", res = 300)
Year
dev.off()


# # welsh
# 
# Year_welsh <- hover_1km_year + 
#   l_ciBar(colour = "darkgoldenrod", size = 1) +   
#   l_fitPoints(colour = "darkgoldenrod2", size = 5) + 
#   l_rug(alpha = 0.3) +
#   xlab("Blwyddyn") +
#   ylab("Effaith ar gyfrif pryfed hofran") +
#   theme_classic()
# 

png(file.path(output_path, "FIT counts/FIT_1Km_hoverflies_parametric.png"), 
    width = 160, height = 200, unit = "mm", res = 300)
gridPrint(Flower_class, Flower_context, Wind, 
          Sunshine, Habitat, Year, ncol = 2, top = "")
dev.off()


# # welsh
# 
# png("./Plots/FIT_1Km_hoverflies_parametric_welsh.png", 
#     width = 180, height = 180, unit = "mm", res = 300)
# gridPrint(Flower_class_welsh, Sunshine_welsh, 
#           Habitat_welsh, Year_welsh, ncol = 2, top = "")
# dev.off()
