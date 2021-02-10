rm(list = ls())

library(tidyverse)
library(lubridate)
library(forcats)
library(spdep)
library(reshape)
library(dplyr)
library(rfishbase)


setwd("I:/VTech/Holly/Data")

#############################################
# AmphiBIO, a global database for amphibians  
#Brunno Freire Oliveira et al.
############################################

dat = read.csv("AmphiBIO_v1.csv")
#dat = dat[complete.cases(dat),]

# calculate mean values for ranges provided
dat$AaM = ((dat$Age_at_maturity_min_y + dat$Age_at_maturity_max_y) / 2)
dat$Litter_size_mean = ((dat$Litter_size_min_n + dat$Litter_size_max_n) / 2)
dat$Offspring_size_mean = ((dat$Offspring_size_min_mm + dat$Offspring_size_max_mm) / 2)
dat$SS = ((dat$Age_at_maturity_max_y - dat$Age_at_maturity_min_y)) / dat$AaM

# calculate juvenile and adult mortality 
# lifetime fecundity and age at maturity as proxies respectively
dat$log.AaM = log(dat$AaM)
dat$juvSurv = ifelse((dat$Longevity_max_y - dat$AaM) < 1, 
                     1/(dat$Litter_size_mean * dat$Reproductive_output_y),
                     1/((dat$Longevity_max_y - dat$AaM) * (dat$Litter_size_mean * dat$Reproductive_output_y)))

dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$AaM)

dat$Order = as.factor(dat$Order)


# create factor for reproductive mode
dat[which(dat[,35] == 0), 35] = NA
dat[which(dat[,36] == 0), 36] = NA
dat[which(dat[,37] == 0), 37] = NA

dat$RepMode = names(dat[,35:37])[max.col(!is.na(dat[,35:37]))]

dat[which(dat[,7] == "P"),47] = "P"

dat$RepMode = as.factor(dat$RepMode)

# create relative cluss match column
dat$RCM = ((dat$Offspring_size_mean^3) * dat$Litter_size_mean) / (dat$Body_size_mm^3)

# create investment per bout column
dat$IPB = dat$Litter_size_mean / (1/dat$juvSurv)

dat$reloff = (dat$Offspring_size_mean^3)/(dat$Body_size_mm^3)

dat = dat %>% 
  mutate(repmode = recode(RepMode, 
                          `Viv`="Dir"))


dat[which(dat$repmode == "Dir"), 41] = "1"
#dat$habitat = names(dat[,6:9])[max.col(!is.na(dat[,6:9]))]
#dat$habitat = as.factor(dat$habitat)

#datsub = dat %>%
#  filter(Family == "Brachycephalidae" | Family == "Bufonidae" | Family == "Ranidae" | Family == "Microhylidae" | Family == "Hylidae" | Family == "Strabomantidae") %>%
#  select(Family, Body_size_mm, Body_mass_g)

#find_hull <- function(datsub) datsub[chull(datsub$Body_size_mm, datsub$Body_mass_g), ]
#hulls <- ddply(na.omit(datsub), "Family", find_hull)

#ggplot(datsub, aes(x = log(Body_size_mm ^ 3), y = log(Body_mass_g), col = Family)) +
#  geom_point() + scale_colour_manual(values = c("Brachycephalidae" = "Dark Green", "Bufonidae" = "Dark Blue", "Hylidae" = "Dark Red", "Microhylidae" = "Black", "Strabomantidae" = "Black", "Ranidae" = "Black"))

#fig2 <- ggplot(datsub, aes(x = log(Body_size_mm ^ 3), y = log(Body_mass_g), col = Family, fill = Family)) + 
#  geom_point() + 
  #labs(x = "Body Size", y = "Mass"); fig2

#ggplot(dat, aes(Order, log(Body_size_mm), col = Order)) +
 #geom_sina() +
#  xlab("") + ylab("Body Size") +
#  theme_Publication() + theme(
#    axis.text.x = element_blank(),
#    axis.text.y = element_blank(),
#    axis.ticks = element_blank())+ 
#  scale_color_brewer()

# Fig. 2A
# Density lines
#fig2a <- fig2 + geom_density2d(alpha=.5); fig2a

# Fig. 2B
# Convex hulls
#fig2b <- fig2 + geom_polygon(data=hulls, alpha=.2); fig2b

# remove species I already have data for (I trust my numbers more than these)
#spp = c("Alytes obstetricans", "Arthroleptis poecilonotus", "Bombina bombina", "Bombina pachypus", "Bombina variegata", 
#        "Anaxyrus americanus", "Rhinella marina", "Anaxyrus quercicus", "Dendrobates auratus", "Nanorana parkeri", "Eleutherodactylus coqui",
#        "Osteopilus septentrionalis", "Pseudacris crucifer", "Hyla gratiosa", "Hyla versicolor", "Gastrophryne carolinensis", "Pelobates fuscus",
#        "Pelodytes punctatus", "Pipa pipa", "Xenopus laevis", "Pyxicephalus adspersus", "Lithobates catesbeianus", "Lithobates pipiens", 
#        "Lithobates clamitans", "Lithobates sylvaticus", "Rana aurora", "Rana latastei", "Scaphiopus couchii", "Scaphiopus holbrookii", 
#        "Ambystoma cingulatum", "Ambystoma gracile", "Ambystoma jeffersonianum", "Ambystoma macrodactylum", "Ambystoma maculatum", "Ambystoma opacum",
#        "Ambystoma talpoideum", "Amphiuma tridactylum", "Andrias japonicus", "Cryptobranchus alleganiensis", "Plethodon glutinosus", "Desmognathus quadramaculatus",
#        "Desmognathus ochrophaeus", "Necturus maculosus", "Proteus anguinus", "Rhyacotriton olympicus", "Notophthalmus viridescens",
#        "Taricha torosa", "Salamandra salamandra", "Salamandrina perspicillata", "Siren intermedia", "Siren lacertina", "Typhlonectes compressicauda")
        
#dat = dat %>%
#  filter(!Species %in% spp)

# extract useful columns
amphs = dat %>%
  mutate(Class = rep("Amphibia", length.out = length(Order))) %>%
  select(class = "Class", order = "Order", family = "Family", AaM = "AaM", log.AaM = "log.AaM", longevity = "Longevity_max_y", "juvSurv", "juvMort", "adMort", mass = "Body_mass_g", svl = "Body_size_mm", birth.svl = "Offspring_size_mean", repmode = "repmode", RCM = "RCM", IPB = "IPB", reloff = "reloff", Terr = "Terr", AdTerr = "AdTerr")

rm(dat)
rm(spp)

################################
# personally compiled amphibians
dat = read.csv("reptilesBIO.csv")

dat = as.data.frame(dat)
#dat = dat[complete.cases(dat),]

dat = dat %>%
  filter(Class == "Amphibia")

dat$repmode = as.factor(dat$repmode)
dat$log.AaM = log(dat$AaM)
dat$juvSurv = ifelse((dat$Longevity - dat$AaM) < 1, 
                     1/(dat$Clutches.per.year * dat$Clutch),
                     1/((dat$Longevity - dat$AaM) * (dat$Clutches.per.year * dat$Clutch)))

dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$AaM)
amps = dat %>%
  select(class = "Class", order = "Order", family = "Family", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", repmode = "repmode", Terr = "Terr")

rm(dat)

##############################
# Personally compiled reptiles
##############################
dat = read.csv("reptilesBIO.csv")

dat = as.data.frame(dat)
#dat = dat[complete.cases(dat),]
dat = dat %>%
  filter(Class == "Reptilia")

dat$repmode = as.factor(dat$repmode)
dat$log.AaM = log(dat$AaM)

dat$juvSurv = ifelse((dat$Longevity - dat$AaM) < 1, 
                     1/(dat$Clutches.per.year * dat$Clutch),
                     1/((dat$Longevity - dat$AaM) * (dat$Clutches.per.year * dat$Clutch)))

dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$AaM)

reps = dat %>%
  select(class = "Class", order = "Order", family = "Family", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", birth.svl = "birth.svl", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr")

rm(dat)

###################################################
# Meiri, S., 2018. Traits of lizards of the world: 
# Variation around a successful evolutionary design. Global ecology and biogeography, 27(10), pp.1168-1172.
dat = read.csv("Lizards.csv")
#dat = dat[complete.cases(dat),]

dat$maximum.SVL = as.numeric(as.character(dat$maximum.SVL))
dat$female.SVL = as.numeric(as.character(dat$female.SVL))
dat$hatchling.neonate.SVL = as.numeric(as.character(dat$hatchling.neonate.SVL))

# if no female specific max size, use max size
dat[is.na(dat[,13]),13] = dat[is.na(dat[,13]),12]

# calculate average clutch size (using range if mean not reported)
dat$Clutch_mean = (dat$smallest.mean.clutch.size + dat$largest.mean.clutch.size) / 2
dat$Clutch_range = (dat$smallest.clutch + dat$largest.clutch) / 2
dat[is.na(dat[,26]),26] = dat[is.na(dat[,26]),27]

# caculate age at maturity (in years)
dat$AaM = (dat$oldest.age.at.first.breeding..months. + dat$youngest.age.at.first.breeding..months.) / 24
dat$log.AaM = log(dat$AaM)
dat$adMort = 1/(1 + dat$AaM)

dat$SS = (dat$oldest.age.at.first.breeding..months. - dat$youngest.age.at.first.breeding..months.) / dat$AaM


dat$Order = as.factor(dat$Order)

liz = dat %>%
  mutate(Terr = rep("1", length.out = length(AaM))) %>%
  mutate(AdTerr = rep("1", length.out = length(AaM))) %>%
  select(class = "Class", order = "Order", family = "Family", "AaM", "log.AaM", "adMort", svl = "female.SVL", birth.svl = "hatchling.neonate.SVL", repmode = "reproductive.mode", Terr = "Terr")

rm(dat)

####################################
# endotherms
# Nathan P. Myhrvold, Elita Baldridge, Benjamin Chan, Dhileep Sivam, Daniel L. Freeman, and S. K. Morgan Ernest. 2015. 
# An amniote life-history database to perform comparative analyses with birds, mammals, and reptiles. Ecology 96:3109.
dat = read.csv("Mammals.csv")

dat = dat %>%
  filter(class != "Reptilia") %>%
  droplevels()

dat[dat == -999] = NA
dat$AaM = dat$female_maturity_d/365
dat$log.AaM = log(dat$AaM)
dat$longevity = dat$longevity_y

dat$juvSurv = ifelse((dat$longevity - dat$AaM) < 1, 
                     1/(dat$litters_or_clutches_per_y * dat$litter_or_clutch_size_n),
                     1/((dat$longevity - dat$AaM) * (dat$litters_or_clutches_per_y * dat$litter_or_clutch_size_n)))


dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$AaM)

dat$repmode = as.factor(as.numeric(as.factor(dat$class))-1)

dat$SS = (dat$female_maturity_d - dat$male_maturity_d) / dat$female_maturity_d

endos = dat %>% 
  mutate(order = class) %>%
  select("order", "class", "family", "AaM", "log.AaM", "longevity", "juvSurv", "juvMort", "adMort", svl = "adult_svl_cm", birth.svl = "birth_or_hatching_svl_cm", mass = "adult_body_mass_g", birth.mass = "birth_or_hatching_weight_g", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr") %>%
  mutate(svl = svl * 10) %>%
  mutate(birth.svl = birth.svl * 10) # convert to mm for consistency

rm(dat)



#############################################
# FishTraits, FishBase
############################################

fish = species() 

dat = fish %>% 
  select(SpecCode, longevity = "LongevityWild", svl = "LengthFemale")


#fecundity traits

juvsurv <- fecundity()

juvsurv$fecundity <- (juvsurv$FecundityMin + juvsurv$FecundityMax)/2

dat1 = juvsurv %>%
  select(SpecCode, fecundity)

# maturity traits
adsurv <- maturity()  

adsurv$AaM <- (adsurv$AgeMatMin + adsurv$AgeMatMin2)/2

dat2 = adsurv %>%
  select(SpecCode, AaM)

dat2$log.AaM = log(dat2$AaM)

# larval data
larv = larvae()

larv$LarvalDuration = (larv$LarvalDurationMin + larv$LarvalDurationMax) / 2
larv$LL = (larv$LhMin + larv$LhMax) / 2

dat3 = larv %>%
  select(SpecCode, PlaceofDevelopment, LarvalDuration, LL)



dd = merge(dat, dat1, by = "SpecCode")
dd = merge(dd, dat2, by = "SpecCode")
dd = merge(dd, dat3, by = "SpecCode")




spplon = tapply(dd$longevity, dd$SpecCode, mean, na.rm=TRUE)
sppfec <- tapply(dd$fecundity, dd$SpecCode, mean, na.rm=TRUE)
sppmat <- tapply(dd$AaM, dd$SpecCode, mean, na.rm=TRUE) 
sppLL <- tapply(dd$LL, dd$SpecCode, mean, na.rm=TRUE) 
sppL <- tapply(dd$svl, dd$SpecCode, mean, na.rm=TRUE) 
sppLD = tapply(dd$LarvalDuration, dd$SpecCode, mean, na.rm=TRUE)
sppPoD = tapply(dd$PlaceofDevelopment, dd$SpecCode, ) 

dat <- dd[match(unique(dd$SpecCode), dd$SpecCode),]

dat$juvSurv = ifelse((dat$longevity - dat$AaM) < 1, 
                     1/(dat$fecundity),
                     1/((dat$longevity - dat$AaM) * (dat$fecundity)))

dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$AaM)

fish = dat %>%
  mutate(Terr = rep("0", length.out = length(svl))) %>%
  mutate(AdTerr = rep("0", length.out = length(svl))) %>%
  mutate(Class = rep("Fish", length.out = length(svl))) %>%
  mutate(Order = rep("Fish", length.out = length(svl))) %>%
  mutate(Family = rep("Fish", length.out = length(svl))) %>%
  mutate(repmode = rep("0", length.out = length(svl))) %>%
  select(class = "Class", order = "Order", family = "Family", "AaM", log.AaM = "log.AaM", longevity = "longevity", "juvSurv", "juvMort", "adMort", svl = "svl", birth.svl = "LL", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr")

rm(dat)




dat = read.csv("SharkData.csv")
#dat = dat[complete.cases(dat),]


# calculate juvenile and adult mortality 
# lifetime fecundity and age at maturity as proxies respectively
dat$log.AaM = log(dat$age.mat)
dat$juvSurv = ifelse((dat$max.age - dat$age.mat) < 1, 
                     1/(dat$litter.size * dat$interval),
                     1/((dat$max.age - dat$age.mat) * (dat$litter.size * dat$interval)))

dat$juvMort = 1-log(dat$juvSurv)
dat$adMort = 1/(1 + dat$age.mat)

dat$class = as.factor(dat$ï..superorder)

dat$max.size = dat$max.size * 10
dat$pup.size = dat$pup.size * 10

sharks = dat %>% mutate(Class = rep("Sharks", length.out = length(max.age))) %>%
  mutate(AdTerr = rep("0", length.out = length(max.age))) %>%
  mutate(Terr = rep("0", length.out = length(max.age))) %>%
  select(class = "Class", order = "Class", family = "family", AaM = "age.mat", log.AaM = "log.AaM", longevity = "max.age", "juvSurv", "juvMort", "adMort", svl = "max.size", birth.svl = "pup.size", repmode = "bear", Terr = "Terr", AdTerr = "AdTerr")
  

################
# merge datasets

x = merge(endos, amphs, all = TRUE)
x = merge(x, reps, all = TRUE)
x = merge(x, amps, all = TRUE)
x = merge(x, liz, all = TRUE)
x = merge(x, fish, all = TRUE)
x = merge(x, sharks, all = TRUE)


x$temp1 = (x$birth.svl^3)/(x$svl^3)
x$temp2 = x$birth.mass/x$mass

x$reloff = pmax(x$temp1, x$temp2, na.rm = T)


x = x %>% 
  mutate(repmode = recode(repmode, 
                          `Dir`="1",
                          `Viv`="1",
                          `Lar`="0",
                          `Mixed`="1",
                          `Oviparous`="0",
                          `unclear`="1",
                          `Viviparous`="1",
                          `live-bearing`="1",
                          `egg-laying`="0")) %>%
  droplevels()



