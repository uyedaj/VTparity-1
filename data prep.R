rm(list = ls())

library(tidyverse)
library(lubridate)
library(forcats)
library(spdep)
library(reshape)
library(dplyr)

setwd("C:/Users/George/Documents/Holly")

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
dat$RepMode = as.factor(dat$RepMode)

# remove species I already have data for (I trust my numbers more than these)
spp = c("Alytes obstetricans", "Arthroleptis poecilonotus", "Bombina bombina", "Bombina pachypus", "Bombina variegata", 
        "Anaxyrus americanus", "Rhinella marina", "Anaxyrus quercicus", "Dendrobates auratus", "Nanorana parkeri", "Eleutherodactylus coqui",
        "Osteopilus septentrionalis", "Pseudacris crucifer", "Hyla gratiosa", "Hyla versicolor", "Gastrophryne carolinensis", "Pelobates fuscus",
        "Pelodytes punctatus", "Pipa pipa", "Xenopus laevis", "Pyxicephalus adspersus", "Lithobates catesbeianus", "Lithobates pipiens", 
        "Lithobates clamitans", "Lithobates sylvaticus", "Rana aurora", "Rana latastei", "Scaphiopus couchii", "Scaphiopus holbrookii", 
        "Ambystoma cingulatum", "Ambystoma gracile", "Ambystoma jeffersonianum", "Ambystoma macrodactylum", "Ambystoma maculatum", "Ambystoma opacum",
        "Ambystoma talpoideum", "Amphiuma tridactylum", "Andrias japonicus", "Cryptobranchus alleganiensis", "Plethodon glutinosus", "Desmognathus quadramaculatus",
        "Desmognathus ochrophaeus", "Necturus maculosus", "Proteus anguinus", "Rhyacotriton olympicus", "Notophthalmus viridescens",
        "Taricha torosa", "Salamandra salamandra", "Salamandrina perspicillata", "Siren intermedia", "Siren lacertina", "Typhlonectes compressicauda")
        
dat = dat %>%
  filter(!Species %in% spp)

# extract useful columns
amphs = dat %>%
  mutate(Class = rep("Amphibia", length.out = length(Order))) %>%
  select(class = "Class", order = "Order", family = "Family", AaM = "AaM", log.AaM = "log.AaM", longevity = "Longevity_max_y", "juvSurv", "juvMort", "adMort", mass = "Body_mass_g", svl = "Body_size_mm", birth.svl = "Offspring_size_mean", repmode = "RepMode")

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
  select(class = "Class", order = "Order", family = "Family", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", repmode = "repmode")

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
  select(class = "Class", order = "Order", family = "Family", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", repmode = "repmode")

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

dat$Order = as.factor(dat$Order)

liz = dat %>%
  select(class = "Class", order = "Order", family = "Family", "AaM", "log.AaM", "adMort", svl = "female.SVL", birth.svl = "hatchling.neonate.SVL", repmode = "reproductive.mode")

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

endos = dat %>% 
  mutate(order = class) %>%
  select("order", "class", "family", "AaM", "log.AaM", "longevity", "juvSurv", "juvMort", "adMort", svl = "adult_svl_cm", birth.svl = "birth_or_hatching_svl_cm", mass = "adult_body_mass_g", birth.mass = "birth_or_hatching_weight_g", repmode = "repmode") %>%
  mutate(svl = svl * 10) %>%
  mutate(birth.svl = birth.svl * 10) # convert to mm for consistency

rm(dat)

################
# merge datasets

x = merge(endos, amphs, all = TRUE)
x = merge(x, reps, all = TRUE)
x = merge(x, amps, all = TRUE)




