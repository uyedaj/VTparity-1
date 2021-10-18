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
dat[which(dat[,22] == 0), 22] = NA
dat[which(dat[,23] == 0), 23] = NA
dat[which(dat[,24] == 0), 24] = NA
dat[which(dat[,26] == 0), 26] = NA


repmode = dat[,c(1,22:24,26)] %>% gather(type, value, -id) %>% na.omit() %>% select(-value)%>% arrange(id)

dat = left_join(dat, repmode)
dat$RepMode = as.factor(dat$type)

summary(dat$RepMode)


#dat$RepMode = names(dat[,35:37])[max.col(!is.na(dat[,35:37]))]

#dat[which(dat[,7] == "P"),47] = "P"

#dat$RepMode = as.factor(dat$RepMode)

# create relative cluss match column
dat$RCM = ((dat$Offspring_size_mean^3) * dat$Litter_size_mean) / (dat$Body_size_mm^3)

# create investment per bout column
dat$IPB = dat$Litter_size_mean / (1/dat$juvSurv)

dat$reloff = (dat$Offspring_size_mean^3)/(dat$Body_size_mm^3)

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
  select(class = "Class", order = "Order", family = "Family", species = "Species", AaM = "AaM", log.AaM = "log.AaM", longevity = "Longevity_max_y", "juvSurv", "juvMort", "adMort", mass = "Body_mass_g", svl = "Body_size_mm", birth.svl = "Offspring_size_mean", repmode = "RepMode", RCM = "RCM", IPB = "IPB", reloff = "reloff", Terr = "Terr", AdTerr = "AdTerr")

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

dat$GS <- paste(dat$Genus, dat$Species)

amps = dat %>%
  select(class = "Class", order = "Order", family = "Family", species = "GS", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", repmode = "repmode", Terr = "Terr")

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

dat$GS <- paste(dat$Genus, dat$Species)


reps = dat %>%
  select(class = "Class", order = "Order", family = "Family", species = "GS", "AaM", log.AaM = "log.AaM", longevity = "Longevity", "juvSurv", "juvMort", "adMort", svl = "Max.Size", birth.svl = "birth.svl", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr")

rm(dat)
summary(reps)
reps$na_count <- apply(reps, 1, function(x) sum(is.na(x)))

summary(as.factor(reps$na_count))
###################################################
# Meiri, S., 2018. Traits of lizards of the world: 
# Variation around a successful evolutionary design. Global ecology and biogeography, 27(10), pp.1168-1172.
dat = read.csv("Lizards.csv")
#dat = dat[complete.cases(dat),]

dat$maximum.SVL = as.numeric(as.character(dat$maximum.SVL))
dat$female.SVL = as.numeric(as.character(dat$female.SVL))
dat$hatchling.neonate.SVL = as.numeric(as.character(dat$hatchling.neonate.SVL))
dat$Longevity = as.numeric(dat$Longevity)

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

dat$juvSurv = ifelse((dat$Longevity - dat$AaM) < 1, 
                     1/(dat$Clutch_mean),
                     1/((dat$Longevity - dat$AaM) * (dat$Clutch)))

dat$juvMort = 1-log(dat$juvSurv)

dat$Order = as.factor(dat$Order)
dat$Species = paste(dat$Genus, dat$epithet, sep=" ")

liz = dat %>%
  mutate(Terr = rep("1", length.out = length(AaM))) %>%
  mutate(AdTerr = rep("1", length.out = length(AaM))) %>%
  select(class = "X", order = "Order", family = "Family", species = "Species", "AaM", "log.AaM", "adMort", "juvMort",  svl = "female.SVL", birth.svl = "hatchling.neonate.SVL", repmode = "reproductive.mode", Terr = "Terr")

rm(dat)
summary(liz)
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
dat$Species = paste(dat$genus, dat$species, sep=" ")


endos = dat %>% 
  mutate(order = class) %>%
  select("order", "class", "family", species = "Species", "AaM", "log.AaM", "longevity", "juvSurv", "juvMort", "adMort", svl = "adult_svl_cm", birth.svl = "birth_or_hatching_svl_cm", mass = "adult_body_mass_g", birth.mass = "birth_or_hatching_weight_g", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr") %>%
  mutate(svl = svl * 10) %>%
  mutate(birth.svl = birth.svl * 10) # convert to mm for consistency

endos[which(endos$family == "Tachyglossidae"),15] = "0"
endos[which(endos$family == "Ornithorhynchidae"),15] = "0"

rm(dat)



#############################################
# FishTraits, FishBase
############################################

dat = read.csv("bony_fish.csv")

species = unique(dat[,c(2,3,16,17,18)])

fish = data.frame(species, 
                  AgeMatMin = as.vector(tapply(dat$AgeMatMin, dat$SpecCode, mean, na.rm=TRUE)),
                  AgeMatMin2 = as.vector(tapply(dat$AgeMatMin2, dat$SpecCode, mean, na.rm=TRUE)),
                  FecundityMin = as.vector(tapply(dat$FecundityMin, dat$SpecCode, mean, na.rm=TRUE)),
                  FecundityMax = as.vector(tapply(dat$FecundityMax, dat$SpecCode, mean, na.rm=TRUE)),
                  longevity = as.vector(tapply(dat$tmax, dat$SpecCode, mean, na.rm=TRUE)),
                  birth.svlMin = as.vector(tapply(dat$LengthOffspringMin, dat$SpecCode, mean, na.rm=TRUE)), 
                  birth.svlMax = as.vector(tapply(dat$LengthOffspringMax, dat$SpecCode, mean, na.rm=TRUE)))
                  
fish$AaM <- rowMeans(cbind(fish$AgeMatMin, fish$AgeMatMin2), na.rm = T)
fish$fecundity <- rowMeans(cbind(fish$FecundityMin, fish$FecundityMax), na.rm = T)
fish$birth.svl <- rowMeans(cbind(fish$birth.svlMin, fish$birth.svlMax), na.rm = T)

fish$juvSurv = ifelse((fish$longevity - fish$AaM) < 1, 
                      1/(fish$fecundity),
                      1/((fish$longevity - fish$AaM) * (fish$fecundity)))

fish$juvMort = 1-log(fish$juvSurv)

fish$log.AaM = log(fish$AaM)
fish$adMort = 1/(1 + fish$AaM)

# body size
dat2 = species() 

dat2$svl <- rowMeans(cbind(dat2$LengthFemale, dat2$Length), na.rm = T)

dat2 = dat2 %>% 
  select(SpecCode, Species, svl)

fish = merge(fish, dat2, by = "SpecCode") 

fish = fish %>%
  mutate(Terr = rep("0", length.out = length(Class))) %>%
  mutate(AdTerr = rep("0", length.out = length(Class))) %>%
  mutate(repmode = rep("0", length.out = length(Class))) %>%
  select(class = "Class", order = "Order", family = "Family", species = "Species.x", "AaM", log.AaM = "log.AaM", longevity = "longevity", "juvSurv", "juvMort", "adMort", svl = "svl", birth.svl, repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr")

fish$na_count <- apply(fish, 1, function(x) sum(is.na(x)))

summary(fish$na_count)

write.csv(fish, "bony_fish.csv")
###############
#dat2 = species() 

#dat2$svl <- rowMeans(cbind(dat2$LengthFemale, dat2$Length), na.rm = T)

#dat2 = dat2 %>% 
#  select(SpecCode, Species, svl)


#fecundity traits
#dat3 <- fecundity()

#dat3$fec <- rowMeans(cbind(dat3$FecundityMin, dat3$FecundityMax), na.rm = T)

#dat3 = dat3 %>%
#  select(SpecCode, fec)


# maturity traits
#dat4 <- maturity()  

#dat4$AaM <- rowMeans(cbind(dat4$AgeMatMin, dat4$AgeMatMin2), na.rm = T)

#dat4 = dat4 %>%
#  select(SpecCode, AaM)


# larval data
#dat5 = larvae()

#dat5$birth.svl = rowMeans(cbind(dat5$LhMin, dat5$LhMax), na.rm = T)

#dat5 = dat5 %>%
#  select(SpecCode, birth.svl)


#dd = merge(dat, dat2, by = "SpecCode")
#dd = merge(dd, dat3, by = "SpecCode")
#dd = merge(dd, dat4, by = "SpecCode")
#dd = merge(dd, dat5, by = "SpecCode")

#dat2$fec <- as.vector(tapply(dd$fec, dd$SpecCode, mean, na.rm=TRUE))
#dat2$AaM <- as.vector(tapply(dd$AaM, dd$SpecCode, mean, na.rm=TRUE)) 
#dat2$birth.svl <- as.vector(tapply(dd$birth.svl, dd$SpecCode, mean, na.rm=TRUE)) 


#dat <- dd[match(unique(dd$SpecCode), dd$SpecCode),]

#dat2$juvSurv = ifelse((dat2$longevity - dat2$AaM) < 1, 
#                     1/(dat2$fec),
#                     1/((dat2$longevity - dat2$AaM) * (dat2$fec)))

#dat2$juvMort = 1-log(dat2$juvSurv)

#dat4$log.AaM = log(dat4$AaM)
#dat$adMort = 1/(1 + dat$AaM)

#fish = dat %>%
#  mutate(Terr = rep("0", length.out = length(svl))) %>%
#  mutate(AdTerr = rep("0", length.out = length(svl))) %>%
#  mutate(Class = rep("Fish", length.out = length(svl))) %>%
#  mutate(Order = rep("Fish", length.out = length(svl))) %>%
#  mutate(Family = rep("Fish", length.out = length(svl))) %>%
#  mutate(repmode = rep("0", length.out = length(svl))) %>%
#  select(class = "Class", order = "Order", family = "Family", species = "Species.x", "AaM", log.AaM = "log.AaM", longevity = "longevity", "juvSurv", "juvMort", "adMort", svl = "svl", birth.svl = "LL", repmode = "repmode", Terr = "Terr", AdTerr = "AdTerr")

rm(dat)



#### shark data ######

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
  select(class = "Class", order = "Class", family = "family", "species", AaM = "age.mat", log.AaM = "log.AaM", longevity = "max.age", "juvSurv", "juvMort", "adMort", svl = "max.size", birth.svl = "pup.size", repmode = "bear", Terr = "Terr", AdTerr = "AdTerr")
  

################
# merge datasets
fish = read.csv("bony_fish.csv")

x = merge(endos, amphs, all = TRUE)
x = merge(x, reps, all = TRUE)
x = merge(x, amps, all = TRUE)
x = merge(x, liz, all = TRUE)
x = merge(x, fish, all = TRUE)
x = merge(x, sharks, all = TRUE)




x = x %>% 
  mutate(repmode = recode(repmode, 
                          `Dir`="1",
                          `Viv`="1",
                          `Lar`="0",
                          `Ped`="0",
                          `Mixed`="1",
                          `Oviparous`="0",
                          `unclear`="1",
                          `Viviparous`="1",
                          `live-bearing`="1",
                          `egg-laying`="0")) %>%
  droplevels()


write.csv(x = x, file = "vtparity.csv")

summary(x$repmode)
