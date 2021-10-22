
library(ggplot2)
library(tidyverse)
library(lubridate)
library(forcats)
library(spdep)
library(reshape)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(cowplot) 
library(ggforce)
library(corrplot)
library(gghalves)
library(EnvStats)

x = x %>% 
  mutate(repmode = recode(repmode, 
                          `0`="Egg Laying",
                          `1`="Live Bearing")) %>%
  mutate(AdTerr = recode(AdTerr, 
                          `0`="Aquatic",
                          `1`="Terrestrial")) %>%
  mutate(Terr = recode(Terr, 
                         `0`="Aquatic",
                         `1`="Terrestrial")) %>%
  droplevels()

###########################################
# body size between aquatic and terrestrial
p1 = ggplot(subset(x, !is.na(adMort) & !is.na(svl) & !is.na(AdTerr) & !is.na(repmode)), aes(x = repmode, y = log(svl))) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.8, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
   coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Body Size") +
  xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(~AdTerr)

###########################################
# adult mortality between aquatic and terrestrial
p2 = ggplot(subset(x, !is.na(adMort) & !is.na(svl) & !is.na(AdTerr) & !is.na(repmode)), aes(x = repmode, y = adMort)) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.8, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Adult Mortality") +
  xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(~AdTerr)



ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

###########################################
# offspring size between aquatic and terrestrial
p3 = ggplot(subset(x, !is.na(birth.svl) & !is.na(juvMort) & !is.na(Terr) & !is.na(repmode)), aes(x = repmode, y = log(birth.svl))) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 1, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Offspring Size") +
  xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_grid(~Terr)

###########################################
# offspring mortality between aquatic and terrestrial
p4 = ggplot(subset(x, !is.na(birth.svl) & !is.na(juvMort) & !is.na(Terr) & !is.na(repmode)), aes(x = repmode, y = juvMort)) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 1, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Offspring Mortality") +
  xlab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_grid(~Terr)



ggarrange(p3, p4, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)



###########################################
# relative offspring size between aquatic and terrestrial
p5 = ggplot(subset(x, !is.na(svl) & !is.na(birth.svl) & !is.na(juvMort) & !is.na(Terr) & !is.na(repmode)), aes(x = repmode, y = birth.svl/svl)) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.8, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Relative Offspring Size") +
  xlab("") + ylim(0,1) +
  facet_grid(~Terr)

###########################################
# relative offspring mortality between aquatic and terrestrial
p6 = ggplot(subset(x, !is.na(svl) & !is.na(birth.svl) & !is.na(juvMort) & !is.na(Terr) & !is.na(repmode)), aes(x = repmode, y = adMort/juvMort)) + 
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.8, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95),
    justification = -.1
  ) +
  coord_cartesian(xlim = c(1.2, NA)) +
  theme_Publication() +
  ylab("Relative Offspring Survival") +
  xlab("") + ylim(0,1) +
  facet_grid(~Terr)

ggarrange(p5, p6, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)


##############################
# age at maturity by longevity
ggplot(x, aes(log(AaM), log(longevity), color = class)) +
  geom_point() +
  geom_smooth(method = "glm") +
  xlab("Age at Maturity") + ylab("Longevity") +
  theme_Publication() + theme(
    axis.text = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggplot(x, aes(log(AaM), juvMort, color = class)) +
  geom_point() +
  geom_smooth(method = "glm") +
  xlab("Age at Maturity") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")



###################################
# lifetime mortality by class
x$adMortScal = scale(x$adMort)
x$juvMortScal = scale(x$juvMort)
x$totMort = x$adMortScal + x$juvMortScal

ggplot(subset(x, !is.na(repmode)), aes(class, juvMort, col = class, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Offspring Mortality") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')

ggplot(subset(x, !is.na(repmode)), aes(class, adMort, col = class, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Offspring Mortality") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')

ggplot(x, aes(class, totMort, col = class, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Lifetime Mortality") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  stat_n_text() + 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')


#####################
# body size by class
ggplot(x, aes(class, log(svl), col = class)) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  stat_n_text() + 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')




# scatter plot, color by order (color by clas also looks good), densities plotted on edges
ggscatterhist(subset(x, !is.na(adMort) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), x = "juvMort", y = "adMort",
              color = "repmode",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality",
              margin.params = list(fill = "repmode", color = "black", size = 0.2), ellipse = F
)

ggscatterhist(subset(x, !is.na(adMort) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), x = "juvMort", y = "adMort",
              color = "class",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "class", color = "black", size = 0.2), ellipse = F
) 


ggscatterhist(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), x = "juvMort", y = "adMort",
              color = "AdTerr",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "AdTerr", color = "black", size = 0.2), ellipse = F
) 

ggscatterhist(subset(x, !is.na(adMort) & !is.na(Terr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), x = "juvMort", y = "adMort",
              color = "Terr",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "Terr", color = "black", size = 0.2), ellipse = F
) 



# relationship between max size and adult mortality (!!! have mass for some, lenght for others)
ggplot(subset(x, !is.na(adMort) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(adMort), colour = class)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")


# relationship between max size and adult mortality (!!! have mass for some, lenght for others)
ggplot(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(adMort), colour = repmode)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2") + 
  facet_wrap(~AdTerr)


# relationship between juv size and juv mortality (!!! have mass for some, lenght for others)
ggplot(subset(x, !is.na(adMort) & !is.na(Terr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(juvMort), colour = repmode)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Offspring Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2") + 
  facet_wrap(~Terr)


ggplot(subset(x, !is.na(adMort) & !is.na(Terr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(totMort), colour = class)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Lifetime Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")




#############################
# size vs mortality rgressions by habitat
reg1 = ggplot(subset(x, !is.na(adMort) & !is.na(Terr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(juvMort), colour = Terr)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("") + ylab("Offspring Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "null")+ 
  scale_color_brewer(palette="Dark2")


reg2 = ggplot(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(adMort), colour = AdTerr)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "null")+ 
  scale_color_brewer(palette="Dark2")


reg3 = ggplot(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(totMort), colour = Terr)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Lifetime Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")

ggarrange(reg1, reg2, reg3, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


#############################
# size vs mortality rgressions by parity
reg1 = ggplot(subset(x, !is.na(adMort) & !is.na(Terr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(juvMort), colour = repmode)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("") + ylab("Offspring Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "null")+ 
  scale_color_brewer(palette="Dark2") +
  facet_wrap(~Terr)


reg2 = ggplot(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(adMort), colour = repmode)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "null")+ 
  scale_color_brewer(palette="Dark2")+
  facet_wrap(~AdTerr)


reg3 = ggplot(subset(x, !is.na(adMort) & !is.na(AdTerr) & !is.na(juvMort) & !is.na(svl) & !is.na(repmode)), aes(x = log(svl), y = jitter(totMort), colour = repmode)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Lifetime Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")+
  facet_wrap(~AdTerr)

ggarrange(reg1, reg2, reg3, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)
