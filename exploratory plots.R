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

# gg plot themes to make neat figures
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}



dat = x %>%
  filter(order != "Rhynchocephalia")%>%
  filter(order != "Gymnophiona") %>%
  filter(class != "Aves") %>%
  filter(class != "Mammalia")


ggplot(dat, aes(order, log(svl))) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

ggplot(dat, aes(order, log(birth.svl), col = order)) +
  geom_sina() +
  xlab("") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(order, log(reloff), col = order, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Offspring:Adult Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")+ 
  scale_size(guide = 'none')

ggplot(dat, aes(order, log(RCM), col = order, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Relative Clutch Mass") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")+ 
  scale_size(guide = 'none')

ggplot(dat, aes(order, log(IPB), col = order, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Relative Investment Per Clutch") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")+ 
  scale_size(guide = 'none')

ggplot(dat, aes(order, juvMort, col = repmode)) +
  geom_sina() +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(order, adMort, col = repmode)) +
  geom_sina() +
  xlab("") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(order, log(juvMort/adMort), size = sqrt(svl))) +
  geom_sina() +
  xlab("") + ylab("Juvenile mortality relative to adult mortality") +
  theme_Publication() + theme(
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(order, juvMort, size = log(svl))) +
  geom_sina() +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggplot(dat, aes(log(reloff), log(juvMort/adMort), color = class)) +
  geom_point() +
  geom_smooth(method = "glm") +
  xlab("Relative Offspring Size") + ylab("Relative Offspring Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")



dat$order = factor(dat$order, levels = c("Fish", "Sharks", "Anura", "Caudata", "Crocodilia", "Testudines", "Squamata", "Aves", "Mammalia"))


#ggplot(dat, aes(log(AaM), log(longevity), color = class)) +
#  geom_point() +
#  geom_smooth(method = "glm") +
#  xlab("Age at Maturity") + ylab("Longevity") +
#  theme_Publication() + theme(
#    axis.text = element_blank(),
#    axis.ticks = element_blank())+ 
#  scale_color_brewer(palette = "Dark2")

#ggplot(x, aes(log(AaM), juvMort, color = class)) +
#  geom_point() +
#  geom_smooth(method = "glm") +
#  xlab("Age at Maturity") + ylab("Juvenile Mortality") +
#  theme_Publication() + theme(
#    axis.text.y = element_blank(),
#    axis.text.x = element_blank(),
#    axis.ticks = element_blank())+ 
#  scale_color_brewer(palette = "Dark2")

ggplot(x, aes(adMort, juvMort, color = class)) +
  geom_point() +
  xlab("Adult Mortality") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(order, juvMort, col = repmode)) +
  geom_boxplot(lwd=1.5) +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) + 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(order, log(svl), col = repmode)) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggplot(subset(dat, !is.na(repmode)), aes(order, log(reloff), col = repmode)) +
  geom_boxplot(lwd=1.5) +
  xlab("") + ylab("Offspring:Adult Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(order, log(svl-birth.svl), col = order, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Lifetime Growth") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") + 
  scale_size(guide = 'none')


ggplot(subset(dat, !is.na(repmode)), aes(order, log(birth.svl), col = repmode)) +
  geom_boxplot(lwd=1.5) +
  xlab("") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") 

ggplot(subset(dat, !is.na(repmode)), aes(order, log(svl), col = repmode)) +
  geom_boxplot(lwd=1.5) +
  xlab("") + ylab("Adult Size") +
  theme_Publication() + theme(
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")+
  ylim(c(0,11))

ggplot(subset(dat, !is.na(repmode)), aes(order, log(svl-birth.svl), col = repmode)) +
  geom_boxplot(lwd=1.5) +
  xlab("") + ylab("Lifetime Growth") +
  theme_Publication() + theme(
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(order, log(svl-birth.svl), col = repmode, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Lifetime Growth") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

x$adMortScal = scale(x$adMort)
x$juvMortScal = scale(x$juvMort)
x$totMort = x$adMortScal + x$juvMortScal

ggplot(subset(x, !is.na(repmode)), aes(class, totMort, col = class, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Lifetime Mortality") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") +
  scale_size(guide = 'none')

ggplot(subset(dat, !is.na(repmode)), aes(order, juvMort, col = repmode, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Lifetime Reproductive Output") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(order, adMort, col = repmode, size = svl)) +
  geom_sina() +
  xlab("") + ylab("Adult Mortality") +
  theme_Publication() + theme(    
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


dat2 = x %>%
  filter(class == "Amphibia" | order == "Squamata")%>% 
  mutate(repmode = recode(repmode, 
                          `1`="Viviparity / DD",
                          `0`="Oviparity / L")) %>%
  droplevels()


ggplot(subset(dat2, !is.na(repmode)), aes(class, juvMort, col = repmode)) +
  geom_boxplot(width=1.5,lwd=1.5, varwidth = T) +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) + 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(x, !is.na(repmode)), aes(order, log(reloff), col = repmode)) +
  geom_boxplot(width=1.5,lwd=1.5, varwidth = T) +
  xlab("") + ylab("Offspring:Adult Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


# filter out depauperate clades that will mess up density plots
dat = x %>%
  filter(order != "Gymnophiona") %>%
  filter(order != "Rhynchocephalia") %>%
  mutate(repmode = recode(repmode, 
                        `Dir`="1",
                        `Viv`="1",
                        `Lar`="0")) %>%
  droplevels()

which(dat$repmode == "NA")

# scatter plot, color by order (color by clas also looks good), densities plotted on edges
ggscatterhist(dat, x = "juvMort", y = "adMort",
              color = "repmode",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "repmode", color = "black", size = 0.2), ellipse = F
) 

ggscatterhist(dat, x = "juvMort", y = "adMort",
              color = "class",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "class", color = "black", size = 0.2), ellipse = F
) 

# same as above, but size of points related to body size (!!! not all records have size, so may omit points)
ggscatterhist(dat, x = "juvMort", y = "adMort",
              color = "order", size = log(dat$svl)/3, 
              palette = "jco",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "order", color = "black", size = 0.2), ellipse = F
) 


# same as above, but color by repmode
ggscatterhist(dat, x = "juvMort", y = "adMort",
              color = "repmode", shape = "class", 
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "repmode", color = "black", size = 0.2), ellipse = F
) 

# visualizing the ecto endo split by how taxa partition juvenile mortality
# fill by order alos interesting
ggplot(dat, aes(juvMort, stat(count), fill = class)) +
  geom_density(position = "fill")+
  xlab("Juvenile Mortality") + ylab("Species Proportion") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Paired")

# same as above, but raw densities for perspective
ggplot(dat, aes(juvMort, stat(count), fill = class)) +
  geom_density(position = "stack")+
  xlab("Juvenile Mortality") + ylab("Stacked Densities") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Paired")

# visualizing the ecto endo split by how taxa partition adult mortality
ggplot(dat, aes(adMort, stat(count), fill = class)) +
  geom_density(position = "fill")+
  xlab("Adult Mortality") + ylab("Species Proportion") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Paired")

# same as above, but raw densities for perspective
ggplot(dat, aes(adMort, stat(count), fill = class)) +
  geom_density(position = "stack")+
  xlab("Adult Mortality") + ylab("Species Proportion") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Paired")


# relationship between max size and adult mortality (!!! have mass for some, lenght for others)
ggplot(x, aes(x = jitter(adMort), y = log(svl), colour = class)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Adult Mortality") + ylab("Maximum Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")

ggplot(x, aes(x = log(svl), y = log(birth.svl), colour = repmode, shape = class)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Max Size") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")

ggplot(dat, aes(x = log(svl), y = log(birth.svl), shape = class, colour = repmode)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Maximum Size") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2")

dat2 = x %>%
  filter(class == "Amphibia" | order == "Squamata")

ggplot(x, aes(x = juvMort, y = log(birth.svl), colour = repmode, size = svl)) +
  geom_point() +
  xlab("Juvenile Mortality") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2") +
  scale_shape(guide = 'none') +
  scale_size(guide = 'none') +
  facet_grid(~class)


ggplot(dat, aes(x = log(dat$Litter_size_mean), y = log(dat$Offspring_size_mean), colour = RepMode, shape = Order, size = dat$Offspring_size_mean)) +
  geom_point() +
  xlab("Litter Size") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Dark2") +
  scale_shape(guide = 'none') +
  scale_size(guide = 'none') 
  

dat$class = factor(dat$class, levels = c("Osteichthyes", "Amphibia", "Reptilia", "Aves", "Mammalia"))
comps = list(c("Osteichthyes", "Amphibia"), 
             c("Amphibia", "Reptilia"), 
             c("Reptilia", "Aves"), 
             c("Aves", "Mammalia"),
             c("Osteichthyes", "Reptilia"),
             c("Osteichthyes", "Aves"),
             c("Osteichthyes", "Mammalia"),
             c("Amphibia", "Aves"),
             c("Amphibia", "Mammalia"),
             c("Reptilia", "Mammalia"))

ggplot(subset(dat, !is.na(repmode)), aes(class, juvMort, col = repmode)) +
  geom_boxplot(lwd = 1.5)+
  ylab("Juvenile Mortality") + xlab("Order") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) + 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(class, adMort, col = repmode)) +
  geom_boxplot(lwd = 1.5)+
  ylab("Adult Mortality") + xlab("Order") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(subset(dat, !is.na(repmode)), aes(class, log(svl), col = repmode)) +
  geom_boxplot(lwd = 1.5)+
  ylab("Body Size") + xlab("Order") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


mod = lm(juvMort ~ adMort + svl + class, data = dat)
summary(mod)


ggplot(subset(dat, !is.na(repmode)), aes(log(svl), juvMort, col = order)) +
  geom_point()+
  geom_smooth(method = "lm") +
  xlab("Body Size") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggplot(subset(dat, !is.na(repmode)), aes(log(svl), adMort, col = order)) +
  geom_point()+
  geom_smooth(method = "gam") +
  xlab("Body Size") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")
