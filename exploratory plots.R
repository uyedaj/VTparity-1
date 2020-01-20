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

setwd("C:/Users/George/Documents/Holly")

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

# filter out depauperate clades that will mess up density plots
dat = x %>%
  filter(order != "Gymnophiona") %>%
  filter(order != "Rhynchocephalia") %>%
  mutate(repmode = recode(repmode, 
                        `Dir`="1",
                        `Viv`="1",
                        `Lar`="0")) %>%
  droplevels()


# scatter plot, color by order (color by clas also looks good), densities plotted on edges
ggscatterhist(dat, x = "juvMort", y = "adMort",
              color = "order",
              palette = "Dark2",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
              margin.params = list(fill = "order", color = "black", size = 0.2), ellipse = F
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
              palette = "jco",xlab = "Juvenile Mortality", ylab = "Adult Mortality", 
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
ggplot(dat, aes(x = jitter(adMort), y = log(svl), colour = class)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Adult Mortality") + ylab("Maximum Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette="Paired")


