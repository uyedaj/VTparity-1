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

setwd("I:/VTech/Holly/Data")

dat = read.csv("AmphiBIO_v1.csv")
#dat = dat[complete.cases(dat),]

# calculate mean values for ranges provided
dat$AaM = ((dat$Age_at_maturity_min_y + dat$Age_at_maturity_max_y) / 2)
dat$Litter_size_mean = ((dat$Litter_size_min_n + dat$Litter_size_max_n) / 2)
dat$Offspring_size_mean = ((dat$Offspring_size_min_mm + dat$Offspring_size_max_mm) / 2)
dat$SS = ((dat$Age_at_maturity_max_y - dat$Age_at_maturity_min_y)) / dat$AaM
dat$SaM = ((dat$Size_at_maturity_max_mm + dat$Size_at_maturity_min_mm) / 2)
dat$reloff = dat$Body_size_mm - dat$Offspring_size_mean
dat$relmat = dat$AaM / dat$Longevity_max_y
dat$relsmat = dat$SaM / dat$Body_size_mm

dat$gtm = dat$relsmat - dat$relmat

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
dat[which(dat[,39] == 0), 39] = NA


repmode = dat[,c(1,35:37,39)] %>% gather(type, value, -id) %>% na.omit() %>% select(-value)%>% arrange(id)

dat = left_join(dat, repmode)
dat$RepMode = as.factor(dat$type)

summary(dat$RepMode)
# create relative cluss match column
dat$RCM = ((dat$Offspring_size_mean^3) * dat$Litter_size_mean) / (dat$Body_size_mm^3)

# create investment per bout column
dat$IPB = dat$Litter_size_mean / (1/dat$juvSurv)

dat$reloff = log(dat$Body_size_mm / dat$Longevity_max_y)

dat = dat %>% 
  mutate(repmode = recode(RepMode, 
                          `Viv`="Dir"))




###################
# Plotting
ggplot(dat, aes(order, log(svl), col = order)) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(Order, log(Offspring_size_mean), col = Order)) +
  geom_sina() +
  xlab("") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(Order, juvMort, col = Order)) +
  geom_sina() +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")+ 
  scale_size(guide = 'none')

ggplot(dat, aes(juvMort, birth.svl, col = repmode, size = svl, shape = order)) +
  geom_point() +
  xlim(3, 15) +
  ylim(0, 15) +
  xlab("Juvenile Mortality (1/lifetime egg production)") + ylab("Propagule Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(log(Offspring_size_mean), log(Litter_size_mean), col = RepMode, size = dat$Body_size_mm, shape = Order)) +
  geom_point() +
  xlab("Propagule Size") + ylab("Litter Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggplot(amphs, aes(adMort, log(birth.svl), col = repmode, size = svl, shape = order)) +
  geom_point() +
  xlab("Adult Mortality (1/age at maturity)") + ylab("Propagule Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")




cats = read.csv("amphibian_IUCN.csv")

cats = cats %>%
  select(scientific_name, category)

names(cats) = c("Species", "IUCN")

ddat = merge(dat, cats, by = "Species")

ddat$IUCN = as.factor(ddat$IUCN)

table(ddat$RepMode, ddat$IUCN)

table(ddat$RepMode, ddat$Order)

ddat = melt(table(ddat$RepMode, ddat$IUCN)/sum(table(ddat$RepMode, ddat$IUCN)))

categories = c("EX", "CR", "EN", "VU", "NT", "LC", "DD")
dd = c(5, 171, 273, 195, 110, 433, 281)
prop.dd = dd/sum(dd)
lar = c(9, 226, 381, 278, 165, 1645, 517)
prop.lar = lar/sum(lar)
peds = c(0, 2, 2, 1, 3, 13, 0)
prop.peds = peds/sum(peds)
viv = c(1, 6, 7, 2, 0, 10, 19)
prop.viv = viv/sum(viv)


# counts for fig titles
rowSums(table(ddat$RepMode, ddat$IUCN))


# plotting
op <- par(mfrow = c(2, 2), mar = c(5.9, 6, 4, 2) + 0.1)
iucnpal <-c("#000000", "#D81E05", "#FC7F3F", "#F9E814", "#CCE226", "#60C659", "#D1D1C6")

barplot(height = prop.dd, names.arg = categories, col = iucnpal, ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, main = "Direct Developers (n = 1468)")
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)


barplot(height = prop.lar, names.arg = categories, col = iucnpal, ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, main = "Aquatic Larvae (n = 3221)")
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)
par(las = 0)

barplot(height = prop.peds, names.arg = categories, col = iucnpal, ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, main = "Paedomorphic / Neotony (n = 21)")
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)
par(las = 0)

barplot(height = prop.viv, names.arg = categories, col = iucnpal, ylim = c(0, 0.6), axes = FALSE, width = 1, cex.names = 1.4, main = "Live-bearing (n = 45)")
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)


legend("topleft", legend=c("DD", "LC", "NT", "VU", "EN", "CR", "EX"), text.col=iucnpal[1:7], cex=1.1, bty="n")


# chi sq tests
exp.dd = prop.lar * sum(dd)
exp.peds = prop.lar * sum(peds)
exp.viv = prop.lar * sum(viv)

M <- as.table(rbind(lar, dd, peds, viv))
dimnames(M) <- list(repmode = c("Aquatic Larvae", "Direct Development", "Neotony", "Viviparity"),
                    IUCN = categories)
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

op <- par(mfrow = c(1, 1), mar = c(5.9, 6, 4, 2) + 0.1)
corrplot(Xsq$residuals, is.cor = FALSE)

contrib <- 100*Xsq$residuals^2/Xsq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)




# plotting
op <- par(mfrow = c(2, 2))

barplot(height = prop.dd, names.arg = categories)
barplot(height = prop.lar, names.arg = categories)
barplot(height = prop.peds, names.arg = categories)
barplot(height = prop.viv, names.arg = categories)




dat2 = dat %>%
  filter(repmode != "NA")


ggplot(dat2, aes(reloff, juvMort, col = repmode, shape = repmode)) +
  geom_point() +
  xlab("Body Size") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")


ggscatterhist(dat2, x = "reloff", y = "juvMort",
              color = "repmode",
              palette = "Dark2",xlab = "Lifetime Growth", ylab = "Juvenile Mortality", 
              margin.params = list(fill = "repmode", color = "black", size = 0.2), ellipse = F
)





ggplot(dat, aes(Order, log(reloff), col = repmode)) +
  geom_sina() +
  xlab("") + ylab("Offspring:Adult Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2")
