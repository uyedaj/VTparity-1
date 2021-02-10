
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



ggplot(subset(x, !is.na(AdTerr)), aes(AdTerr, log(svl), col = AdTerr)) +
  geom_sina() +
  xlab("") + ylab("Body Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") 


ggplot(subset(x, !is.na(AdTerr)), aes(AdTerr, adMort, col = AdTerr)) +
  geom_sina() +
  xlab("") + ylab("Adult Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") 


ggplot(subset(x, !is.na(Terr)), aes(Terr, log(birth.svl), col = Terr)) +
  geom_sina() +
  xlab("") + ylab("Offspring Size") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") 


ggplot(subset(x, !is.na(Terr)), aes(Terr, juvMort, col = Terr)) +
  geom_sina() +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") 


ggplot(subset(x, !is.na(repmode)), aes(Terr, juvMort, col = Terr)) +
  geom_sina() +
  xlab("") + ylab("Juvenile Mortality") +
  theme_Publication() + theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+ 
  scale_color_brewer(palette = "Dark2") +
  facet_grid(~repmode)




size.t = 2:11
uT = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.03, 0.01, 0.005)
gT = c(0.01, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.07, 0.06, 0.04)

size.a = 1:20
uA = c(0.9, 0.9, 0.8, 0.7, 0.5, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 0.05, 0.05, 0.03, 0.03, 0.01, 0.01, 0.01)
gA = c(0.1, 0.3, 0.9, 0.8, 0.7, 0.7, 0.6, 0.5, 0.5, 0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.05, 0.01, 0.008, 0.006)


plot(size.a, uA/gA)
lines(size.t, uT/gT)


op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
          font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

par(mfrow = c(3,2))

yhigh <- 0.6
xlow <- -3
xhigh <- 3
postmean <- 0.5
postsd <- 0.8
priormean <- 0
priorsd <- 1

plot(function(x) dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                               yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Size at Metamorphosis", 
     axes = FALSE, col = "dark red")
lines(c(0, 0), c(0, 1.25), lwd = 2, col = "grey")

par(new = TRUE)

plot(function(x) dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                 yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, col = "dark blue", ylab = "", xlab = "", axes = FALSE)
axis(1, labels = F)
axis(2, labels = F)
par(las = 0)
mtext("Size", side = 1, line = 2.5, cex = 1.5)
mtext("Rate (g/mu)", side = 2, line = 3, cex = 1.8)



yhigh <- 0.6
xlow <- -3
xhigh <- 3
postmean <- 0
postsd <- 0.7
priormean <- 0
priorsd <- 1

plot(function(x) 0.75*dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                               yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Paedomorphosis", 
     axes = FALSE, col = "dark red")

par(new = TRUE)

plot(function(x) 1.25*dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                 yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, col = "dark blue", ylab = "", xlab = "", axes = FALSE)
axis(1, labels = F)
axis(2, labels = F)
par(las = 0)
mtext("Size", side = 1, line = 2.5, cex = 1.5)
mtext("Rate (g/mu)", side = 2, line = 3, cex = 1.8)


yhigh <- 0.6
xlow <- -3
xhigh <- 3
postmean <- 0
postsd <- 0.7
priormean <- 0
priorsd <- 1

plot(function(x) 0.75*dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                                    yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Direct Development", 
     axes = FALSE, col = "dark blue")

par(new = TRUE)

plot(function(x) 1.25*dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                      yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, col = "dark red", ylab = "", xlab = "", axes = FALSE)
axis(1, labels = F)
axis(2, labels = F)
par(las = 0)
mtext("Size", side = 1, line = 2.5, cex = 1.5)
mtext("Rate (g/mu)", side = 2, line = 3, cex = 1.8)



yhigh <- 0.6
xlow <- -3
xhigh <- 3
postmean <- 0
postsd <- 0.8
priormean <- 0
priorsd <- 1

plot(function(x) dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                                    yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Newts", 
     axes = FALSE, col = "dark red")

par(new = TRUE)

plot(function(x) dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                      yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, col = "dark blue", ylab = "", xlab = "", axes = FALSE)
axis(1, labels = F)
axis(2, labels = F)
par(las = 0)
mtext("Size", side = 1, line = 2.5, cex = 1.5)
mtext("Rate (g/mu)", side = 2, line = 3, cex = 1.8)


yhigh <- 1
xlow <- -3
xhigh <- 3
postmean <- -0.8
postsd <- 0.6
priormean <- 0
priorsd <- 1

plot(function(x) dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
                                                                               yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Glass Frogs", 
     axes = FALSE, col = "dark blue")

par(new = TRUE)

plot(function(x) 1.25*dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
                                                                                      yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, col = "dark red", ylab = "", xlab = "", axes = FALSE)
axis(1, labels = F)
axis(2, labels = F)
par(las = 0)
mtext("Size", side = 1, line = 2.5, cex = 1.5)
mtext("Rate (g/mu)", side = 2, line = 3, cex = 1.8)

plot.new()

legend("center", # position
       legend = c("Aquatic", "Terrestrial"), 
       lty = 1,
       lwd = 3,
       col = c("dark blue", "dark red"),
       cex = 2,
       bty = "n") # border







library(ggplot2)
library(tidyverse)

data <- data.frame(
  x = rnorm(10000),
  y = rnorm(10000, mean=6)
)

data %>%
  ggplot( aes(x) ) +
  geom_density( aes(x = x, y = -..density..), binwidth = diff(range(data$x))/30, fill="#404080" ) +
  geom_density( aes(x = y, y = ..density..), binwidth = diff(range(data$x))/30, fill= "#69b3a2") +
  theme_bw() +
  xlim(0,10)

data.sal <- data.frame(
  x = rnorm(10000),
  y = rnorm(10000, mean=10, sd = 3)
)

data.sal %>%
  ggplot( aes(x) ) +
  geom_density( aes(x = x, y = -..density..), binwidth = diff(range(data$x))/30, fill="#404080" ) +
  geom_density( aes(x = y, y = ..density..), binwidth = diff(range(data$x))/30, fill= "#69b3a2") +
  theme_bw() +
  xlim(0,20)

data.newt <- data.frame(
  x = rnorm(10000),
  y = rnorm(10000, mean=6),
  z = rnorm(10000, mean=12)
)

data.newt %>%
  ggplot( aes(x) ) +
  geom_density( aes(x = x, y = -..density..), binwidth = diff(range(data$x))/30, fill="#404080" ) +
  geom_density( aes(x = y, y = ..density..), binwidth = diff(range(data$x))/30, fill= "#69b3a2") +
  geom_density( aes(x = z, y = -..density..), binwidth = diff(range(data$x))/30, fill="#404080" ) +
  theme_bw() +
  xlim(0,20)
