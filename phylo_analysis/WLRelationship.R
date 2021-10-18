library(dplyr)

lens <- read.csv("../datasets/VTparity_length_mass.csv")
#taking the mean for each species
length(lens$Species)
g <- aggregate(cbind(lens$a, lens$b), by = list(lens$Species), mean)
colnames(g) <- c("Species", "a", "b")
shark_lens <- read.csv("../datasets/SharkData.csv")
shark_lens <- select(shark_lens, c(4,15,16))
colnames(shark_lens) <- c("Species",  "a","b")
lens_attempt <- merge(lens, shark_lens, by = "Species")
colnames(lens_attempt) <- c("Species","X", "a1","b1","a","b")

# changing values when they are different
ind <- which(lens_attempt$a1 != lens_attempt$a)
change <- lens_attempt[ind,]
lens_shark <- data.frame(lens$Species, lens$a, lens$b)
colnames(lens_shark) <- c("species","a","b")
#old_dat <- read.csv("../datasets/vtparity.csv")
dat <- read.csv("../datasets/vtparity.csv")
dat <- data.frame(dat)

#merging the two 
#full_dat <- merge(dat, lens_shark, by = "species", all.x = T)

g <- data.frame(g)
names(g) <- c("species", "a","b")

# combo of both datasets 
get <- plyr::join(g, dat, by = 'species', type = "full")

#get_mass <- mutate(get, mass_est = a * (svl^b))

#converting 
get$a <- ifelse(get$a < 0, yes = exp(get$a), get$a)

transformed_dat <- saveRDS(get, file = "../output/transformed_dat.RDS")

get_mass_con <- mutate(get, lnSVL = log(svl), logMass = log(mass))

get_mass_jo <- mutate(get_mass_con, logEstMass = b * lnSVL + log((a)))

#saving 
saveRDS(get_mass_jo, file = "../output/NB_massEst.RDS")
mass_est <- readRDS("../output/NB_massEst.RDS")

lm1 <- lm(mass_est$logEstMass / 3 ~  mass_est$lnSVL)
#main plot
plot(mass_est$lnSVL, mass_est$logEstMass / 3, pch = 16, cex = 1.3, col = "red", 
     main = "log(estMass)/3 and lnSVL", xlab = "lnSVL", ylab = " log(estMass) / 3")
abline(lm1)

# dropping based on residual
lm1.res <- (resid(lm1))
plot(lm1.res)
res_ind <- (which((lm1.res) < -2))
r <- as.data.frame(res_ind)
q <- as.numeric(rownames(r))
res_ind <- c(r$res_ind, q)
# outliers based on the residuals, not sure whether the reptiles should be dropped
res_out <- mass_est[res_ind,]

# dropped dataset 
mass_est <- mass_est[-res_ind, ]
saveRDS(mass_est, file = "../output/NB_massEst_NoOutliers.RDS")

par(mfrow = c(2,1))
hist(get_mass_jo$a)
hist(res_out$a)


# things to be dropped based on svl and estMass
ind_out <- which(get_mass_jo$logEstMass < 0 & get_mass_jo$lnSVL > 2.5)
eye_out <- get_mass_jo[ind_out,]

par(mfrow = c(2,1))
hist(eye_out$a)
hist(res_out$a)




