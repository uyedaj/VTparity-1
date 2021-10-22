rm(list=ls(all=TRUE))
library(Ternary)
library(MCMCpack)
library(combinat)
library(dplyr)
library(castor)
tree <- geiger::sim.bdtree(b=1, stop="taxa", n=5000)

dat <- rdirichlet(100, c(1,1,1))

bins <- 4
bins2 <- bins^2
bins4 <- 2*bins2

TernaryBin <- function(xyz, bins){
  bins2 <- bins^2
  bins4 <- 2*bins2
  combos <- combinat::combn(rep(1:bins,3), 3) ## Create all combinations of values 1:bins
  sums <- apply(combos, 2, sum) #Sum the 3 values for the 3 traits
  combos <- combos[,sums %in% c(bins+2, bins+1)] #Only retain those that sum to bins+2 and bins+1 (fit in the triangle)
  cells <- t(combos[,!duplicated(apply(combos, 2,paste, collapse=""))]) #remove duplicated bin combinations
  colnames(cells) <- c("A", "B", "C") #label traits
  cells <- cells[order(cells[,3],(bins+1)-cells[,1],(bins+1)-cells[,2], decreasing=TRUE),] #reorder the cells to be prettier
  cellnames <- apply(cells, 1, paste, collapse="") #Give the cells a state label  
  ddat <- apply(xyz, 2, cut,breaks=seq(0,1, length.out=bins+1), labels=1:bins) #Find where the data falls in the state space
  ddat <- match(apply(ddat, 1, paste, collapse=""), cellnames) #match the data's state to the state label
  ddat 
}

combos <- combinat::combn(rep(1:bins,3), 3) ## Create all combinations of values 1:bins
sums <- apply(combos, 2, sum) #Sum the 3 values for the 3 traits
combos <- combos[,sums %in% c(bins+2, bins+1)] #Only retain those that sum to bins+2 and bins+1 (fit in the triangle)
cells <- t(combos[,!duplicated(apply(combos, 2,paste, collapse=""))]) #remove duplicated bin combinations
colnames(cells) <- c("A", "B", "C") #label traits
cells <- cells[order(cells[,3],(bins+1)-cells[,1],(bins+1)-cells[,2], decreasing=TRUE),] #reorder the cells to be prettier
cellnames <- apply(cells, 1, paste, collapse="") #Give the cells a state label  


ddat <- TernaryBin(dat, bins)
 

Ternary::TernaryPlot(alab="A", blab="B", clab="C", grid.lines = 3, grid.minor.lines = 1) #Plot with points labeled by their numeric state value (values with more than two digits won't show up right)
TernaryPoints(dat, pch=as.character(ddat)) 

r <- c(0,-1,2,-3) #3 directions of gradients (1-3 = A-C) and the final one is a center attractor/repulsor

r2Q <- function(r,c,bins){
  bins2 <- bins^2
  bins4 <- 2*bins2
  combos <- combinat::combn(rep(1:bins,3), 3) ## Create all combinations of values 1:bins
  sums <- apply(combos, 2, sum) #Sum the 3 values for the 3 traits
  combos <- combos[,sums %in% c(bins+2, bins+1)] #Only retain those that sum to bins+2 and bins+1 (fit in the triangle)
  cells <- t(combos[,!duplicated(apply(combos, 2,paste, collapse=""))]) #remove duplicated bin combinations
  colnames(cells) <- c("A", "B", "C") #label traits
  cells <- cells[order(cells[,3],(bins+1)-cells[,1],(bins+1)-cells[,2], decreasing=TRUE),] #reorder the cells to be prettier
  cellnames <- apply(cells, 1, paste, collapse="") #Give the cells a state label  
  Q <- as.matrix(dist(cells))
  Q[Q!=1] <- 0
  non0rates <- which(Q != 0, arr.ind = T)
  tQ <- Q
  centers <- t(Ternary:::TriangleCentres(bins))
  cells <-t(Ternary:::XYToTernary(centers[,1], centers[,2]))
  labels <- TernaryBin(cells, bins)
  centers <- centers[order(labels),]
  midcells <- cells[order(labels),]
  abscenter <- c(1/3, 1/3, 1/3)
  for(i in 1:nrow(non0rates)){
    d2 <- sum((midcells[non0rates[i,2], ] - abscenter)^2)
    d1 <- sum((midcells[non0rates[i,1], ] - abscenter)^2)
    tQ[non0rates[i,1], non0rates[i,2]] <- c*exp(sum(r[1:3]*(midcells[non0rates[i,2], ] - midcells[non0rates[i,1], ])) + (d2-d1)*r[4])
  }
  diag(tQ) <- -1*apply(tQ, 1, sum)
  return(tQ)
}

simulateTernaryData <- function(tree, r, c, bins, line.weight=10, plot.points=TRUE, ...){
  bins2 <- bins^2
  bins4 <- 2*bins2
  combos <- combinat::combn(rep(1:bins,3), 3) ## Create all combinations of values 1:bins
  sums <- apply(combos, 2, sum) #Sum the 3 values for the 3 traits
  combos <- combos[,sums %in% c(bins+2, bins+1)] #Only retain those that sum to bins+2 and bins+1 (fit in the triangle)
  cells <- t(combos[,!duplicated(apply(combos, 2,paste, collapse=""))]) #remove duplicated bin combinations
  colnames(cells) <- c("A", "B", "C") #label traits
  cells <- cells[order(cells[,3],(bins+1)-cells[,1],(bins+1)-cells[,2], decreasing=TRUE),] #reorder the cells to be prettier
  cellnames <- apply(cells, 1, paste, collapse="") #Give the cells a state label  
  Q <- as.matrix(dist(cells))
  Q[Q!=1] <- 0
  non0rates <- which(Q != 0, arr.ind = T)
  tQ <- Q
  centers <- t(Ternary:::TriangleCentres(bins))
  cells <-t(Ternary:::XYToTernary(centers[,1], centers[,2]))
  labels <- TernaryBin(cells, bins)
  
  #Ternary::TernaryPlot(alab="A", blab="B", clab="C", grid.lines = 3, grid.minor.lines = 0) #Plot with points labeled by their numeric state value (values with more than two digits won't show up right)
  #points(centers, pch=as.character(labels))
  centers <- centers[order(labels),]
  midcells <- cells[order(labels),]
  abscenter <- c(1/3, 1/3, 1/3)
  Ternary::TernaryPlot(alab="A", blab="B", clab="C", grid.lines = bins, grid.minor.lines = 0,...) #Plot with points labeled by their numeric state value (values with more than two digits won't show up right)
  for(i in 1:nrow(non0rates)){
    #Q[non0rates[i,1], non0rates[i,2]] <- (
    #  r[4]^(sum((midcells[non0rates[i,2], ] - abscenter)^2) - sum((midcells[non0rates[i,1], ] - abscenter)^2))*
    #  sum(r[1:3]^(bins*(midcells[non0rates[i,2], ] - midcells[non0rates[i,1], ]))*abs((midcells[non0rates[i,2], ] - midcells[non0rates[i,1], ])))*
    #  c
    #  )
    d2 <- sum((midcells[non0rates[i,2], ] - abscenter)^2)
    d1 <- sum((midcells[non0rates[i,1], ] - abscenter)^2)
    tQ[non0rates[i,1], non0rates[i,2]] <- c*exp(sum(r[1:3]*(midcells[non0rates[i,2], ] - midcells[non0rates[i,1], ])) + (d2-d1)*r[4])
    arrows(centers[non0rates[i,1], 1],centers[non0rates[i,1], 2],centers[non0rates[i,2],1], centers[non0rates[i,2],2], lwd =  tQ[non0rates[i,1], non0rates[i,2]]*line.weight, length = 0.1)
  }
  
  diag(tQ) <- -1*apply(tQ, 1, sum)
  simdat <- castor::simulate_mk_model(tree, Q=tQ)
  if(plot.points){
    TernaryPoints(midcells[simdat$tip_states, ]+rnorm(3*length(simdat$tip_states),0, 0.05), pch=21, bg=bayou:::makeTransparent("red", 10), col=bayou:::makeTransparent("red", 10)) 
  }
  
  points(centers, pch=as.character(1:bins2), col="red")
  return(list(simdat=simdat, Q=tQ, indQ=Q))
}

simdat <- simulateTernaryData(tree, r=c(-3,0,0,0), 0.5, bins)
simdat <- simulateTernaryData(tree, r=c(3,0,0,0), 0.5, bins)

simdat <- simulateTernaryData(tree, r=c(0,-3,0,0), 0.5, bins)
simdat <- simulateTernaryData(tree, r=c(0,3,0,0), 0.5, bins)

simdat <- simulateTernaryData(tree, r=c(0,0,-3,0), 0.5, bins)
simdat <- simulateTernaryData(tree, r=c(0,0,3,0), 0.5, bins)


simdat <- simulateTernaryData(tree, r=c(0,0,0,2), 0.1, bins)# Center repulsing
simdat <- simulateTernaryData(tree, r=c(0, 0, 0,-5), 0.5, bins)#Center attracting


tree <- geiger::sim.bdtree(b=1, stop="taxa", n=5000)
simdat <- simulateTernaryData(tree, r=c(0,0,-3,-2), 0.5, bins, line.weight=20)


terntd <- readRDS("../output/terntd.rds")

### All data
tree <- terntd$phy
dat <- terntd[['ddat']]
bins <- 4
bins2 <- bins^2

lnLTernaryGrad <- function(pp){
  Q <- r2Q(pp[1:4], pp[5], bins=bins)
  fitdat <- dat
  Ntips <- length(tree$tip.label)
  Nnodes <- tree$Nnode
  Nedges <- length(tree$edge.length)
  Nstates <- bins2
  tip_priors <- matrix(1e-08/(Nstates - 1), nrow=length(tree$tip.label), ncol=bins2)
  for(i in 1:nrow(tip_priors)){
    tip_priors[i, fitdat[i]] <- 1 - 1e-08
  }
  tree_edge <- as.vector(t(tree$edge)) - 1
  edge.length <- tree$edge.length
  transition_matrix <- as.vector(t(Q))
  prior_probabilities_per_tip <- as.vector(t(tip_priors))
  root_prior_type = "max_likelihood"
  root_prior_probabilities = numeric(0)
  oldest_age <- -1
  runtime_out_seconds <- 0
  exponentiation_accuracy <- 0.001
  max_polynomials <- 1000
  lnL0 <- castor:::Mk_loglikelihood_CPP(Ntips=Ntips, Nnodes=Nnodes, Nstates=Nstates, Nedges=Nedges, prior_probabilities_per_tip = prior_probabilities_per_tip, 
                                        root_prior_type = root_prior_type, tree_edge <- tree_edge, edge_length=edge.length, transition_matrix=transition_matrix,
                                        root_prior=root_prior_probabilities, oldest_age=oldest_age, runtime_out_seconds = runtime_out_seconds, 
                                        exponentiation_accuracy = exponentiation_accuracy, max_polynomials = max_polynomials)
  return(lnL0$loglikelihood*-1)
}
lnLTernaryGrad(c(0,0,0,0,0.1))

fitTernaryGradAll <- stats::nlminb(c(0,0,0,0,0.1) , lnLTernaryGrad, 
              lower=c(-10,-10,-10,-10,0), upper=c(10,10,10,10,100), 
              control = list(step.min = 1e-04))


## Aquatic
.terntd <- filter(terntd, Terr==0)
tree <- .terntd$phy
dat <- .terntd[['ddat']]
bins <- 4
bins2 <- bins^2

lnLTernaryGrad <- function(pp){
  Q <- r2Q(pp[1:4], pp[5], bins=bins)
  fitdat <- dat
  Ntips <- length(tree$tip.label)
  Nnodes <- tree$Nnode
  Nedges <- length(tree$edge.length)
  Nstates <- bins2
  tip_priors <- matrix(1e-08/(Nstates - 1), nrow=length(tree$tip.label), ncol=bins2)
  for(i in 1:nrow(tip_priors)){
    tip_priors[i, fitdat[i]] <- 1 - 1e-08
  }
  tree_edge <- as.vector(t(tree$edge)) - 1
  edge.length <- tree$edge.length
  transition_matrix <- as.vector(t(Q))
  prior_probabilities_per_tip <- as.vector(t(tip_priors))
  root_prior_type = "max_likelihood"
  root_prior_probabilities = numeric(0)
  oldest_age <- -1
  runtime_out_seconds <- 0
  exponentiation_accuracy <- 0.001
  max_polynomials <- 1000
  lnL0 <- castor:::Mk_loglikelihood_CPP(Ntips=Ntips, Nnodes=Nnodes, Nstates=Nstates, Nedges=Nedges, prior_probabilities_per_tip = prior_probabilities_per_tip, 
                                        root_prior_type = root_prior_type, tree_edge <- tree_edge, edge_length=edge.length, transition_matrix=transition_matrix,
                                        root_prior=root_prior_probabilities, oldest_age=oldest_age, runtime_out_seconds = runtime_out_seconds, 
                                        exponentiation_accuracy = exponentiation_accuracy, max_polynomials = max_polynomials)
  return(lnL0$loglikelihood*-1)
}
lnLTernaryGrad(c(0,0,0,0,0.1))

fitTernaryGradAq <- stats::nlminb(c(0,0,0,0,0.1) , lnLTernaryGrad, 
                                   lower=c(-10,-10,-10,-10,0), upper=c(10,10,10,10,100), 
                                   control = list(step.min = 1e-04))

## Terrestrial
.terntd <- filter(terntd, Terr==1)
tree <- .terntd$phy
dat <- .terntd[['ddat']]
bins <- 4
bins2 <- bins^2

lnLTernaryGrad <- function(pp){
  Q <- r2Q(pp[1:4], pp[5], bins=bins)
  fitdat <- dat
  Ntips <- length(tree$tip.label)
  Nnodes <- tree$Nnode
  Nedges <- length(tree$edge.length)
  Nstates <- bins2
  tip_priors <- matrix(1e-08/(Nstates - 1), nrow=length(tree$tip.label), ncol=bins2)
  for(i in 1:nrow(tip_priors)){
    tip_priors[i, fitdat[i]] <- 1 - 1e-08
  }
  tree_edge <- as.vector(t(tree$edge)) - 1
  edge.length <- tree$edge.length
  transition_matrix <- as.vector(t(Q))
  prior_probabilities_per_tip <- as.vector(t(tip_priors))
  root_prior_type = "max_likelihood"
  root_prior_probabilities = numeric(0)
  oldest_age <- -1
  runtime_out_seconds <- 0
  exponentiation_accuracy <- 0.001
  max_polynomials <- 1000
  lnL0 <- castor:::Mk_loglikelihood_CPP(Ntips=Ntips, Nnodes=Nnodes, Nstates=Nstates, Nedges=Nedges, prior_probabilities_per_tip = prior_probabilities_per_tip, 
                                        root_prior_type = root_prior_type, tree_edge <- tree_edge, edge_length=edge.length, transition_matrix=transition_matrix,
                                        root_prior=root_prior_probabilities, oldest_age=oldest_age, runtime_out_seconds = runtime_out_seconds, 
                                        exponentiation_accuracy = exponentiation_accuracy, max_polynomials = max_polynomials)
  return(lnL0$loglikelihood*-1)
}
lnLTernaryGrad(c(0,0,0,0,0.1))

fitTernaryGradTerr <- stats::nlminb(c(0,0,0,0,0.1) , lnLTernaryGrad, 
                                  lower=c(-10,-10,-10,-10,0), upper=c(10,10,10,10,100), 
                                  control = list(step.min = 1e-04))




set.seed(1)
par(mfrow=c(2,2), mar=c(0,0,2,0))
simulateTernaryData(tree, r=fitTernaryGradAll$par[1:4], c=fitTernaryGradAll$par[5], bins=4, line.weight=50, plot.points=TRUE, main="All")
simulateTernaryData(tree, r=fitTernaryGradAq$par[1:4], c=fitTernaryGradAq$par[5], bins=4, line.weight=50, plot.points=TRUE, main="Aquatic")
simulateTernaryData(tree, r=fitTernaryGradTerr$par[1:4], c=fitTernaryGradTerr$par[5], bins=4, line.weight=50, plot.points=TRUE, main="Terrestrial")


###############################
fitdat <- simdat$simdat$tip_states
Ntips <- length(tree$tip.label)
Nnodes <- tree$Nnode
Nedges <- length(tree$edge.length)
Nstates <- bins2
tip_priors <- matrix(1e-08/(Nstates - 1), nrow=length(tree$tip.label), ncol=bins2)
for(i in 1:nrow(tip_priors)){
  tip_priors[i, fitdat[i]] <- 1 - 1e-08
}
tree_edge <- as.vector(t(tree$edge)) - 1
edge.length <- tree$edge.length
transition_matrix <- as.vector(t(simdat$Q))
prior_probabilities_per_tip <- as.vector(t(tip_priors))
root_prior_type = "max_likelihood"
root_prior_probabilities = numeric(0)
oldest_age <- -1
runtime_out_seconds <- 0
exponentiation_accuracy <- 0.001
max_polynomials <- 1000


lnL0 <- castor:::Mk_loglikelihood_CPP(Ntips=Ntips, Nnodes=Nnodes, Nstates=Nstates, Nedges=Nedges, prior_probabilities_per_tip = prior_probabilities_per_tip, 
                              root_prior_type = root_prior_type, tree_edge <- tree_edge, edge_length=edge.length, transition_matrix=transition_matrix,
                              root_prior=root_prior_probabilities, oldest_age=oldest_age, runtime_out_seconds = runtime_out_seconds, 
                              exponentiation_accuracy = exponentiation_accuracy, max_polynomials = max_polynomials)



##################################################
results = Mk_loglikelihood_CPP(Ntips = length(focal_tree$tip.label), 
                               Nnodes = focal_tree$Nnode, Nedges = nrow(focal_tree$edge), 
                               Nstates = Nstates, tree_edge = as.vector(t(focal_tree$edge)) - 
                                 1, edge_length = (if (is.null(focal_tree$edge.length)) 
                                   numeric()
                                   else focal_tree$edge.length), transition_matrix = as.vector(t(Q)), 
                               prior_probabilities_per_tip = as.vector(t(tip_priors[[tr]])), 
                               root_prior_type = root_prior_type, root_prior = root_prior_probabilities, 
                               oldest_age = (if (is.null(oldest_ages)) 
                                 -1
                                 else (if (is.finite(oldest_ages[tr])) 
                                   oldest_ages[tr]
                                   else -1)), runtime_out_seconds = max_model_runtime, 
                               exponentiation_accuracy = 0.001, max_polynomials = 1000)

#################################################################

function (Ntips, Nnodes, Nedges, Nstates, tree_edge, edge_length, 
          transition_matrix, prior_probabilities_per_tip, root_prior_type, 
          root_prior, oldest_age, runtime_out_seconds, exponentiation_accuracy, 
          max_polynomials) 
{
  .Call(`_castor_Mk_loglikelihood_CPP`, Ntips, Nnodes, Nedges, 
        Nstates, tree_edge, edge_length, transition_matrix, prior_probabilities_per_tip, 
        root_prior_type, root_prior, oldest_age, runtime_out_seconds, 
        exponentiation_accuracy, max_polynomials)
}
<bytecode: 0x5613a39a9a28>
  <environment: namespace:castor>





#######################################################

function (trees, Nstates, tip_states = NULL, tip_priors = NULL, 
          rate_model = "ER", root_prior = "auto", oldest_ages = NULL, 
          guess_transition_matrix = NULL, Ntrials = 1, max_model_runtime = NULL, 
          optim_algorithm = "nlminb", optim_max_iterations = 200, optim_rel_tol = 1e-08, 
          check_input = TRUE, Nthreads = 1, Nbootstraps = 0, Ntrials_per_bootstrap = NULL, 
          verbose = FALSE, verbose_prefix = "") 
{
#  if (verbose) 
#    cat(sprintf("%sChecking input variables..\n", verbose_prefix))
#  if ((!is.null(tip_states)) && (!is.null(tip_priors))) 
#    return(list(success = FALSE, error = "tip_states and tip_priors are both non-NULL, but exactly one of them should be NULL"))
#  else if (is.null(tip_states) && is.null(tip_priors)) 
#    return(list(success = FALSE, error = "tip_states and tip_priors are both NULL, but exactly one of them should be non-NULL"))
#  if ("phylo" %in% class(trees)) {
#    trees = list(trees)
#    Ntrees = 1
#  }
#  else if ("list" %in% class(trees)) {
#    Ntrees = length(trees)
#  }
#  else {
#    return(list(success = FALSE, error = sprintf("Unknown data format '%s' for input trees[]: Expected a list of phylo trees or a single phylo tree", 
#                                                 class(trees)[1])))
#  }
#  if (!is.null(tip_states)) {
#    if (!(("list" %in% class(tip_states)) && (length(tip_states) == 
#                                              Ntrees))) {
#      if ((Ntrees == 1) && (length(tip_states) == length(trees[[1]]$tip.label))) {
#        tip_states = list(unlist(tip_states))
#      }
#      else {
#        return(list(success = FALSE, error = sprintf("Invalid input format for tip_states: Expected a list of vectors, each listing the tip states of a specific tree")))
#      }
#    }
#  }
#  if (!is.null(tip_priors)) {
#    if (!(("list" %in% class(tip_priors)) && (length(tip_priors) == 
#                                              Ntrees))) {
#      if ((Ntrees == 1) && (nrow(tip_priors) == length(trees[[1]]$tip.label)) && 
#          (ncol(tip_priors) == Nstates)) {
#        tip_priors = list(tip_priors)
#      }
#      else {
#        return(list(success = FALSE, error = sprintf("Invalid input format for tip_priors: Expected a list of matrixes, each listing the tip priors of a specific tree")))
#      }
#    }
#  }
#  if (is.null(max_model_runtime)) 
#    max_model_runtime = 0
#  if (is.null(Ntrials_per_bootstrap)) 
#    Ntrials_per_bootstrap = max(1, Ntrials)
#  if (!is.null(oldest_ages)) {
#    if (length(oldest_ages) == 1) {
#      oldest_ages = rep(oldest_ages, times = Ntrees)
#    }
#    else if (length(oldest_ages) != Ntrees) {
#      return(list(success = FALSE, error = sprintf("Invalid number of oldest_ages[]; expected either 1 value or %d values (=Ntrees)", 
#                                                   Ntrees)))
#    }
#  }
  #if (!is.null(guess_transition_matrix)) {
  #  if ((nrow(guess_transition_matrix) != Nstates) || (ncol(guess_transition_matrix) != 
  #                                                     Nstates)) 
  #    return(list(success = FALSE, error = sprintf("Guess transition matrix has incorrect dimensions (%d x %d); expected a %d x %d matrix", 
  ##                                                 nrow(guess_transition_matrix), ncol(guess_transition_matrix), 
  #                                                 Nstates, Nstates)))
  #}
  #original_guess_transition_matrix = guess_transition_matrix
  if (!is.null(tip_states)) {
    tip_priors = vector(mode = "list", Ntrees)
    for (tr in 1:Ntrees) {
      focal_tip_states = tip_states[[tr]]
      focal_tree = trees[[tr]]
      Ntips = length(focal_tree$tip.label)
      if (!is.numeric(focal_tip_states)) 
        return(list(success = FALSE, error = sprintf("tip_states for tree %d are not integers", 
                                                     tr)))
      if (length(focal_tip_states) == 0) 
        return(list(success = FALSE, error = sprintf("tip_states for tree %d are non-NULL but empty", 
                                                     tr)))
      if (length(focal_tip_states) != Ntips) 
        return(list(success = FALSE, error = sprintf("Length of tip_states (%d) for tree %d is not the same as the number of tips in the tree (%d)", 
                                                     length(focal_tip_states), tr, Ntips)))
      if (check_input) {
        min_tip_state = min(focal_tip_states)
        max_tip_state = max(focal_tip_states)
        if ((min_tip_state < 1) || (max_tip_state > Nstates)) 
          return(list(success = FALSE, error = sprintf("tip_states must be integers between 1 and %d, but found values between %d and %d for tree %d", 
                                                       Nstates, min_tip_state, max_tip_state, tr)))
        if ((!is.null(names(focal_tip_states))) && any(names(focal_tip_states) != 
                                                       focal_tree$tip.label)) 
          return(list(success = FALSE, error = "Names in tip_states and tip labels in tree %d don't match (must be in the same order)", 
                      tr))
      }
      focal_tip_priors = matrix(1e-08/(Nstates - 1), nrow = Ntips, 
                                ncol = Nstates)
      focal_tip_priors[cbind(1:Ntips, focal_tip_states)] = 1 - 
        1e-08
      tip_priors[[tr]] = focal_tip_priors
    }
  }
  else {
    for (tr in 1:Ntrees) {
      focal_tree = trees[[tr]]
      focal_tip_priors = tip_priors[[tr]]
      if (nrow(focal_tip_priors) == 0) 
        return(list(success = FALSE, error = sprintf("ERROR: tip_priors for tree %d is non-NULL but has zero rows", 
                                                     tr)))
      if (Nstates != ncol(focal_tip_priors)) {
        return(list(success = FALSE, error = sprintf("ERROR: Nstates (%d) differs from the number of columns in tip_priors (%d), for tree %d", 
                                                     Nstates, ncol(focal_tip_priors), tr)))
      }
      if (check_input) {
        if (any(focal_tip_priors > 1)) 
          return(list(success = FALSE, error = sprintf("ERROR: Some tip_priors are larger than 1.0 (max was %g), for tree %d", 
                                                       max(focal_tip_priors), tr)))
        if ((!is.null(rownames(focal_tip_priors))) && 
            (!is.null(focal_tree$tip.label)) && (rownames(focal_tip_priors) != 
                                                 focal_tree$tip.label)) 
          return(list(success = FALSE, error = sprintf("ERROR: Row names in tip_priors and tip labels in tree %d don't match", 
                                                       tr)))
      }
    }
  }
  NtotalTips = sum(sapply(1:Ntrees, FUN = function(tr) length(trees[[tr]]$tip.label)))
  if (root_prior[1] == "auto") {
    root_prior_type = "max_likelihood"
    root_prior_probabilities = numeric(0)
  }
  else if (root_prior[1] == "flat") {
    root_prior_type = "custom"
    root_prior_probabilities = rep(1/Nstates, times = Nstates)
  }
  else if (root_prior[1] == "empirical") {
    root_prior_type = "custom"
    root_prior_probabilities = sapply(1:Nstates, FUN = function(state) sum(sapply(1:Ntrees, 
                                                                                  FUN = function(tr) sum(tip_priors[[tr]][, state])))/NtotalTips)
    root_prior_probabilities = root_prior_probabilities/sum(root_prior_probabilities)
  }
  else if ((root_prior[1] == "stationary")) {
    root_prior_type = NULL
    root_prior_probabilities = NULL
  }
  else if (root_prior[1] == "max_likelihood") {
    root_prior_type = "max_likelihood"
    root_prior_probabilities = numeric(0)
  }
  else if (root_prior[1] == "likelihoods") {
    root_prior_type = "likelihoods"
    root_prior_probabilities = numeric(0)
  }
  else {
    if (length(root_prior) != Nstates) 
      return(list(success = FALSE, error = sprintf("ERROR: root_prior has length %d, expected %d", 
                                                   length(root_prior), Nstates)))
    if (check_input) {
      if (any(root_prior < 0)) 
        return(list(success = FALSE, error = sprintf("ERROR: root_prior contains negative values (down to %g)", 
                                                     min(root_prior))))
      if (abs(1 - sum(root_prior)) > 1e-06) 
        return(list(success = FALSE, error = sprintf("ERROR: Entries in root prior do not sum up to 1 (sum=%.10g)", 
                                                     sum(root_prior))))
    }
    root_prior_type = "custom"
    root_prior_probabilities = root_prior
  }
  temp_results = get_transition_index_matrix(Nstates, rate_model)
  index_matrix = temp_results$index_matrix
  Nrates = temp_results$Nrates
  first_guess_rate = 0
  NICs = 0
  for (tr in 1:Ntrees) {
    if (is.null(tip_states) || is.null(tip_states[[tr]])) {
      focal_tip_states = max.col(tip_priors[[tr]])
    }
    else {
      focal_tip_states = tip_states[[tr]]
    }
    focal_tree = trees[[tr]]
    focal_tip_pairs = extract_independent_sister_tips(focal_tree)
    phylogenetic_distances = get_pairwise_distances(focal_tree, 
                                                    A = focal_tip_pairs[, 1], B = focal_tip_pairs[, 2], 
                                                    check_input = FALSE)
    valids = which(phylogenetic_distances > 0)
    phylogenetic_distances = phylogenetic_distances[valids]
    focal_tip_pairs = focal_tip_pairs[valids, , drop = FALSE]
    transitions = abs(focal_tip_states[focal_tip_pairs[, 
                                                       1]] - focal_tip_states[focal_tip_pairs[, 2]])
    first_guess_rate = first_guess_rate + sum(transitions/phylogenetic_distances)
    NICs = NICs + length(valids)
  }
  first_guess_rate = first_guess_rate/NICs
  if (first_guess_rate == 0) {
    if (is.null(guess_transition_matrix) || all(is.na(guess_transition_matrix))) {
      first_guess_rate = mean(sapply(1:Ntrees, FUN = function(tr) Nstates/((if (is.null(trees[[tr]]$edge.length)) 
        1
        else mean(trees[[tr]]$edge.length)) * log(length(trees[[tr]]$tip.label))/log(2))))
    }
    else {
      first_guess_rate = mean(abs(as.vector(guess_transition_matrix)), 
                              na.rm = TRUE)
    }
  }
  if (is.null(guess_transition_matrix) || all(is.na(guess_transition_matrix))) {
    guess_transition_matrix = get_transition_matrix_from_rate_vector(rates = rep(first_guess_rate, 
                                                                                 Nrates), index_matrix = index_matrix, Nstates = Nstates)
  }
  else {
    guess_transition_matrix[is.na(guess_transition_matrix)] = first_guess_rate
  }
  diag(guess_transition_matrix) = 0
  diag(guess_transition_matrix) = -rowSums(guess_transition_matrix)
  objective_function = function(dense_rates) {
    if (any(is.nan(dense_rates)) || any(is.infinite(dense_rates))) 
      return(Inf)
    Q = get_transition_matrix_from_rate_vector(dense_rates, 
                                               index_matrix, Nstates)
    if (root_prior[1] == "stationary") {
      root_prior_type = "custom"
      root_prior_probabilities = get_stationary_distribution(Q)
    }
    loglikelihood = 0
    for (tr in 1:Ntrees) {
      focal_tree = trees[[tr]]
      results = Mk_loglikelihood_CPP(Ntips = length(focal_tree$tip.label), 
                                     Nnodes = focal_tree$Nnode, Nedges = nrow(focal_tree$edge), 
                                     Nstates = Nstates, tree_edge = as.vector(t(focal_tree$edge)) - 
                                       1, edge_length = (if (is.null(focal_tree$edge.length)) 
                                         numeric()
                                         else focal_tree$edge.length), transition_matrix = as.vector(t(Q)), 
                                     prior_probabilities_per_tip = as.vector(t(tip_priors[[tr]])), 
                                     root_prior_type = root_prior_type, root_prior = root_prior_probabilities, 
                                     oldest_age = (if (is.null(oldest_ages)) 
                                       -1
                                       else (if (is.finite(oldest_ages[tr])) 
                                         oldest_ages[tr]
                                         else -1)), runtime_out_seconds = max_model_runtime, 
                                     exponentiation_accuracy = 0.001, max_polynomials = 1000)
      if ((!results$success) || is.na(results$loglikelihood) || 
          is.nan(results$loglikelihood)) 
        return(Inf)
      loglikelihood = loglikelihood + results$loglikelihood
    }
    return(-loglikelihood)
  }
  fit_single_trial = function(trial) {
    power_range = 8
    initial_dense_rates = if (trial == 1) 
      extract_independent_rates_from_transition_matrix(guess_transition_matrix, 
                                                       index_matrix)
    else first_guess_rate * 10^runif(n = Nrates, min = -power_range/2, 
                                     max = power_range/2)
    rate_scale = mean(abs(initial_dense_rates))
    if (optim_algorithm == "optim") {
      fit = stats::optim(initial_dense_rates/rate_scale, 
                         function(x) objective_function(x * rate_scale), 
                         method = "L-BFGS-B", lower = rep(first_guess_rate/(10^power_range), 
                                                          Nrates)/rate_scale, upper = rep((10^power_range) * 
                                                                                            first_guess_rate, Nrates)/rate_scale, control = list(maxit = optim_max_iterations, 
                                                                                                                                                 reltol = optim_rel_tol))
      LL = -fit$value
      Nevaluations = fit$counts
      Niterations = NA
      converged = (fit$convergence == 0)
    }
    else {
      fit = stats::nlminb(initial_dense_rates/rate_scale, 
                          function(x) objective_function(x * rate_scale), 
                          lower = rep(0, Nrates)/rate_scale, upper = rep((10^power_range) * 
                                                                           first_guess_rate, Nrates)/rate_scale, control = list(iter.max = optim_max_iterations, 
                                                                                                                                eval.max = optim_max_iterations * Nrates * 
                                                                                                                                  10, rel.tol = optim_rel_tol, step.min = 1e-04))
      LL = -fit$objective
      Nevaluations = fit$evaluations[1]
      Niterations = fit$iterations
      converged = (fit$convergence == 0)
    }
    fit$par = fit$par * rate_scale
    return(list(LL = LL, Nevaluations = Nevaluations, Niterations = Niterations, 
                converged = converged, fit = fit))
  }
  if ((Ntrials > 1) && (Nthreads > 1) && (.Platform$OS.type != 
                                          "windows")) {
    if (verbose) 
      cat(sprintf("%sFitting %d free parameters (%d trials, parallelized)..\n", 
                  verbose_prefix, Nrates, Ntrials))
    fits = parallel::mclapply(1:Ntrials, FUN = function(trial) fit_single_trial(trial), 
                              mc.cores = min(Nthreads, Ntrials), mc.preschedule = FALSE, 
                              mc.cleanup = TRUE)
  }
  else {
    if (verbose) 
      cat(sprintf("%sFitting %d free parameters (%s)..\n", 
                  verbose_prefix, Nrates, (if (Ntrials == 1) 
                    "1 trial"
                    else sprintf("%d trials", Ntrials))))
    fits = sapply(1:Ntrials, function(x) NULL)
    for (trial in 1:Ntrials) {
      fits[[trial]] = fit_single_trial(trial)
    }
  }
  LLs = unlist_with_nulls(sapply(1:Ntrials, function(trial) fits[[trial]]$LL))
  valids = which((!is.na(LLs)) & (!is.nan(LLs)) & (!is.null(LLs)) & 
                   (!is.infinite(LLs)) & sapply(1:Ntrials, function(trial) (!any(is.null(fits[[trial]]$fit$par))) && 
                                                  all(is.finite(fits[[trial]]$fit$par))))
  if (length(valids) == 0) 
    return(list(success = FALSE, error = "Fitting failed for all trials"))
  best = valids[which.max(LLs[valids])]
  loglikelihood = fits[[best]]$LL
  fitted_rates = fits[[best]]$fit$par
  transition_matrix = get_transition_matrix_from_rate_vector(fitted_rates, 
                                                             index_matrix, Nstates)
  if (Nbootstraps > 0) {
    if (verbose) 
      cat(sprintf("%sEstimating confidence intervals using %d parametric bootstraps..\n", 
                  verbose_prefix, Nbootstraps))
    bootstrap_params_flat = matrix(NA, nrow = Nbootstraps, 
                                   ncol = Nstates * Nstates)
    for (b in 1:Nbootstraps) {
      if (verbose) 
        cat(sprintf("%s  Bootstrap #%d..\n", verbose_prefix, 
                    b))
      bootstrap_tip_states = vector(mode = "list", Ntrees)
      for (tr in 1:Ntrees) {
        bootstrap_tip_states[[tr]] = simulate_mk_model(tree = trees[[tr]], 
                                                       Q = transition_matrix, root_probabilities = "stationary", 
                                                       include_tips = TRUE, include_nodes = FALSE, 
                                                       Nsimulations = 1)$tip_states
      }
      fit = fit_mk(trees = trees, Nstates = Nstates, tip_states = bootstrap_tip_states, 
                   rate_model = rate_model, root_prior = root_prior, 
                   guess_transition_matrix = original_guess_transition_matrix, 
                   Ntrials = Ntrials_per_bootstrap, max_model_runtime = max_model_runtime, 
                   optim_algorithm = optim_algorithm, optim_max_iterations = optim_max_iterations, 
                   optim_rel_tol = optim_rel_tol, check_input = FALSE, 
                   Nthreads = Nthreads, Nbootstraps = 0, verbose = verbose, 
                   verbose_prefix = paste0(verbose_prefix, "    "))
      if (!fit$success) {
        if (verbose) 
          cat(sprintf("%s  WARNING: Fitting failed for this bootstrap: %s\n", 
                      verbose_prefix, fit$error))
      }
      else {
        bootstrap_params_flat[b, ] = as.vector(fit$transition_matrix)
      }
    }
    standard_errors = matrix(sqrt(pmax(0, colMeans(bootstrap_params_flat^2, 
                                                   na.rm = TRUE) - colMeans(bootstrap_params_flat, na.rm = TRUE)^2)), 
                             nrow = Nstates, byrow = FALSE)
    quantiles_flat = sapply(1:ncol(bootstrap_params_flat), 
                            FUN = function(p) quantile(bootstrap_params_flat[, 
                                                                             p], probs = c(0.25, 0.75, 0.025, 0.975, 0.5), 
                                                       na.rm = TRUE, type = 8))
    CI50lower = matrix(quantiles_flat[1, ], nrow = Nstates, 
                       byrow = FALSE)
    CI50upper = matrix(quantiles_flat[2, ], nrow = Nstates, 
                       byrow = FALSE)
    CI95lower = matrix(quantiles_flat[3, ], nrow = Nstates, 
                       byrow = FALSE)
    CI95upper = matrix(quantiles_flat[4, ], nrow = Nstates, 
                       byrow = FALSE)
    medians = matrix(quantiles_flat[5, ], nrow = Nstates, 
                     byrow = FALSE)
  }
  return(list(success = TRUE, Nstates = Nstates, transition_matrix = transition_matrix, 
              loglikelihood = loglikelihood, Niterations = fits[[best]]$Niterations, 
              Nevaluations = fits[[best]]$Nevaluations, converged = fits[[best]]$converged, 
              guess_rate = first_guess_rate, standard_errors = (if (Nbootstraps > 
                                                                    0) standard_errors else NULL), CI50lower = (if (Nbootstraps > 
                                                                                                                    0) CI50lower else NULL), CI50upper = (if (Nbootstraps > 
                                                                                                                                                              0) CI50upper else NULL), CI95lower = (if (Nbootstraps > 
                                                                                                                                                                                                        0) CI95lower else NULL), CI95upper = (if (Nbootstraps > 
                                                                                                                                                                                                                                                  0) CI95upper else NULL)))
}
<bytecode: 0x5613a1720830>
  <environment: namespace:castor>




