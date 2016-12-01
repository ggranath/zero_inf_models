# test data
require(MCMCglmm)
require(RCurl)
zero.dat <-read.csv(text=getURL("https://raw.githubusercontent.com/ggranath/zero_inf_models/master/test_data.csv"),header=T)
#zero.dat <- read.csv("test_data.csv")

#plots nested in site (X3 treatments at plot-level)
zero.dat$nested_plot <- factor(with(zero.dat, paste(site, plot, sep= "_")) ) 

#zip model
nitt = 10000 #low for testing
thin = 10 #low for testing
burnin = 1000 #low for testing
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=diag(2)*c(1,0.001), nu=0.002, fix=2))) 
zip <- MCMCglmm(y ~ trait -1 + at.level(trait, 1):(X1*X2*X3_nest), 
                random = ~ idh(trait):nested_plot, rcov = ~idh(trait):units,
                data=zero.dat, family = "zipoisson",  nitt = nitt, 
                burnin = burnin, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip)
# Gives a warning!! And contrasts are not straight forward to interpret.

#zap model
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
zap <- MCMCglmm(y ~ trait*(X1*X2*X3_nest), 
                    random = ~ nested_plot, rcov = ~idh(trait):units,
                    data=zero.dat, family = "zapoisson",  nitt = nitt, 
                    burnin = burnin, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(zap)
# No warning and everything looks good

# Poisson model
prior = list( R = list(V = diag(1), nu = 0.002), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
pois <- MCMCglmm(y ~ X1*X2*X3_nest, 
                random = ~ nested_plot,
                data=zero.dat, family = "poisson",  nitt = nitt, 
                burnin = burnin, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(pois)
# No warning

# Some predictions
# by default, all random factors are marginalised
p.zip <- predict(zip,   type="response", posterior = "mean")
p.zap <- predict(zap,  type="response", posterior = "mean")
p.pois <- predict(pois,   type="response", posterior = "mean")
cbind(aggregate(y ~ X1*X2*X3_nest, zero.dat, mean),
      zip = aggregate(p.zip ~ X1*X2*X3_nest, zero.dat, mean)$V1,
      zap = aggregate(p.zap ~ X1*X2*X3_nest, zero.dat, mean)$V1,
      pois = aggregate(p.pois ~ X1*X2*X3_nest, zero.dat, mean)$V1)
# poissoon model give extreme (unrealistic) values. zap best I think.


# Predictive testing: zeros

# zip model
oz <- sum(zero.dat == 0)
sim.zi <- simulate(zip, type="response", posterior = "all", nsim=1000)
dist.zeros.zi <- apply(sim.zi, 2, function (x) sum(x==0))
hist(dist.zeros.zi)
abline(v = oz, col = "red")

# zap model
oz <- sum(zero.dat == 0)
sim.za <- simulate(zap, type="response", posterior = "mean", nsim=1000)
dist.zeros.za <- apply(sim.za, 2, function (x) sum(x==0))
hist(dist.zeros.za)
abline(v = oz, col = "red")
# very good prediction of zeros!

# Poisson model 
oz <- sum(zero.dat == 0)
sim.pois <- simulate(pois, type="response", posterior = "mean", nsim=1000)
dist.zeros.pois <- apply(sim.pois, 2, function (x) sum(x==0))
hist(dist.zeros.pois, xlim=c(900,1250))
abline(v = oz, col = "red")
# Zero-inflated!

# Simulation from pois model, by hand.
mc.samp <- nrow(pois$VCV)
nz <- 1:mc.samp
oz <- sum(zero.dat == 0)
for (i in 1:mc.samp) {
  pred.l <- rnorm(nrow(zero.dat), (cbind(pois$X,pois$Z) %*% pois$Sol[1,])@x, sqrt(pois$VCV[i,]))
  nz[i] <- sum(rpois(nrow(zero.dat), exp(pred.l)) == 0)
}
hist(nz, xlim=c(900,1250))
abline(v = oz, col = "red")


# plot zero distributions
zeroDens <- data.frame(y = c(dist.zeros.zi, dist.zeros.za, dist.zeros.pois, sample(nz,1000)),
                       model = rep(c("zi", "za", "pois", "pois.byHand"), each=1000))
library(ggplot2)
ggplot(zeroDens,aes(x=y, fill=model)) + geom_density(alpha=0.25) + geom_vline(xintercept=oz)
# only ZAP that captures all the zeros. Zip not that bad though.

# test
#library(lme4)
#summary(lmer(y ~ X1*X2*X3_nest + (1|nested_plot), zero.dat))