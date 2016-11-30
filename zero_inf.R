# test data
zero.dat <- read.csv("test_data.csv")

#plots nested in site (X3 treatments at plot-level)
zero.dat$nested_plot <- factor(with(zero.dat, paste(site, plot, sep= "_")) ) 

#zip model
nitt = 2000 #low for testing
thin = 10 #low for testing
prior = list( R = list(V = diag(2), nu = 0.002, fix = 2), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
zip <- MCMCglmm(y ~ trait -1 + at.level(trait, 1):(X1*X2*X3_nest), 
                random = ~ nested_plot, rcov = ~idh(trait):units,
                data=zero.dat, family = "zipoisson",  nitt = nitt, 
                burnin = 100, thin=thin, prior=prior, pr = TRUE, pl = TRUE)
summary(zip)
# Gives a warning!! And coef table seems messed up.
# Something is going on here.....dont know what though.

#zap model
zap <- MCMCglmm(y ~ trait*(X1*X2*X3_nest), 
                    random = ~ nested_plot, rcov = ~idh(trait):units,
                    data=zero.dat, family = "zapoisson",  nitt = nitt, 
                    burnin = 100, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(zap)
# No warning and everything looks good

# Poisson model
prior = list( R = list(V = diag(1), nu = 0.002), 
              G=list(G1=list(V=1, nu=0.002,alpha.mu=0, alpha.V=625^2))) 
pois <- MCMCglmm(y ~ X1*X2*X3_nest, 
                random = ~ nested_plot,
                data=zero.dat, family = "poisson",  nitt = nitt, 
                burnin = 100, thin=thin,  prior=prior, pr = TRUE, pl = TRUE)
summary(pois)
# No warning

# Some predictions
p.zip <- predict(zip, marginal = ~ nested_plot,  type="response", posterior = "mean")
p.zap <- predict(zap, marginal = ~ nested_plot,  type="response", posterior = "mean")
p.pois <- predict(pois, marginal = ~ nested_plot,  type="response", posterior = "mean")
cbind(aggregate(y ~ X1*X2*X3_nest, zero.dat, mean),
      zip = aggregate(p.zip ~ fire*retention*micro_hab.two, dat, mean)$V1,
      zap = aggregate(p.zap ~ fire*retention*micro_hab.two, dat, mean)$V1,
      pois = aggregate(p.pois ~ fire*retention*micro_hab.two, dat, mean)$V1)
# poissoon model give extreme (unrealistic) values. zap best I think.


# Predictive testing: zeros
# Only for poisson models. Dont know how to do it with zip/zap.
mc.samp <- nrow(pois$VCV)
nz <- 1:mc.samp
oz <- sum(zero.dat == 0)
for (i in 1:mc.samp) {
  pred.l <- rnorm(nrow(zero.dat), (cbind(pois$X,pois$Z) %*% pois$Sol[1,])@x, sqrt(pois$VCV[i,]))
  nz[i] <- sum(rpois(nrow(dat), exp(pred.l)) == 0)
}
hist(nz, xlim=c(100,1500))
abline(v = oz, col = "red")
# Zero-inflated!

# test
#library(lme4)
#summary(lmer(y ~ X1*X2*X3_nest + (1|nested_plot), zero.dat))