##########
# QUIZ A #
##########

# Question 4
############

dat = read.csv(file="data/pctgrowth.csv", header=TRUE)

library(rjags)
mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(theta[grp[i]], prec)
  }
  
  for (j in 1:max(grp)) {
    theta[j] ~ dnorm(mu, tau_sq)
  }
  
  mu ~ dnorm(0, 1/1e6)
  tau_sq ~ dgamma(1.0/2.0, 1.0*3.0/2.0)
  prec ~ dgamma(2.0/2.0, 2*1/2)
  sig = sqrt(1/prec)
} "

set.seed(113)

data_jags = as.list(dat)

params = c("theta", "mu", "sig")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic = dic.samples(mod, n.iter=1e3)

pm_params = apply(mod_csim, 2, mean)
means_theta = pm_params[-c(1,2)]

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
plot(means_anova)
points(means_theta, col="red")

##########
# QUIZ B #
##########

# Question 1
############

library("rjags")
library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]]  + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}

  for (k in 1:max(ID)) {
    a[k] ~ dnorm(a0, prec_a)
  }
	
	a0 ~ dnorm(0.0, 1.0/10.0^2)
  prec_a ~ dgamma(1/2.0, 1*1.0/2.0)
  tau = sqrt( 1.0 / prec_a )

	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}

} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

params = c("a0", "a", "b", "tau")

set.seed(116)

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3) # burn-in

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic.samples(mod, n.iter=1e3)

