
# Question 1
############

Ey = exp(1.5 + 0.8*-0.3 + 1.2*1.0)
Ey

# Question 2
############
install.packages("COUNT")
library(COUNT)
data("badhealth")
library(rjags)

mod1_string = " model {
  for (i in 1:length(numvisit)) {
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
  }
  int ~ dnorm(0.0, 1.0/1e6)
  b_badh ~ dnorm(0.0, 1.0/1e4)
  b_age ~ dnorm(0.0, 1.0/1e4)
  b_intx ~ dnorm(0.0, 1.0/1e4)
} "

mod2_string = " model {
  for (i in 1:length(numvisit)) {
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
  }
  int ~ dnorm(0.0, 1.0/1e6)
  b_badh ~ dnorm(0.0, 1.0/1e4)
  b_age ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params1 = c("int", "b_badh", "b_age", "b_intx")
params2 = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)
mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod1_sim = coda.samples(model=mod1, variable.names=params1, n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

mod2_sim = coda.samples(model=mod2, variable.names=params2, n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

dic1 = dic.samples(mod1, n.iter=1e3)
dic2 = dic.samples(mod2, n.iter=1e3)

dic1
dic2

# Question 4
############
ppois(21, 30, lower.tail=TRUE)


# Question 5
############
dat = read.csv(file="data/callers.csv", header=TRUE)
head(dat)

# Question 7
############

mod_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i]*days_active[i])
    log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  }
  
  b0 ~ dnorm(0.0, 1.0/1e2)  
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1e2)
  }
} "

data_jags = as.list(dat)

params = c("b0", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod, variable.names=params, n.iter=15e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic = dic.samples(mod, n.iter=1e3)

mean(mod_csim[,2] > 0)

# Question 8
############

summary(mod_sim)

