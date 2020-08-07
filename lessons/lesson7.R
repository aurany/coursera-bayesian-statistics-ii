##########
# QUIZ A #
##########

# Question 4
############
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

# Question 5
############
lm_model = lm(education ~ income+young+urban, Anscombe)
summary(lm_model)

# Question 6
############

library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)

data = as.list(Anscombe)
params = c("b", "sig")

inits = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1,1.0,1.0))
}

model = jags.model(textConnection(mod_string), data=data, inits=inits, n.chains=3)
update(model, 1000) # burn-in

model_sim = coda.samples(model=model, variable.names=params, n.iter=5000)

model_csim = do.call(rbind, model_sim) # combine multiple chains

plot(model_sim)
gelman.diag(model_sim)
autocorr.diag(model_sim)
autocorr.plot(model_sim)

# Question 8
############
plot(lm_model)


##########
# QUIZ B #
##########

# Question 3
############
dic.samples(model, n.iter=100000)

# Question 4
############

mod_string2 = " model {
  for (i in 1:length(education)) {
    education[i] ~ dnorm(mu[i], prec)
    mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
  }

  b0 ~ dnorm(0.0, 1.0/1.0e6)
  for (i in 1:2) {
    b[i] ~ dnorm(0.0, 1.0/1.0e6)
  }

  prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)

  sig2 = 1.0 / prec
  sig = sqrt(sig2)
} "

set.seed(72)

data2 = as.list(Anscombe[c("education", "income", "young")])
params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(2, 0.0, 100.0), "prec"=rgamma(1,1.0,1.0))
}

model2 = jags.model(textConnection(mod_string2), data=data2, inits=inits2, n.chains=3)
update(model2, 1000) # burn-in

mod_string3 = " model {
  for (i in 1:length(education)) {
    education[i] ~ dnorm(mu[i], prec)
    mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
  }

  b0 ~ dnorm(0.0, 1.0/1.0e6)
  for (i in 1:3) {
    b[i] ~ dnorm(0.0, 1.0/1.0e6)
  }

  prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)

  sig2 = 1.0 / prec
  sig = sqrt(sig2)
} "

set.seed(72)

data3 = as.list(Anscombe[c("education", "income", "young")])
params3 = c("b", "sig")

inits3 = function() {
  inits = list("b"=rnorm(3, 0.0, 100.0), "prec"=rgamma(1,1.0,1.0))
}

model3 = jags.model(textConnection(mod_string3), data=data3, inits=inits3, n.chains=3)
update(model3, 1000) # burn-in

dic.samples(model, n.iter=100000)
dic.samples(model2, n.iter=100000)
dic.samples(model3, n.iter=100000)

# Question 5
############

print(summary(model_sim))
mean(model_csim[,1]>0.0)
