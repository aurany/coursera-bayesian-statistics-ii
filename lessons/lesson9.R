
# Question 2
############

library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )

# Question 3
############

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

# Question 4
############

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)

# Question 5
############

library("rjags")

mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

params = c("b0", "b")

model = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(model, 1e3)

model_sim = coda.samples(model=model, variable.names=params, n.iter=10000)
model_csim = as.mcmc(do.call(rbind, model_sim)) # combined chains

plot(model_sim, ask=TRUE)

gelman.diag(model_sim)
raftery.diag(model_sim)
autocorr.diag(model_sim)
autocorr.plot(model_sim)
effectiveSize(model_sim)

# Question 6
############
coeffs = colMeans(model_csim)
coeffs

# Question 7
############

# child of age 60 months, with high OME, using a coherent stimulus of 50 decibels. 
log_prob = coeffs[5] + 60*coeffs[1] + 0*coeffs[2] + 50*coeffs[3] + 0*coeffs[4]
prob = 1/(1+exp(-log_prob))
prob

# Question 8
############

betas = coeffs[5] + X[] %*% coeffs[1:4]
phat = as.numeric(1.0 / (1.0 + exp(-betas)))

p_data = subset(OME, OME != "N/A")
p = p_data$Correct / p_data$Trials

table = table(phat > 0.7, p > 0.7)
table

sum(diag(table)) / sum(table)
