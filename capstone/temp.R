
library("rjags")


############### DATA ##################
#######################################

dat = read.csv(file="UCI_Credit_Card.csv", header=TRUE)
head(dat)

colnames(dat)[colnames(dat) == "default.payment.next.month"] = "DEFAULT"

dat$MAX_PAY_HIST <- pmax(dat$PAY_0, dat$PAY_2, dat$PAY_3, dat$PAY_4, dat$PAY_5, dat$PAY_6)
dat$D_EDU <- as.numeric(dat$EDUCATION <= 2)
dat$D_MAR <- as.numeric(dat$MARRIAGE <= 1)
dat$LIMIT <- dat$LIMIT_BAL

keep_columns <- c("ID", "LIMIT", "AGE", "SEX", "D_MAR", "D_EDU", "MAX_PAY_HIST", "DEFAULT") 
dat <- dat[keep_columns]
head(dat)

dat$AGE = round(dat$AGE, -1)

before_aggregation = dat

nobs = aggregate(dat$DEFAULT, by=list(dat$AGE, dat$SEX, dat$D_MAR, dat$D_EDU, dat$MAX_PAY_HIST), FUN=length)
ndef = aggregate(dat$DEFAULT, by=list(dat$AGE, dat$SEX, dat$D_MAR, dat$D_EDU, dat$MAX_PAY_HIST), FUN=sum)

colnames(nobs)[colnames(nobs) == "x"] = "NOBS"
colnames(ndef)[colnames(ndef) == "x"] = "NDEFAULT"

dat = merge(nobs, ndef)
colnames(dat)[1:5] = c("AGE", "SEX", "D_MAR", "D_EDU", "MAX_PAY_HIST")

head(dat)

sum(dat$NDEFAULT)/sum(dat$NOBS)


############### JAGS ##################
#######################################

mod_string = " model {
	for (i in 1:length(NOBS)) {
		NDEFAULT[i] ~ dbin(phi[i], NOBS[i])
    logit(phi[i]) = B0 + B[1]*AGE[i] + B[2]*SEX[i] + B[3]*D_MAR[i] + B[4]*D_EDU[i] + B[5]*MAX_PAY_HIST[i]
	}
	
	B0 ~ dnorm(0.0, 1.0/100.0)
	B[1] ~ dnorm(0.0, 1.0/100.0)
  B[2] ~ dnorm(0.0, 1.0/100.0)
  B[3] ~ dnorm(0.0, 1.0/100.0)
  B[4] ~ dnorm(0.0, 1.0/100.0)
  B[5] ~ dnorm(0.0, 1.0/100.0)

} "

dat_jags = as.list(dat)
params = c("B0", "B")
mod_bay = jags.model(textConnection(mod_string), data=dat_jags, n.chains=3)

update(mod_bay, 1e4)
mod_sim = coda.samples(model=mod_bay, variable.names=params, n.iter=1e5)
mod_csim = do.call(rbind, mod_sim)

summary(mod_sim)
colMeans(mod_csim)

plot(mod_sim)
autocorr.plot(mod_sim)
raftery.diag(mod_sim)
autocorr.diag(mod_sim)
gelman.diag(mod_sim)
effectiveSize(mod_sim)

params = colMeans(mod_csim)

############### ROC ##################
######################################

varnames = c("AGE", "SEX", "D_MAR", "D_EDU", "MAX_PAY_HIST")
head(before_aggregation)

new_dat = before_aggregation[varnames]
head(new_dat)
params

X = as.matrix(cbind(new_dat, 1))
head(X)

log_yhat = X %*% params
yhat = 1/(1+exp(-log_yhat))
head(yhat)
head(new_dat$DEFAULT)

length(before_aggregation$DEFAULT)
length(as.numeric(yhat))

roc <- roc(before_aggregation$DEFAULT, as.numeric(yhat))
str(roc)
roc$auc

############### Q's ##################
######################################

# varnames = c("AGE", "SEX", "D_MAR", "D_EDU", "MAX_PAY_HIST")

X_male = c(40, 1, 1, 1, 0, 1)
log_yhat_male = mod_csim %*% X_male
yhat_male = 1/(1+exp(-log_yhat_male))

X_female = c(40, 2, 1, 1, 0, 1)
log_yhat_female =  mod_csim %*% X_female
yhat_female = 1/(1+exp(-log_yhat_female))

head(yhat_male)
head(yhat_female)

mean(yhat_female < yhat_male)
