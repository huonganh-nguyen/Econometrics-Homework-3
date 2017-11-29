install.packages("AER")
library(AER)
library(stargazer)
library(foreign)
library(car)
library(sandwich)

##### Question 1 ####
chicken <- read.csv("chicken.csv")
summary(chicken)

# transform the data set to construct a constant elasticity of demand model
cols <- colnames(chicken)
chicken[cols] <- log(chicken[cols])
summary(chicken)

# run an OLS regression of price on quantity
reg1 <- lm(QPRODA ~ PCHICK, data = chicken)
summary(reg1)

# regress one supply-side instrument against another to check if they are perfectly correlated
reg2 <- lm(PCOR ~ PF, data = chicken)
summary(reg2)

# generate an IV estimate for price w/o any covariates using the supply-side instruments
reg3 <- ivreg(QPRODA ~ PCHICK | PCOR + PF, data = chicken)
summary(reg3)

# check for weak instruments
reg4 <- lm(PCHICK ~ PCOR + PF, data = chicken)
summary(reg4)
linearHypothesis(reg4, c("PCOR = 0", "PF = 0"), test = "F")

# generate an IV estimate for price with pbeef as an add'l covariate
reg5 <- ivreg(QPRODA ~ PCHICK + PBEEF | PCOR + PF + PBEEF, data = chicken)
summary(reg5)

# generate an IV estimate for price with pbeef, population, and income as add'l covariates
reg6 <- ivreg(QPRODA ~ PCHICK + PBEEF + POP + Y | PCOR + PF + PBEEF + POP + Y, data = chicken)
summary(reg6)

# generate an IV estimate for price with pbeef, population, income, and cpi as add'l covariates
reg7 <- ivreg(QPRODA ~ PCHICK + PBEEF + POP + Y + CPI | PCOR + PF + PBEEF + POP + Y + CPI, data = chicken)
summary(reg7)

# check again for weak instruments
reg8 <- lm(PCHICK ~ PCOR + PF + PBEEF + POP + Y, data = chicken)
linearHypothesis(reg8, c("PCOR = 0", "PF = 0"), test = "F")

##### Question 2 ####
load("fish.RData")
summary(data)
desc
# 2SLS Table 3
reg1 <- ivreg(log(prca) ~ log(qtya) + mon + tues + wed + thurs |
              log(speed2) + log(speed3) + log(wave2) + log(wave3) + mon + tues + wed + thurs,
              data = data)
reg2 <- ivreg(log(prcw) ~ log(qtyw) + mon + tues + wed + thurs |
              log(speed2) + log(speed3) + log(wave2) + log(wave3) + mon + tues + wed + thurs,
              data = data)
stargazer(reg1, reg2, type = "text")

#### Question 3 ####
smoking <- read.dta("Smoking.dta")
summary(smoking)

# probability of smoking for all workers
mean(smoking$smoker)

# probability of smoking for workers affected by workplace smoking bans
smoking.bans <- smoking[smoking$smkban == 1, ]
mean(smoking.bans$smoker)

# probability of smoking for workers not affected by workplace smoking bans
smoking.no.bans <- smoking[smoking$smkban == 0, ]
mean(smoking.no.bans$smoker)

# difference in probability between bans and no bans
mean(smoking.bans$smoker) - mean(smoking.no.bans$smoker)

# run LPM to determine statistical significance of the difference in probability between bans and no bans
reg10 <- lm(smoker ~ smkban, data = smoking)
summary(reg10, robust = TRUE)

# run LPM of smoker
reg11 <- lm(smoker ~ smkban + female + age + I(age^2) + hsdrop + hsgrad + colsome + colgrad
            + black + hispanic, data = smoking)
stargazer(reg10, reg11, type = "text")
summary(reg11)

# run logit of smoker
reg12 <- glm(smoker ~ smkban + female + age + I(age^2) + hsdrop + hsgrad + colsome + colgrad
            + black + hispanic, family = "binomial"(link = logit), data = smoking)
stargazer(reg10, reg11, reg12, type = "text")
summary(reg12)

# run probit of smoker
reg13 <- glm(smoker ~ smkban + female + age + I(age^2) + hsdrop + hsgrad + colsome + colgrad
             + black + hispanic, family = "binomial"(link = probit), data = smoking)
stargazer(reg10, reg13, type = "text")
summary(reg13)

# Mr. A1 not subject to a workplace smoking ban, using probit
mra1 <- data.frame(smkban = 0,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra1.predict <- predict(reg13, newdata = mra1)
mra1.predict

# Mr. A2 subject to a workplace smoking ban, using probit
mra2 <- data.frame(smkban = 1,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra2.predict <- predict(reg13, newdata = mra2)
mra2.predict

# effect of smoking ban on Mr. A's probability of smoking, using probit
mra2.predict - mra1.predict

# Ms. B1 not subject to a workplace smoking ban, using probit
msb1 <- data.frame(smkban = 0,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb1.predict <- predict(reg13, newdata = msb1)
msb1.predict

# Ms. B2 subject to a workplace smoking ban, using probit
msb2 <- data.frame(smkban = 1,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb2.predict <- predict(reg13, newdata = msb2)
msb2.predict

# effect of smoking ban on Ms. B's probability of smoking, using probit
msb2.predict - msb1.predict

# Mr. A1 not subject to a workplace smoking ban, using logit
mra1 <- data.frame(smkban = 0,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra1.predict.logit <- predict(reg12, newdata = mra1)
mra1.predict.logit

# Mr. A2 subject to a workplace smoking ban, using logit
mra2 <- data.frame(smkban = 1,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra2.predict.logit <- predict(reg12, newdata = mra2)
mra2.predict.logit

# effect of smoking ban on Mr. A's probability of smoking, using logit
mra2.predict.logit - mra1.predict.logit

# Ms. B1 not subject to a workplace smoking ban, using logit
msb1 <- data.frame(smkban = 0,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb1.predict.logit <- predict(reg12, newdata = msb1)
msb1.predict.logit

# Ms. B2 subject to a workplace smoking ban, using logit
msb2 <- data.frame(smkban = 1,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb2.predict.logit <- predict(reg12, newdata = msb2)
msb2.predict.logit

# effect of smoking ban on Ms. B's probability of smoking, using logit
msb2.predict.logit - msb1.predict.logit

# Mr. A1 not subject to a workplace smoking ban, using LPM
mra1 <- data.frame(smkban = 0,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra1.predict.lpm <- predict(reg11, newdata = mra1)
mra1.predict.lpm

# Mr. A2 subject to a workplace smoking ban, using LPM
mra2 <- data.frame(smkban = 1,
                   age = 20,
                   hsdrop = 1,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 0,
                   black = 0,
                   hispanic = 0,
                   female = 1)
mra2.predict.lpm <- predict(reg11, newdata = mra2)
mra2.predict.lpm

# effect of smoking ban on Mr. A's probability of smoking, using LPM
mra2.predict.lpm - mra1.predict.lpm

# Ms. B1 not subject to a workplace smoking ban, using LPM
msb1 <- data.frame(smkban = 0,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb1.predict.lpm <- predict(reg11, newdata = msb1)
msb1.predict.lpm

# Ms. B2 subject to a workplace smoking ban, using LPM
msb2 <- data.frame(smkban = 1,
                   age = 40,
                   hsdrop = 0,
                   hsgrad = 0,
                   colsome = 0,
                   colgrad = 1,
                   black = 1,
                   hispanic = 0,
                   female = 1)
msb2.predict.lpm <- predict(reg11, newdata = msb2)
msb2.predict.lpm

# effect of smoking ban on Ms. B's probability of smoking, using LPM
msb2.predict.lpm - msb1.predict.lpm
