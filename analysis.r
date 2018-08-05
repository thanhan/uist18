require(MASS)
require(lme4)
require(lmerTest)
require(ordinal)
require(nlme)

# Experiment 1, before seeing veracity prediction.
data = read.csv('exp1.csv')
data$HE = ordered(data$Human.Error)

m1 <- clmm(data = data, HE ~ 1 + Model.Correct + Model.Wrong + (1|Claim) +(1|participant))
summary(m1)

# Experiment 1, after seeing veracity prediction.

data1p = read.csv('exp1p.csv')
data1p$HE = ordered(data1p$Human.Error)
m1p <- clmm(data = data1p, HE ~ 1 + Model.Correct + Model.Wrong + (1|Claim) +(1|participant))
summary(m1p)

# Experiment 2.
data2 = read.csv('exp2.csv')
m2 <- lmer(data = data2, Points ~ 1 + Condition + (1|Claim) +(1|participant))
summary(m2)

# Experiment 3.
data3 = read.csv('exp3.csv')
m3 <- lmer(data = data3, Points ~ 1 + Condition + (1|Claim) +(1|participant))
summary(m3)

# Experiment 2 with outliers removed.
data2o <- read.csv('exp2_removed_outliers.csv')
m2o <- lmer(data = data2o, Points ~ 1 + Condition + (1|Claim) +(1|participant))
summary(m2o)