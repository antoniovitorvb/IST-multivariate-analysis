require(MASS)
library(psych)

data <- crabs
head(data)

table(data$sp)
table(data$sp, data$sex)

# 1.2 Summary Statistics

summary(data)

describe(data)

describeBy(data[,4:8], group = data$sp)
describeBy(data[,4:8], group = data$sex)

round(quantile(data$FL,probs = seq(0,1,0.2)),3) # quantiles every 20%
round(quantile(data[,4],probs = seq(0,1,0.25)),3) # quantiles every 25%

# 1.3 Associations among variables

round(var(data[,4:8]),3) # Variance Matrix
round(cov(data[,4:8]),3) # Covariance Matrix

round(cor(data[,4:8]),3) # Correlation Matrix

corr = round(cor(data[,4:8]),3)
rowSums(corr)


