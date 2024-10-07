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

# 2.1 Univariate data: Qualitative variables
library(plotrix)

pie3D( # 3D Exploded Pie Chart
  as.vector(table(data$sp)),
  labels = unique(data$sp),
  explode=0.1, labelcex=1.0,
  main="Crabs Species") 

pie(
  slices,labels = lbls, col=rainbow(length(lbls)),
    main="Crabs Species")

x <- table(data$sp)
labels_x <- names(x) %>%
  paste(round(as.vector(x)/sum(x)*100)) %>%
  paste('%', sep = '')

pie(
  x = as.vector(x),
  labels = labels_x,
  col = rainbow(length(x)),
  main = "Crabs Species"
)

# Specific color for each bar
library(RColorBrewer)

aux <- table(data$sp, data$sex)
coul <- brewer.pal(length(aux), "Set2")
barplot(
  height = as.vector(aux),
  names = names(aux),
  col = coul)

# 2.2 Univariate data: Quantitative variables
library(ggplot2)

ggplot(
  data,
  aes(x = FL, y = RW, fill = sp)
) +
  geom_boxplot(alpha = 0.3) +
  theme(legend.position = 'none')

boxplot(
  select(data, FL:BD),
  col = 4, prob = TRUE,
  xlab = "Crabs Characteristics"
)
head(iris)

x <- round(iris[c(1,10,100,101), 3:4],1)
x
round(dist(x))






