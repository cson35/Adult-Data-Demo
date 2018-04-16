library(RColorBrewer)
library(ggplot2)
library(ROCR)

train <- read.csv('C:/Users/canso/Downloads/train.csv', row.names=1)
test <- read.csv('C:/Users/canso/Downloads/test.csv', row.names=1)

cateCol <- c('workclass', 'education', 'martial_status', 'occupation', 
            'relationship', 'race', 'sex', 'native_country', 'income')
numCol <- setdiff(colnames(train), cateCol)

train[cateCol] <- lapply(train[cateCol], factor)
test[cateCol] <- lapply(test[cateCol], factor)
sapply(train, class)

# exploratory 

colorRange <- c('#69091e', '#e37f65', 'white', '#aed2e6', '#042f60')
## colorRamp() returns a function which takes as an argument a number
## on [0,1] and returns a color in the gradient in colorRange
myColorRampFunc <- colorRamp(colorRange)

panel.cor <- function(w, z, ...) {
  correlation <- cor(w, z)
  
  ## because the func needs [0,1] and cor gives [-1,1], we need to shift and scale it
  col <- rgb(myColorRampFunc((1 + correlation) / 2 ) / 255 )
  
  ## square it to avoid visual bias due to "area vs diameter"
  radius <- sqrt(abs(correlation))
  radians <- seq(0, 2*pi, len = 50) # 50 is arbitrary
  x <- radius * cos(radians)
  y <- radius * sin(radians)
  ## make them full loops
  x <- c(x, tail(x,n=1))
  y <- c(y, tail(y,n=1))
  
  ## trick: "don't create a new plot" thing by following the
  ## advice here: http://www.r-bloggers.com/multiple-y-axis-in-a-r-plot/
  ## This allows
  par(new=TRUE)
  plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1), axes=FALSE, asp=1)
  polygon(x, y, border=col, col=col)
}


pairs(train[sample.int(nrow(train),5000),], lower.panel=panel.cor)

# plot high correlated features
plot(education_num ~ education, data = train) 
# they are identical, so will remove education since it has no order
train <- subset(train, select = -education)
test <- subset(test, select = -education)

# plot(relationship ~ sex, data = train) # still different

# check distribution of features
par(mfrow = c(2,3))
hist(train$age)
hist(train$fnlwgt)
hist(train$education_num)
hist(train$captial_gain)
hist(train$captial_loss)
hist(train$hours_per_week)

par(mfrow = c(3,3))
plot(train$workclass) 
plot(train$martial_status)
plot(train$occupation)
plot(train$relationship)
plot(train$race)
plot(train$sex)
plot(train$native_country)
plot(train$income)

# data mining
# logistic
lgm <- glm(income ~., data = train, family = 'binomial')
summary(lgm)
# most statistically significant features : age, workclass Self-emp-not-inc, 
# education_num, occupation Exec-managerial, relationship Wife, sex male, 
# capital_gain, capital_loss, hours_per_week

# coefficient: taking age for example, for every one unit change in age, 
# the log odds of admission increases by 0.02634

# fit indicies
coef(lgm)
library(aod)
# test the significant of whole factor features
# workclass
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 3:8)
# martial
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 11:16)
# occupcation
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 17:29)
# relationship
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 30:34)
# race
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 35:38)
# native_country
wald.test(b = coef(lgm), Sigma = vcov(lgm), Terms = 43:82)
# all siginificant

with(lgm, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# this model significant difference with a null model

anova(lgm, test = 'Chisq')
# all varaibles has significant affect on reducing deviance of residuals 

testre <- predict(lgm, newdata = test, type = "response")
classre <- ifelse(testre > 0.5,">50K", "<=50K")
misClasificError <- mean(classre != test$income)
print(paste('Accuracy',1-misClasificError))

par(mfrow = c(1,1))
pr <- prediction(testre, test$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# precision and recall
perf <- performance(pr, "prec", "rec")
plot(perf)

# precision and recall under threshold 0.3
classre <- ifelse(testre > 0.3,">50K", "<=50K")
realT <- which(test$income == '>50K')
realF <- which(test$income == '<=50K')
testT <- which(unname(classre) == '>50K')
testF <- which(unname(classre) == '<=50K')
pressions <- length(intersect(realT, testT))/(length(intersect(realT, testT)) + length(intersect(realF, testT)))
recall <- length(intersect(realT, testT))/(length(intersect(realT, testT)) + length(intersect(realT, testF)))

# precision and recall under different threshold
prtable <- as.data.frame(matrix(0, nrow = 9, ncol = 3))
colnames(prtable) = c('Threshold', 'pression', 'recall')
for (th in 1:9){
  classre <- ifelse(testre > th*0.1,">50K", "<=50K")
  realT <- which(test$income == '>50K')
  realF <- which(test$income == '<=50K')
  testT <- which(unname(classre) == '>50K')
  testF <- which(unname(classre) == '<=50K')
  pressions <- length(intersect(realT, testT))/(length(intersect(realT, testT)) + length(intersect(realF, testT)))
  recall <- length(intersect(realT, testT))/(length(intersect(realT, testT)) + length(intersect(realT, testF)))
  prtable[th, 1] <- th*0.1
  prtable[th,2] <- pressions
  prtable[th,3] <- recall
}

prtable
