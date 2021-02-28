library(mice)
library(randomForest)
library(caret)


data <- read.csv("wdi.covid.csv", header=T, row.names=1) 

colnames(data) [1] <- "Continent"

data$Continent <- factor(data$Continent, 
				levels = c("Africa","Asia","Europe","North America","South America","Oceania"),
				labels = c(1,2,3,4,5,6))

#data imputation
impute <- mice(data, m=3, maxit=100, method="cart", seed=42)

new.data <-complete(impute,1)

#full data
xc <- new.data

xc2 <- scale(xc[,-1])

#basic analysis
index <- sample(1:nrow(xc), nrow(xc) * 0.7, replace=FALSE)
gdp.train <- xc[index,]
gdp.test <- xc[-index,]

set.seed(12)
#analysis with resampling 
train_control <- trainControl(method="cv", number=10, search="random")

model <- train(gdp20 ~., data=xc2, trControl= train_control, method="lm")

model2 <- train(gdp20 ~., data=xc[,-1], trControl= train_control, method="lasso")

model3 <- train(gdp20 ~., data=xc[,-1], trControl= train_control, method="rf")

#Ablation study 
#tp1.1 out
ab <- train(gdp20 ~., data=xc[,-c(1,9)], trControl= train_control, method="rf")

#tp1 out
ab2 <- train(gdp20 ~., data=xc[,-c(1,6)], trControl= train_control, method="rf")

#leb out
ab3 <- train(gdp20 ~., data=xc[,-c(1,11)], trControl= train_control, method="rf")

#uhc out
ab4 <- train(gdp20 ~., data=xc[,-c(1,13)], trControl= train_control, method="rf")

#dp1 out
ab5 <- train(gdp20 ~., data=xc[,-c(1,7)], trControl= train_control, method="rf")

#l6.b out
ab6 <- train(gdp20 ~., data=xc[,-c(1,12)], trControl= train_control, method="rf")


