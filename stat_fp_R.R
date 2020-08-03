######visualisation###########
###scatterplot for sqft_living and price
data <- read.csv(file="C:/Users/GMU/Downloads/Aswini.csv")
str(data)
head(data)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaps)
library(glmnet)
library(GGally)
attach(b)
plot(b$sqft_living,b$price,col="Blue",
     main="Scatterplot for Area of House and Price($)",
     xlab="Area of House(sqft)",ylab="Price($)")


#best fit line
scatter.smooth(x=b$sqft_living, y=b$price, main="living room area and price",
               col="red",xlab="living area room ",ylab = "price of the house")


#boxplot for bedrooms bathrooms and floors
boxplot(b$bedrooms,b$bathrooms,b$floors,col="Blue",
        main="Boxplot for analysing Average Bathrooms Bedrooms and Floors for Houses",
        xlab="Bedrooms  Bathrooms  Floors", ylab="count")



#histgram for grade
hist(b$grade,col="Red",
     main="Histogram for grade",xlab="Grade",ylab="Count")

#####################################
######### Lasso Regression ##########
#####################################

x = model.matrix(price~.,data)[,-1]
y = data$price

grid = 10^seq(10,-2,length=100)

# create training data
set.seed(1)
train = sample(1:nrow(x), size = nrow(x)/2)
lasso.mod = glmnet(x[train,],y[train], alpha = 1, lambda = grid)

# crossvalidation of datasets
set.seed(1)
cv.out = cv.glmnet(x,y,alpha = 1,nfold = 10)
plot(cv.out)

bestlambda = cv.out$lambda.min
bestlambda


## Model
model.lasso = glmnet(x,y,alpha = 1)
predict(model.lasso, type = "coefficients", s = bestlambda)[1:15,]


#King County dataset (Multiple Linear Regression)
########## Multiple Linear Regression############
#######################################
lm.fit = lm(price~bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_basement+yr_built+yr_renovated,data=data)
summary(lm.fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm.fit)



# the King county housing data (Random Forest Regression)
library(MASS)
data <- read.csv(file="C:/Users/GMU/Downloads/Aswini.csv")
#####data cleaning and preprocessing of datset######
str(data)
head(data)
data <- subset(data,select = -c(id))
data <- subset(data, select = -c(lat))
data <- subset(data, select = -c(long)) 
data <- subset(data, select = -c(month))
data <- subset(data, select = -c(year))
head(data)

# Create a training data set so can compare test MSE with that from regression trees
set.seed(1)
train = sample(1:nrow(data), nrow(data)/2)
# Create bagged trees by letting mtry=13. Default is to use 500 trees. 
# First call randomForest() using the formula interface.
library(randomForest)
set.seed(123)
bag.data=randomForest(price~.,data=data,subset=train,
                        mtry=13,importance=TRUE)
bag.data

# Now call randomForest() without using the formula interface
library(tidyverse)
set.seed(123)
King <- dplyr::select(data,price,everything())
bag.king=randomForest(x=data[train,-1],
                        y=data[train,1],
                        data=data,
                        mtry=13,importance=TRUE)
bag.data

# Compute test MSE for 500 bagged trees
king.test=data[-train,"price"]
yhat.bag = predict(bag.king,newdata=data[-train,])
mean1 <- mean((yhat.bag-king.test)^2)
mean1
# Plot test-set price vs. bagged-tree predicted price
options(scipen = 999)
ggplot(data.frame(yhat.bag, king.test), aes(x=yhat.bag ,y=king.test)) +
    geom_point() +
    geom_abline(slope=1,intercept=0) +
    labs(x="predicted price", 
         y="test-set price",
         title="Plot for test set and Bagged-tree predicted price")


# Create only 25 bagged trees
set.seed(123)
bag.king=randomForest(price~.,data=data,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=data[-train,])
mean2 <- mean((yhat.bag-king.test)^2)
mean2
# Create a random forest with 500 trees for mtry=6
set.seed(123)
rf.king=randomForest(price~.,data=data,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.king,newdata=data[-train,])
mean((yhat.rf-king.test)^2)

# Print and plot the variable-importance measures
importance(rf.king)
varImpPlot(rf.king,main="Variable importance for King County Dataset")

# Increase mtry to 8
set.seed(123)
rf.king=randomForest(price~.,data=data,subset=train,mtry=8,importance=TRUE)
yhat.rf = predict(rf.king,newdata=data[-train,])
mean((yhat.rf-king.test)^2)

# Create a random forest with the four most important variables
# and let mtry=2
King2 <- dplyr::select(data,price,sqft_living,grade,sqft_living15,sqft_above)
set.seed(123)
rf.king2=randomForest(x=King2[train,-1],
                        y=King2[train,1],
                        data=King2,
                        mtry=2,importance=TRUE)
yhat.rf2 = predict(rf.king2, newdata=King2[-train,])
king.test2=King2[-train,"price"]
mean((yhat.rf2-king.test2)^2)
