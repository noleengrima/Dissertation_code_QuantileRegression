rm(list=ls())
graphics.off()

library(quantreg)
set.seed(2023)
data_M<- read.csv("")
attach(data_M)
names(data_M)
str(data_M)

# Defining the variables
Y=BaP
Y=as.vector(Y)
X<-as.matrix(data_M)


###### Cross Validation - Training and Test sets ######
library(tidyverse)
library(caret)
data_M<-as.data.frame(cbind(X,Y))
training.samples <- Y %>%
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- data_M[training.samples, ]
test.data <- data_M[-training.samples, ]
#summary(train.data)
#summary(test.data)

# Step 1: Calculate mean and variance for each variable in the training set
train_mean <- colMeans(train.data[,1:18])
train_std <- apply(train.data[,1:18], 2, sd)

# Step 2: Standardize both training and test sets using the training mean and variance
train.data.std <- scale(train.data[,1:18], center = train_mean, scale = train_std)
test.data.std <- scale(test.data[,1:18], center = train_mean, scale = train_std)

# Checking the means and std dev of each set
colMeans(train.data.std)
apply(train.data.std, 2, sd)
colMeans(test.data.std)
apply(test.data.std, 2, sd)

# Attaching the dependent variable with the standardised independent variables
train.data<-as.data.frame(cbind(train.data.std,train.data[,19]))
test.data<-as.data.frame(cbind(test.data.std,test.data[,19]))
colnames(train.data)[19] <- "Y"
colnames(test.data)[19] <- "Y"
attach(train.data)
attach(test.data)

###### Multicollinearity ###### 
# Correlation Matrix
cor(data_M,method = "spearman")

#Condition Index
library(klaR)
cond.index(Y~.,data = data_M[,1:18])

#VIF scores
library(jtools)
c3<-rq(Y~.,tau = 0.3,data = train.data) #fitting a classical quantile regression 
c5<-rq(Y~.,tau = 0.5,data = train.data) #fitting a classical quantile regression 
c7<-rq(Y~.,tau = 0.7,data = train.data) #fitting a classical quantile regression 
c95<-rq(Y~.,tau = 0.95,data = train.data) #fitting a classical quantile regression 
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  
summ(c5,vifs = TRUE,confint = TRUE,pvals = TRUE)  
summ(c7,vifs = TRUE,confint = TRUE,pvals = TRUE)  
summ(c95,vifs = TRUE,confint = TRUE,pvals = TRUE)  

###### Fittin the Classical for 0.30 quantile #######
# The other quantiles follow the same way
# Removing variables by largest VIF score 
library(jtools)
X<-as.data.frame(train.data[,1:18])
c3<-rq(train.data$Y~.,tau = 0.3,data = X)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 4387.62 urban

M<-X[,-15]
c3<-rq(train.data$Y~.,tau = 0.3,data = M)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 1182.10 Petrol Station

N<-M[,-2]
c3<-rq(train.data$Y~.,tau = 0.3,data = N)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 250.79 ETS

O<-N[,-5]
c3<-rq(train.data$Y~.,tau = 0.3,data = O)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 101.36 Use gas cooker 

P<-O[,-8]
c3<-rq(train.data$Y~.,tau = 0.3,data = P)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 270.97 Redecoration

Q<-P[,-2]
c3<-rq(train.data$Y~.,tau = 0.3,data = Q)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 21.44 Gas_main_heating

R<-Q[,-6]
c3<-rq(train.data$Y~.,tau = 0.3,data = R)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 59.79 Aerosol use

S<-R[,-8]
c3<-rq(train.data$Y~.,tau = 0.3,data = S)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 57.65 WM

T<-S[,-3]
c3<-rq(train.data$Y~.,tau = 0.3,data = T)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 41.54 New

U<-T[,-2]
c3<-rq(train.data$Y~.,tau = 0.3,data = U)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)   #largest = 6.58 Aerosol_use All below VIF=10


# removing according to p-value: ink 0.94 largest is eliminated next
V<-U[,-6]
c3<-rq(train.data$Y~.,tau = 0.3,data = V)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)      #largest p value= 0.76 walk busy road

W<-V[,-7]
c3<-rq(train.data$Y~.,tau = 0.3,data = W)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)     #largest pvalue = 0.62 laminated floor

Z<-W[,-5]
c3<-rq(train.data$Y~.,tau = 0.3,data = Z)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  #largest p value = 0.68 FL  

A<-Z[,-5]
c3<-rq(train.data$Y~.,tau = 0.3,data = A)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  #largest p value = 0.42 Floor

B<-A[,-5]
c3<-rq(train.data$Y~.,tau = 0.3,data = B)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  #largest p value = 0.33 Time ETS

C<-B[,-2]
c3<-rq(train.data$Y~.,tau = 0.3,data = C)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  #largest p value = 0.35 time travelling

D<-C[,-2]
c3<-rq(train.data$Y~.,tau = 0.3,data = D)
summ(c3,vifs = TRUE,confint = TRUE,pvals = TRUE)  #largest p value = 0.42 Floor

E<-as.data.frame(D[,1])
colnames(E)<- "Summer"
c3<-rq(train.data$Y~.,tau = 0.3,data = E)
summ(c3,vifs = FALSE,confint = TRUE,pvals = TRUE,digits = 4)  

# Performance Tests
predictions30 <- c3 %>% predict(test.data)
pred_train30 <- c3 %>% predict(train.data)
set.seed(2023)
data.frame( 
  RMSEP = RMSE(predictions30, test.data$Y),
  MAEP = MAE(predictions30, test.data$Y),
  RMSE = RMSE(pred_train30, train.data$Y),
  MAE = MAE(pred_train30,train.data$Y))

###### Fitting a LASSO QR ######
# The other quantiles follow by replacing the tau value
class30 <- rq(Y~.,data = train.data , tau = 0.3, method = "lasso")
predictions30 <- class30 %>% predict(test.data)
pred_train30 <- class30 %>% predict(train.data)
set.seed(2023)
data.frame( 
  RMSEP = RMSE(predictions30, test.data$Y),
  MAEP = MAE(predictions30, test.data$Y),
  RMSE = RMSE(pred_train30, train.data$Y),
  MAE = MAE(pred_train30,train.data$Y))

###### Fitting a LASSO QR model with interaction terms  ######

# Generate pairwise interaction terms for tarin and test set
interaction_train <- as.data.frame(model.matrix(~ (
  Summer+Distance_PetrolStation+Redecoration+New+WM+ETS+Time_ETS+Time_travelling+
    Gas_main_heating+Use_gas_cooker+Use_gas_cooker_wekend+Laminated_floor+Aerosol_use
  +stqhmeik+urban+FL+Walk_busy_road+which.floor.is.flat.located
)^2, data = train.data.std))

interaction_test <- as.data.frame(model.matrix(~ (
  Summer+Distance_PetrolStation+Redecoration+New+WM+ETS+Time_ETS+Time_travelling+
    Gas_main_heating+Use_gas_cooker+Use_gas_cooker_wekend+Laminated_floor+Aerosol_use
  +stqhmeik+urban+FL+Walk_busy_road+which.floor.is.flat.located
)^2, data = test.data.std))

interaction_train<-as.data.frame(cbind(interaction_train,train.data[,19]))
interaction_test<-as.data.frame(cbind(interaction_test,test.data[,19]))
colnames(interaction_train)[173] <- "Y"
colnames(interaction_test)[173] <- "Y"
attach(train.data)
attach(test.data)


# Define the quantile levels 
quantiles <- c(0.3, 0.50, 0.7,0.95)

# Fit quantile regression model with interaction terms
quant_reg_model <- rq(Y ~ ., data = interaction_train, tau = quantiles,method = "lasso")
predictions <- quant_reg_model %>% predict(interaction_test)
pred_train <- quant_reg_model %>% predict(interaction_train)
set.seed(2023)
data.frame( 
  RMSEP = c(RMSE(predictions[,1], interaction_test$Y),
            RMSE(predictions[,2], interaction_test$Y),
            RMSE(predictions[,3], interaction_test$Y),
            RMSE(predictions[,4], interaction_test$Y)),
  
  MAEP = c(MAE(predictions[,1], interaction_test$Y),
           MAE(predictions[,2], interaction_test$Y),
           MAE(predictions[,3], interaction_test$Y),
           MAE(predictions[,4], interaction_test$Y)),
  
  RMSE = c(RMSE(pred_train[,1], interaction_train$Y),
           RMSE(pred_train[,2], interaction_train$Y),
           RMSE(pred_train[,3], interaction_train$Y),
           RMSE(pred_train[,4], interaction_train$Y)),
  
  MAE = c(MAE(pred_train[,1],interaction_train$Y),
          MAE(pred_train[,2],interaction_train$Y),
          MAE(pred_train[,3],interaction_train$Y),
          MAE(pred_train[,4],interaction_train$Y)),
  row.names = c("tau=0.3","tau=0.5","tau=0.7","tau=0.95")
)






