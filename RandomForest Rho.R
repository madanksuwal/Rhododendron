## Rhodendron Endemic Model
#### Read Data ####
rh.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Presence Absence Rho 3 spp value extract.csv")
# Data set contains all extracted values of BioClim 19 varialbes, 
# Lat and Long for 3 species R. lepi, R. lown & R. cown

## Explore data
head(rh.df)
str(rh.df)
names(rh.df)
rh.df$PA<-as.factor(rh.df$PA)
dim(rh.df)
# Remove 
rh.df<-rh.df[, -c(1, 2, 3, 23)]
head(rh.df)

# Response or target variable is PA (presence/Absence)
table(rh.df$PA)
# Check prevalence
table(rh.df$PA)/ nrow(rh.df)
# Here, P/ is maintained at 50% by 50%

# Split to training and test data set
set.seed(5555)
sample.id <- sample (2, nrow (rh.df), replace =TRUE, prob = c(0.7, 0.3))
rh.train <- rh.df [sample.id == 1, ]
rh.test <- rh.df [sample.id == 2, ]

# Training data set
head(rh.train)
dim(rh.train)
table(rh.train$PA)

# Test data set
head (rh.test)
table(rh.test$PA)

# Selecting variables
varTrain<- names(rh.train)
print(varTrain)
# Exclude non-predictor variables
varTrain <- varTrain [!varTrain %in% c("CID", "latitude", "longitude", "species",   "PA")]
print (varTrain)

# add "+" sign between predictor variables 
varTrain1 <- paste(varTrain, collapse = "+")
print (varTrain1)

# Add response variables and convert to a formula object
rf.formula<- as.formula(paste ("PA", varTrain1, sep= "~"))
print (rf.formula)

# Load library
library(randomForest)
# Run RandomForest with above formula
rh.rf<- randomForest (rf.formula, data = rh.train, ntree = 500, importance= TRUE)
# See model output, confusion matrix and class.error
print(rh.rf)


# Plot RF model
plot(rh.rf)
# 500 decision trees or a forest has been built using the Random Forest 
# algorithm based learning. We can plot the error rate across decision trees. 
# The plot seems to indicate that after 200 decision trees, there is not a 
# significant reduction in error rate

# Variable Importance plot
varImpPlot(rh.rf, sort = TRUE, main = "Variable Importance", n.var=5)
# Variable Importance Table
var.imp<- data.frame (importance(rh.rf, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]

# To measure the accuracy of the Random Forest model. 
# Some of the other model performance statistics are KS, Lift Chart and ROC Curve


# Predicting response variables
rh.train$predicted.resp<- predict(rh.rf, rh.train)

# Confusion matrix
# confusionMatrix function from "caret" package can be used for creating 
# confusion matrix based on actual response variable and predicted value.
library(e1071)
library(caret)
confusionMatrix(data= rh.train$predicted.resp, reference= rh.train$PA, positive = '1')
# The "positive" represents true case such as 1 for 1/0, Yes for Yes/No, Ture for T/F
# It has accuracy of 99.18%, which is fantastic. 
# Now we can predict response for the validation sample and 
# calculate model accuracy for the sample.

# Predicting to test data set
# Predicting response variable
rh.test$predicted.resp<- predict(rh.rf, rh.test, OOB=TRUE, type="response")

# Create confusion matrix
confusionMatrix(data=rh.test$predicted.resp, reference = rh.test$PA, positive = '1')

# Creating performance object
library(ROCR)
a.v<-as.vector(rh.rf$votes[,2]) # extract predicted '1'
perf.obj<- prediction( predictions = a.v, labels = rh.train$PA  )

# Calculate AUC
rh.AUC <- performance(perf.obj, "auc")
rh.AUC
AUC=rh.AUC@y.values[[1]]
AUC
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate")
abline(a=0,b=1, lty=3)  # diagonal line

=======
## Rhodendron Endemic Model
#### Read Data ####
rh.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Presence Absence Rho 3 spp value extract.csv")
# Data set contains all extracted values of BioClim 19 varialbes, 
# Lat and Long for 3 species R. lepi, R. lown & R. cown

## Explore data
head(rh.df)
str(rh.df)
names(rh.df)
rh.df$PA<-as.factor(rh.df$PA)
dim(rh.df)
# Remove 
rh.df<-rh.df[, -c(1, 2, 3, 23)]
head(rh.df)

# Response or target variable is PA (presence/Absence)
table(rh.df$PA)
# Check prevalence
table(rh.df$PA)/ nrow(rh.df)
# Here, P/ is maintained at 50% by 50%

# Split to training and test data set
sample.id <- sample (2, nrow (rh.df), replace =TRUE, prob = c(0.7, 0.3))
rh.train <- rh.df [sample.id == 1, ]
rh.test <- rh.df [sample.id == 2, ]

# Training data set
head(rh.train)
dim(rh.train)
table(rh.train$PA)

# Test data set
rh.test (rh.test)
table(rh.test$PA)

# Selecting variables
varTrain<- names(rh.train)
# Exclude non-predictor variables
varTrain <- varTrain [!varTrain %in% c("CID", "latitude", "longitude", "species",   "PA")]
# add "+" sign between predictor variables 
varTrain1 <- paste(varTrain, collapse = "+")
# Add response variables and convert to a formula object
rf.formula<- as.formula(paste ("PA", varTrain1, sep= "~"))

# Load library
library(randomForest)
# Run RandomForest with above formula
rh.rf<- randomForest (rf.formula, data = rh.train, ntree = 500, importance= TRUE)
# Plot RF model
plot(rh.rf)
# 500 decision trees or a forest has been built using the Random Forest 
# algorithm based learning. We can plot the error rate across decision trees. 
# The plot seems to indicate that after 200 decision trees, there is not a 
# significant reduction in error rate

# Variable Importance plot
varImpPlot(rh.rf, sort = TRUE, main = "Variable Importance", n.var=5)
# Variable Importance Table
var.imp<- data.frame (importance(rh.rf, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]

# To measure the accuracy of the Random Forest model. 
# Some of the other model performance statistics are KS, Lift Chart and ROC Curve


# Predicting response variables
rh.train$predicted.resp<- predict(rh.rf, rh.train)

# Confusion matrix
# confusionMatrix function from "caret" package can be used for creating 
# confusion matrix based on actual response variable and predicted value.
library(e1071)
library(caret)
confusionMatrix(data= rh.train$predicted.resp, reference= rh.train$PA, positive = '1')
# The "positive" represents true case such as 1 for 1/0, Yes for Yes/No, Ture for T/F
# It has accuracy of 99.18%, which is fantastic. 
# Now we can predict response for the validation sample and 
# calculate model accuracy for the sample.

# Predicting to test data set
# Predicting response variable
rh.test$predicted.resp<- predict(rh.rf, rh.test, OOB=TRUE, type="response")

# Create confusion matrix
confusionMatrix(data=rh.test$predicted.resp, reference = rh.test$PA, positive = '1')

# Creating performance object
library(ROCR)
a.v<-as.vector(rh.rf$votes[,2]) # extract predicted '1'
perf.obj<- prediction( predictions = a.v, labels = rh.train$PA  )

# Calculate AUC
rh.AUC <- performance(perf.obj, "auc")
rh.AUC
AUC=rh.AUC@y.values[[1]]
AUC
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate")
abline(a=0,b=1, lty=3)  # diagonal line

