###Rhododendron 3 species RandomForest 
## Rhodendron Endemic Model
#### Read Data ####
pre.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Rhodo 3 spp presence points.csv")
head(pre.df)
abs.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Rhodo sister background point.csv")
head(abs.df)
pseu.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Random_pseudoabsence_elev_1000_to_5500.csv")
head(pseu.df)
latt.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/lattice clip.csv")
head(lattice.df)
# pre.df = presence of three species  R. lepi, R. lown & R. cown
# abs.df = pseudo absence, location of other Rhodoendeon species 
# latt.df = Lattice points (0.008 = 1km resolution) to predict from model,
# Data set contains all extracted values of BioClim 19 varialbes+
# geographi variables (DEM, slope, Elev)+ Relative Radiation Index + Growth related varialbes 
# (Annual BioTemperature, Aridity index, warmth index, coldness indes, and	Ellenberg Quotient)

# These data set lacks Presence/Absence column, i.e. 1/0 
# Add with 1/0 column
pre.df$PA<-1
head(pre.df)
abs.df$PA<-0
head(pre.df)
pseu.df$PA<-0
head(pseu.df)

# Remove unnecessory column, and make consistent in all data sets
pre.df<-pre.df[, -4]
abs.df<-abs.df[, -c(1,5, 6, 7)]
pseu.df<-pseu.df[, -1]
dim (pre.df)
names(pre.df)
dim (abs.df)
names(abs.df)
dim(pseu.df)
names(pseu.df)


pre.lepi<-subset(pre.df, species=="lepidotum" )
pre.lown<-subset(pre.df, species=="lowndesii" )
pre.cown<-subset(pre.df, species=="cowanianum" )
# change the presence file name to get other species
#rbind(pre.df, abs.df)->rh.df  


#### Change File ####
# Change peseudo-absence file to change absence types
rbind(pre.cown, abs.df)->rh.df  


## Explore data
print(rh.df[295:302,])
str(rh.df)
names(rh.df)
rh.df$PA<-as.factor(rh.df$PA)
dim(rh.df)

# Response or target variable is PA (presence/Absence)
table(rh.df$PA)
# Check prevalence
table(rh.df$PA)/ nrow(rh.df)
# Here, P/A is maintained at 25% / 75% 

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
varTrain <- varTrain [!varTrain %in% c("lat", "long", "species",   "PA")]
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
varImpPlot(rh.rf, sort = TRUE, main = "Variable Importance", n.var=10)
# Variable Importance Table
var.imp<- data.frame (importance(rh.rf, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]

# To measure the accuracy of the Random Forest model. 
# Some of the other model performance statistics are KS, Lift Chart and ROC Curve


# Predicting response variables
rh.train$predicted.resp<- predict(rh.rf, rh.train)

# Predict to lattice
names(latt.df)
latt.p<-latt.df[,c(3,4)]
head(latt.p)

#### Prediction File ####
latt.p$probability<- predict (rh.rf, newdata = latt.df[,-1], type = "prob")[,2]
latt.p$Pre_PA<- predict (rh.rf, newdata = latt.df[,-1], type = "response")
head(latt.p)

# Export predicted value ####
write.csv(latt.p, file = "D:/Rhododendron Git/Rhododendron/outputs/Presence with other species presence as absence/latt.predicted_cowanianum spp vs other sppecies absence.csv")




# Plot predicted value
library(ggplot2)
pp<-ggplot(latt.p, aes(long, lat, col=Pre_PA))+
  geom_point() +coord_fixed()
print(pp)

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
AUC=rh.AUC@y.values[[1]]
AUC
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate")
abline(a=0,b=1, lty=3)  # diagonal line



##################################################################
#### K-fold corss validation in RandomForest
#### K-fold CV ####
# To partition the first fold
k=10
n= floor(nrow(rh.df)/k) # n = size of each fold, and value is rounded by "floor" 
err.vect= rep (NA, k)  # store the error in this vector

i = 1
s1 = ((i-1) * n+1) # the start of the subset
s2 = (i*n)         # The end of the subset
subset = s1:s2     # the range of the subset
# because of round above, the end of the subset may be slightly out of range

# Sort the data set in random ordering (not 1 and 0 segregated)
set.seed(444) ## make reproducible here, but not if generating many random samples
rand <- sample(nrow(rh.df))
rand
rh.df$PA<-as.numeric(rh.df$PA)
rh.df<-rh.df[rand,]
head(rh.df)



cv.train=rh.df[-subset,] # Train the model using this data
table(cv.train$PA)
cv.test = rh.df[subset, ] # test the model's performace on this data
table(cv.test$PA)

# next, move to the second fold:
# i = 2
# .......
# but we automate this by looping

library(verification)


# need to loop over each of the folds
for (i in 1:k){
  s1 = ((i-1) * n+1) # the start of the subset
  s2 = (i*n)         # The end of the subset
  subset = s1:s2     # the range of the subset
  
  cv.train=rh.df[-subset,] # Train the model using this data
  cv.test = rh.df[subset, ] # test the model's performace on this data
  
  # run the random forest on the training data set, exclude on predictor variables
  fit = randomForest (x=cv.train[,-c(1,2,3,32)], y= as.factor (cv.train[,32]))
  prediction = predict (fit, newdata= cv.test[, -c(1,2,3,32)], type="prob")[,2]
  rh.ROC[i] <- performance(fit, 'tpr', 'fpr')
  # calculate the model's accurancy for the ith fold
  err.vect [i] = roc.area (cv.test[ , 32], prediction)$A
  print(paste("AUC for fold", i, ":", err.vect[i]))
}
print(data.frame(err.vect)) # list of AUCs from k fold
print(paste("Average AUC:", mean(err.vect)))


##### AUC value add in ROC plot
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate")
abline(a=0,b=1, lty=3)  # diagonal line
text(0.9,0.1, labels = paste("Average AUC:",round( mean(err.vect),3)))


#########################
##### Predict to Lattice data set for mapping

