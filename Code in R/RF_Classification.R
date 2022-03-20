# ===========================================================================
# RF_classification.R   -    Naia Ormaza Zulueta 03/2022
#
# This file loads the ready train_data and test_data datasets and performs 
# different RF classifications on the test_data with the model trained in the
# train_data. We predict the subcaste of the nodes that were not reported in the
# individual_characteristics.dta file provided by Banerjee et al. for all 
# villages in the sample for which we have data on caste ([30,77])
#
# ===========================================================================

rm(list = ls())

# Import libraries
library("foreign")
library("igraph")
library("tidyverse")
library("class")
library("rlist")
library("randomForest")

# Set working directory
setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/R code/")
# Load train and test data
load("train_40.Rdata")
load("test_40.Rdata")

# Lehenengo zutabea ez da behar
test_total <- test_total[,-1]
train_total <- na.omit(train_total)
test_total <- na.omit(test_total)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (at random)
set.seed(100)
# Turn subcastes into factors
train_total$subcaste <- factor(train_total$subcaste) 
train <- sample(nrow(train_total), 0.7*nrow(train_total), replace = FALSE)
TrainSet <- train_total[train,]
ValidSet <- train_total[-train,]
#summary(TrainSet)
#summary(ValidSet)
TrainSet <- droplevels(TrainSet)
ValidSet <- droplevels(ValidSet)


# ----------------- Different model specifications ----------------------

# ---- Model1: Simplest ----
model1 <- randomForest(subcaste ~ ., data = TrainSet, importance = TRUE)
model1
# ---- Model2: mtry ----
model2 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, 
                       mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$subcaste)  

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Make sure both share same factor levels (unlikely for small samples of villages)
levels(ValidSet$subcaste) <- c(levels(ValidSet$subcaste),levels(predValid))
levels(predValid) <- c(levels(predValid), levels(ValidSet$subcaste))
# Checking classification accuracy
mean(na.omit(predValid == ValidSet$subcaste))                    
table(predValid,ValidSet$subcaste)

# Check important vars
importance(model2)        
varImpPlot(model2)   

# Tune mtry for model
a=c()
for (i in 9:16) {
  model3 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, 
                         mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  levels(ValidSet$subcaste) <- c(levels(ValidSet$subcaste),levels(predValid))
  levels(predValid) <- c(levels(predValid), levels(ValidSet$subcaste))
  a[i-8] = mean(na.omit(predValid == ValidSet$subcaste))
}
plot(9:16,a)

# ---- Model4: Tuned mtry ----
model4 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, 
                       mtry = (which.max(a)+7), importance = TRUE)
model4

# Predicting on Validation set
predValid <- predict(model4, ValidSet, type = "class")
# Make sure both share same factor levels (unlikely for small samples of villages)
levels(ValidSet$subcaste) <- c(levels(ValidSet$subcaste),levels(predValid))
levels(predValid) <- c(levels(predValid), levels(ValidSet$subcaste))
# Checking classification accuracy
mean(na.omit(predValid == ValidSet$subcaste))                    
table(predValid,ValidSet$subcaste)

# Variable Importance
varImpPlot(model4,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")


# ================= Predict on the test dataset ===============================
test_total$subcaste <- predict(model4,newdata=test_total)
# Reorder columns
test_total <- test_total[,c(85,1:84)]

# ============================= Save results ===================================
final_data <- rbind(train_total, test_total)
final_data <- tibble::rownames_to_column(final_data, "id")
final_data[,1] <- as.integer(final_data[,1])
final_data <- final_data[order(final_data[,1]),]
save(final_data, file="final_data.Rdata")

