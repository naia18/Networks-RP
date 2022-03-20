# ===========================================================================
# Vil_Visualization.R     -   Naia Ormaza Zulueta 03/2022
#
# In this file we can take one single village, visualise it using the visNetwork
# library, run a RF classifier on the village and visualise the post-prediction
# network
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
library("visNetwork")

setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/R code/")

# =========================== Prepare the data ===============================

data_ind <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/individual_characteristics.dta")
# Take individuals from a specific village
ind <- data_ind[data_ind[,1]==30,]
ind <- ind[!duplicated(ind[,4]),]
ind_dat <- ind %>% select('hhid', 'subcaste')
ind_dat[ind_dat[,2] == "ADI  KARNATAKA",2] <- "ADI KARNATAKA"
ind_dat[ind_dat[,2] == "ADI KARNATAKA",2] <- "ADIKARNATAKA"

data_hh <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/household_characteristics.dta")
# Take hh from a specific village
hh <- data_hh[data_hh[,1]==30,]
hh_dat <- hh %>% select('hhid','castesubcaste')
train <- hh %>% select('hhid',6,7:17)

# TO DISPLAY 
nodes <- merge(x = hh_dat, y = ind_dat, by = "hhid",all.x=TRUE)
# Call all the NA's as NS: Non-specified
nodes_na <- nodes
nodes_na[is.na(nodes_na)] <- 0
nodes[is.na(nodes)] <- 'NS'
# Add id column
nodes <- tibble::rowid_to_column(nodes, "id")

# Clean the badly written subcaste
nodes_na[nodes_na[,3] == "ADI  KARNATAKA",3] <- "ADI KARNATAKA"
nodes_na[nodes_na[,3] == "ADI KARNATAKA",3] <- "ADIKARNATAKA"

nodes[nodes[,4] == "ADI  KARNATAKA",4] <- "ADI KARNATAKA"
nodes[nodes[,4] == "ADI KARNATAKA",4] <- "ADIKARNATAKA"

# Create graph
data_adj <- read.csv("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_30.csv",header=FALSE)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)
# -- Create graph from adjacency matrix --
g <- graph.adjacency(adjm,mode = c("undirected"))


# --------------------- TO TRAIN/TEST -------------------------
traintest <- merge(x = ind_dat, y = train, by = "hhid",all.y=TRUE)
traintest[na.omit(traintest[,2] == "ADI  KARNATAKA"),2] <- "ADI KARNATAKA"
traintest[na.omit(traintest[,2] == "ADI KARNATAKA"), 2] <- "ADIKARNATAKA"
V(g)$subcaste <- nodes_na[,3]
scastes <- unlist(unique(nodes[,4]))[-1]
traintest[scastes] <- 0

# Create columns that count amount of first neighbours of each subcaste
for (i in 1:dim(hh)[1]) {
  for (j in 1:length(scastes)) {
    # Add column with neighbour attributes
    traintest[i,14+j] <- sum(V(g)[unlist(ego(g,1,V(g),mode='all',mindist=1)[i])]$subcaste==scastes[j])
  }
}
train_set <- traintest[!is.na(traintest[,2]),][-1] 
#true_cl <- train_set[,1]
#train_set <- train_set[-1]
test_set <- traintest[is.na(traintest[,2]),][-c(1,2)]
# ----------------------------------------------------------------------------


# --------------- NETWORK VISUALIZATION --------------------------------------
# -- Create vnodes and vlinks for visNetwork visualization --
vlinks <- as.data.frame(get.edgelist(g))
# change column names of edges V1<->from, V2<->to
names(vlinks)[1]<-'from'
names(vlinks)[2]<-'to'
vnodes <- as.data.frame(nodes)

# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- vnodes
vis.links <- vlinks

vis.nodes$subcaste_Num <- as.numeric(nodes[,4]=="BHOVI") + 2*as.numeric(nodes[,4]=="VOKKALIGA") + 3*as.numeric(nodes[,4]=="ADIKARNATAKA")  +  4*as.numeric(nodes[,4]=="NS") + 5*as.numeric(nodes[,4]=="GOWDAS") + 6*as.numeric(nodes[,4]=="KURUBA")  +  7*as.numeric(nodes[,4]=="LINGAYATH") + 8*as.numeric(nodes[,4]=="KURUBARU")  +  9*as.numeric(nodes[,4]=="BAJANTHRI")

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$subcaste # Text on click
vis.nodes$label  <- vis.nodes$id_hh # Node label
vis.nodes$borderWidth <- 2 # Node border width

vis.nodes$color.background <- c("yellowgreen", "salmon", "turquoise", "plum", "orchid", "palegreen", "seagreen", "seashell", "grey")[vis.nodes$subcaste_Num]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visnet<- visNetwork(vis.nodes, vis.links)

visOptions(visnet, highlightNearest = TRUE, selectedBy = "subcaste")

# ------------------------------------------------------------------------



# -------------------------- ATTRIBUTE PREDICTION -----------------------------

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train_set <- na.omit(train_set)
test_set <- na.omit(test_set)
train_set$subcaste <- factor(train_set$subcaste) 
train <- sample(nrow(train_set), 0.7*nrow(train_set), replace = FALSE)
TrainSet <- train_set[train,]
ValidSet <- train_set[-train,]
#summary(TrainSet)
#summary(ValidSet)
TrainSet <- droplevels(TrainSet)
ValidSet <- droplevels(ValidSet)
levels(TrainSet$latrine) <- c(levels(TrainSet$latrine),levels(test_set$latrine))
levels(test_set$latrine) <- c(levels(test_set$latrine), levels(TrainSet$latrine))
levels(ValidSet$latrine) <- c(levels(ValidSet$latrine), levels(TrainSet$latrine))

model1 <- randomForest(subcaste ~ ., data = TrainSet, importance = TRUE)
model1

model2 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
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
mean(predValid == ValidSet$subcaste)                    
table(predValid,ValidSet$subcaste)

# To check important variables
importance(model2)        
varImpPlot(model2)        

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  levels(ValidSet$subcaste) <- c(levels(ValidSet$subcaste),levels(predValid))
  levels(predValid) <- c(levels(predValid), levels(ValidSet$subcaste))
  a[i-2] = mean(predValid == ValidSet$subcaste)
}
plot(3:8,a)

model2 <- randomForest(subcaste ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)

# ===================== Predict on the test dataset ===========================
test_set$subcaste <- predict(model2,newdata=test_set)
# Reorder columns
test_set <- test_set[,c(21,1:20)]

traintest_predicted <- rbind(train_set, test_set)

traintest_predicted <- tibble::rownames_to_column(traintest_predicted, "id")
traintest_predicted[,1] <- as.numeric(traintest_predicted[,1])
traintest_predicted <- traintest_predicted[order(traintest_predicted[,1]),]

save(traintest_predicted,file="RF_prediction.Rdata")

# =============================================================================

load("RF_prediction.Rdata")

nodes <- traintest_predicted[,1:3]

# --------------- NETWORK VISUALIZATION --------------------------------------
# -- Create vnodes and vlinks for visNetwork visualization --
vlinks <- as.data.frame(get.edgelist(g))
# change column names of edges V1<->from, V2<->to
names(vlinks)[1]<-'from'
names(vlinks)[2]<-'to'
vnodes <- as.data.frame(nodes)

# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- vnodes
vis.links <- vlinks

vis.nodes$subcaste_Num <- as.numeric(nodes[,2]=="BHOVI") + 2*as.numeric(nodes[,2]=="VOKKALIGA") + 3*as.numeric(nodes[,2]=="ADIKARNATAKA") + 4*as.numeric(nodes[,2]=="GOWDAS") + 5*as.numeric(nodes[,2]=="KURUBA")  +  6*as.numeric(nodes[,2]=="LINGAYATH") + 7*as.numeric(nodes[,2]=="KURUBARU")  +  8*as.numeric(nodes[,2]=="BAJANTHRI")

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$subcaste # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$borderWidth <- 2 # Node border width

vis.nodes$color.background <- c("yellowgreen", "salmon", "turquoise", "plum", "orchid", "palegreen", "seagreen", "seashell", "grey")[vis.nodes$subcaste_Num]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visnet<- visNetwork(vis.nodes, vis.links)

visOptions(visnet, highlightNearest = TRUE, collapse=TRUE,selectedBy = "subcaste")

# ------------------------------------------------------------------------
