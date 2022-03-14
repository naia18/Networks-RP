# ===========================================================================
# All_predictions.R     -   Naia Ormaza Zulueta 03/2022
#
# This file prepares the data into test_data and train_data in order to perform  
# attribute (subcaste) prediction for all missing nodes in all 47 villages
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
# Load cleaned data
load("individual_data.RData")

# -----------------------------------------------------------------------------
# ---- Individuals ----
data_ind <- data_ind[duplicated(data_ind[,11]),]
# Take individuals from those villages
ind <- data_ind[data_ind[,1]>=30,]
ind <- ind[!duplicated(ind[,4]),]
ind_dat <- ind %>% select('hhid', 'subcaste')

# ---- Households ----
data_hh <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/household_characteristics.dta")
# Take hh from those villages
hh <- data_hh[data_hh[,1]>=30,]
hh[hh[,'castesubcaste'] == "",'castesubcaste'] <- "OBC"
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

traintest <- merge(x = ind_dat, y = train, by = "hhid",all.y=TRUE)
scastes <- unlist(unique(nodes[,4]))[-1]
# Create one column for each subcast
traintest[scastes] <- 0

# To make reference to the nodes index
nodes_ind1 <- 0

# ==================  Start with first village: initialise =====================
vil <- 30
file <- paste("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",vil,".csv",sep="")
data_adj <- read.csv(file,header=FALSE)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)
# -- Create graph from adjacency matrix --
g <- graph.adjacency(adjm,mode = c("undirected"))

# Keep track of nodes' indices
nodes_ind2 <- nodes_ind1 + sum(hh[,1]==vil)
# --------------------- TO TRAIN/TEST -------------------------
V(g)$subcaste <- nodes_na[(nodes_ind1+1):(nodes_ind2),3]


# Create columns that count amount of first neighbours of each subcaste
for (i in (nodes_ind1+1):(nodes_ind2)) {
  for (elem in unique(V(g)$subcaste)) {
    # Add column with neighbour attributes
    index <- which(scastes==elem)
    traintest[i,14+index] <- sum(V(g)[unlist(ego(g,1,V(g),mode='all',mindist=1)[i])]$subcaste==scastes[index])
  }
}
tt <- traintest[(nodes_ind1+1):nodes_ind2,]
train_total <- tt[!is.na(tt[,2]),][,-1] 
test_total <- tt[is.na(tt[,2]),][,-c(1)]
nodes_ind1 <- nodes_ind2

start_time <- Sys.time()

# ===================== Run for loop for the rest of the vil ===================

for (vil in 31:77) {
  # Create graph
  file <- paste("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",vil,".csv",sep="")
  data_adj <- read.csv(file,header=FALSE)
  colnames(data_adj)<-c(1:dim(data_adj)[1])
  adjm <- as.matrix(data_adj)
  # -- Create graph from adjacency matrix --
  g <- graph.adjacency(adjm,mode = c("undirected"))
  
  # Keep track of nodes' indices
  nodes_ind2 <- nodes_ind1 + sum(hh[,1]==vil)
  # --------------------- TO TRAIN/TEST -------------------------
  V(g)$subcaste <- nodes_na[(nodes_ind1+1):(nodes_ind2),3]
  
  print(paste("Village number",vil,"running"))
  print(paste("Node index: ",nodes_ind2))
  
  # Create columns that count amount of first neighbours of each subcaste
  for (i in 1:dim(data_adj)[1]) {
    for (elem in unique(V(g)$subcaste)) {
      if(elem!="0"){
        # Add column with neighbour attributes
        index <- which(scastes==elem)
        traintest[(i+nodes_ind1):nodes_ind2,14+index] <- sum(V(g)[unlist(ego(g,1,V(g),mode='all',mindist=1)[i])]$subcaste==scastes[index])
      }
    }
  }
  # Take only the fraction of the traintest dataset that belongs to the village
  tt <- traintest[(nodes_ind1+1):nodes_ind2,]
  train_set <- tt[!is.na(tt[,2]),][,-1] 
  test_set <- tt[is.na(tt[,2]),][,-1] 
  #true_cl <- train_set[,1]
  #train_set <- train_set[-1]
  # ----------------------------------------------------------------------------
  
  # Append data to the _total matrix
  train_total <- rbind(train_total,train_set)
  test_total <- rbind(test_total, test_set)
  nodes_ind1 <- nodes_ind2
}

save(train_total, file="train_77.Rdata")
save(test_total, file="test_77.Rdata")

end_time <- Sys.time()
dif_time <- end_time - start_time
print(paste("Running time:", dif_time))
# ==============================================================================

