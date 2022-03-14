# ===========================================================================
# PostShock_Scenarios.R     -   Naia Ormaza Zulueta 03/2022
#
# This file yields simulations for potential post-shock scenarios
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
library("stargazer")

setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/R code/")

# ---- Households ----
data_hh <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/household_characteristics.dta")
# Take hh from those villages
hh <- data_hh[data_hh[,1]>=30,]

load("final_data.Rdata")

# Income vector
y = c(0:10)  

# Define different probabilies: 
# General Category (GC)
p_gc <- c(0.01, 0.01, 0.01, 0.02, 0.05, 0.05, 0.07, 0.15, 0.25, 0.25, 0.13)
# Other Backgrownd Castes (OBC)
p_obc <- c(p_gc[5:10],p_gc[10:11],p_gc[1:3])
# Scheduled Caste (SC)
p_sc <- rev(p_gc)

# Doing it for the 30th village
vil <- 30

nodes_ind1 <- 0
nodes_ind2 <- sum(hh[,1]==vil)
vil30 <- final_data[(nodes_ind1+1):nodes_ind2,]

# Create column of income
vil30$income <- 0
l_vil <- dim(vil30)[1]
c_vil <- dim(vil30)[2]
# Reorder columns
vil30 <- vil30[,c(1:3,c_vil,4:(c_vil-1))]

for (i in 1:l_vil) {
  if(vil30[i,3]=="GENERAL"){
    vil30[i,4]<- sample(x=y,size=1,prob=p_gc)   # If GC
  } else if (vil30[i,3]=="OBC"){
    vil30[i,4] <- sample(x=y,size=1,prob=p_obc) # If OBC
  } else {
    vil30[i,4] <- sample(x=y,size=1,prob=p_sc)  # If SC or ST or minority
  }
}

nodes_ind2 <- nodes_ind1
#============================ Logit Regression ===============================

# Read adjacency matrix
file <- paste("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",vil,".csv",sep="")
data_adj <- read.csv(file,header=FALSE)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)

# Construct \Delta_{ij}, d
Delta <- integer(length=(l_vil*(l_vil-1))/2)
d_c <- integer(length=(l_vil*(l_vil-1))/2)
d_sc <- integer(length=(l_vil*(l_vil-1))/2)

ind <- 1
for (i in 1:(l_vil-1)) {
  for (j in (i+1):l_vil) {
    Delta[ind] <- adjm[i,j]
    if(vil30[i,3]==vil30[j,3]){ d_c[ind] <- 1 }
    if(vil30[i,2]==vil30[j,2]){ d_sc[ind] <- 1 }
    ind <- ind+1
  }
} 

# Combine columns as matrix
mydata <- as.data.frame(cbind(Delta,d_c,d_sc)) 
mydata$d_c <- factor(mydata$d_c)
mydata$d_sc <- factor(mydata$d_sc)
mylogit <- glm(Delta ~ d_c + d_sc, data = mydata, family = "binomial")
summary(mylogit)

## odds ratios only
exp(coef(mylogit))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# Print for Latex
stargazer(mylogit,title="Results", align=TRUE)
