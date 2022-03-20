# ===========================================================================
# PostShock_Scenarios.R     -   Naia Ormaza Zulueta 03/2022
#
# This file is used to 
# 1.Estimate a logit model that explains with a 92.5% accuracy the original netw
# 2.Calibrate the parameters left of the model
# 3.Propose a potential post-shock scenario constructed with the calibrated 
# parameters and estimated coefficients for all villages
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
library("Pareto")

setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/R code/")

# ---- Households ----
data_hh <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/household_characteristics.dta")
# Take hh from those villages
hh <- data_hh[data_hh[,1]>=30,]

# Load predicted data
load("final_data.Rdata")

villages <- unique(hh[,1])
l_v <- length(villages)

# ============================= FIRST VILLAGE ==================================

# Doing it for the "vil"th village
i <- 1
vil <- villages[i]

print(paste("----------------------------------------------------------------------------------------"))
print(paste("-------","Running for village ",vil,"-------"))

nodes_ind1 <- 0
nodes_ind2 <- sum(hh[,1]==vil)
vil32 <- final_data[(nodes_ind1+1):nodes_ind2,]

l_vil <- dim(vil32)[1]
c_vil <- dim(vil32)[2]

nodes_ind2 <- nodes_ind1

# --------------------------- FOR THE 32TH VILLAGE ---------------------------
#============================ Summary Statistics ===============================
# Caste ratios
r_sc <- sum(vil32[,3]=="SCHEDULE CASTE")/l_vil
r_obc <- sum(vil32[,3]=="OBC")/l_vil
r_g <- sum(vil32[,3]=="GENERAL")/l_vil

#============================ Logit Regression ===============================

# Read adjacency matrix
file <- paste("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",vil,".csv",sep="")
data_adj <- read.csv(file,header=FALSE)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)
# -- Create graph from adjacency matrix --
g <- graph.adjacency(adjm,mode = c("undirected"))
d <- as.data.frame(distances(g))

# Construct \Delta_{ij}, d
Delta <- integer(length=(l_vil*(l_vil-1))/2)
d_c <- integer(length=(l_vil*(l_vil-1))/2)
d_sc <- integer(length=(l_vil*(l_vil-1))/2)
# Construct amount of common neighbours
com_n <- integer(length=(l_vil*(l_vil-1))/2)

ind <- 1
for (i in 1:(l_vil-1)) {
  for (j in (i+1):l_vil) {
    Delta[ind] <- adjm[i,j]
    if(vil32[i,3]==vil32[j,3]){ d_c[ind] <- 1 }
    if(vil32[i,2]==vil32[j,2]){ d_sc[ind] <- 1 }
    n1 <- neighbors(g, i, mode = c("all"))
    n2 <- neighbors(g, j, mode = c("all"))
    com_n[ind] <- length(n1[is.element(n1,n2)])
    ind <- ind+1
  }
} 

# Combine columns as matrix
mydata <- as.data.frame(cbind(Delta,d_c,d_sc,com_n)) 
mydata$d_c <- factor(mydata$d_c)
mydata$d_sc <- factor(mydata$d_sc)
mylogit <- glm(Delta ~ d_c + d_sc + com_n, data = mydata, family = "binomial")
#summary(mylogit)

# Odds ratios only
#exp(coef(mylogit))
# Odds ratios and 95% CI
#exp(cbind(OR = coef(mylogit), confint(mylogit)))
# Print for Latex
#stargazer(mylogit,title="Results", align=TRUE)

# ================================ Calibration =================================

# Create link matrix
l <- matrix(0,l_vil,l_vil)
# Fix delta
delta <- 0.5
set.seed(1234)
xi <- 1/rPareto(l_vil, 4.5, 3.1)
est <- coef(mylogit)[-1]
for (i in 1:(l_vil-1)) {
  for (j in (i+1):l_vil) {
    n1 <- neighbors(g, i, mode = c("all"))
    n2 <- neighbors(g, j, mode = c("all"))
    lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
            +est[3]*length(n1[is.element(n1,n2)]))*xi[i]
    if(lhs>delta){
      if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
        l[i,j] <- 1
        l[j,i] <- 1
      }else{
        p <- 1/abs((xi[i]-xi[j])*100)
        link <- sample(c(0,1),1,prob=c(1-p,p))
        l[i,j] <- link
        l[j,i] <- link 
      }
    }
  }
}

for (i in 2:(l_vil)) {
  for (j in 1:(i-1)) {
    n1 <- neighbors(g, i, mode = c("all"))
    n2 <- neighbors(g, j, mode = c("all"))
    lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
            +est[3]*length(n1[is.element(n1,n2)]))*xi[i]
    if(lhs>delta){
      if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
        l[i,j] <- 1
        l[j,i] <- 1
      }else{
      p <- 1/abs((xi[i]-xi[j])*100)
      link <- sample(c(0,1),1,prob=c(1-p,p))
      l[i,j] <- link
      l[j,i] <- link 
    }
  }
  }
}

l <- as.data.frame(l)
A <- as.data.frame(adjm)
accuracy <- sum(l==A)/(l_vil*l_vil)*100

if(accuracy<0.85){
  print(paste("WARNING: Accuracy is: ", accuracy,"%"))
}else{
  print(paste("Accuracy is: ", accuracy, "%"))
}
print(paste("# links in the original network: ",sum(adjm)))
print(paste("# links in the calibrated network: ",sum(l)))

# ================================ Shock Design ================================

# ------------------ Changing empathy value --------------------
alt <- 0.15
xi_s <- xi*(1+alt)
A_s_e <- adjm


for (i in 1:(l_vil-1)) {
  for (j in (i+1):l_vil) {
    n1 <- neighbors(g, i, mode = c("all"))
    n2 <- neighbors(g, j, mode = c("all"))
    lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
            +est[3]*length(n1[is.element(n1,n2)]))*xi_s[i]
    if(lhs>delta){
      if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
        A_s_e[i,j] <- 1
        A_s_e[j,i] <- 1
      }else{
        p <- 1/abs((xi[i]-xi[j])*100)
        link <- sample(c(0,1),1,prob=c(1-p,p))
        A_s_e[i,j] <- link
        A_s_e[j,i] <- link 
      }
    }
  }
}




for (i in 2:(l_vil)) {
  for (j in 1:(i-1)) {
    n1 <- neighbors(g, i, mode = c("all"))
    n2 <- neighbors(g, j, mode = c("all"))
    lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
            +est[3]*length(n1[is.element(n1,n2)]))*xi_s[i]
    if(lhs>delta){
      if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
        A_s_e[i,j] <- 1
        A_s_e[j,i] <- 1
      }else{
        p <- 1/abs((xi[i]-xi[j])*100)
        link <- sample(c(0,1),1,prob=c(1-p,p))
        A_s_e[i,j] <- link
        A_s_e[j,i] <- link 
      }
    }
  }
}

new_l <- (sum(A_s_e)-sum(adjm))/2
print(paste(new_l, " new links created after the shock if the empathy is altered by",alt*100,"%"))


# ----------------------------------------------------
# ========================== GRAPHS PRE/POST ==========================
# ----------------------------------------------------
# -- PRE --
data_adj <- as.data.frame(adjm)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)
g1 <- graph.adjacency(adjm,mode = c("undirected"))
nodes <- vil32[,1:2]

# -- POST  --
adjm_e <- as.matrix(A_s_e)
data_adj <- as.data.frame(adjm_e)
colnames(data_adj)<-c(1:dim(data_adj)[1])
adjm <- as.matrix(data_adj)
g2 <- graph.adjacency(adjm,mode = c("undirected"))
nodes <- vil32[,1:2]


# ----------------------------------------------------
# ========================== COMMUNITY DETECTION ==========================
# ----------------------------------------------------

# --------------- G-N COMMUNITY DETECTION  ---------------

# ---- Pre-shock -----
eb <- cluster_edge_betweenness(g1)
com1 <- length(unique(eb$membership))
# ---- Post-shock -----
eb_shock <- cluster_edge_betweenness(g2)
com2 <- length(unique(eb_shock$membership))



#  --------------- GREEDY COMMUNITY DETECTION  ---------------
# PRE
c1 = cluster_fast_greedy(g1)
com_greedy1 <- length(unique(c1$membership))
# POST
c2 = cluster_fast_greedy(g2)
com_greedy2 <- length(unique(c2$membership))


#  --------------- SPECTRAL COMMUNITY DETECTION  ---------------
# PRE
c_sp1 = cluster_leading_eigen(g1)
com_sp1 <- length(unique(c_sp1$membership))
# POST
c_sp2= cluster_leading_eigen(g2)
com_sp2 <- length(unique(c_sp2$membership))



# ========================== Properties ============================

# Diameter
diam <- diameter(g1,directed=FALSE)
diam_s <- diameter(g2,directed=FALSE)
# Density
dens <- ecount(g1)/(vcount(g1)*(vcount(g1)-1))
dens_s <- ecount(g2)/(vcount(g2)*(vcount(g2)-1))
# Average path length
avlp <- mean_distance(g1, directed=FALSE)
avlp_s <- mean_distance(g2, directed=FALSE)
# Clustering coefficient
cls_c <- transitivity(g1, type="global")
cls_c_s <- transitivity(g2, type="global")
# Degree centrality
deg <- degree(g1) 
deg_s <- degree(g2) 
max_deg <- which.max(deg)     # who's got the max degree?
max_deg_s <- which.max(deg_s)
degc <- centr_degree(g1, normalized=T)
degc_s <- centr_degree(g2, normalized=T)
# Eigen centrality
eigc <- eigen_centrality(g1, directed=F, weights=NA)
eigc_s <- eigen_centrality(g2, directed=F, weights=NA)
max_eigc <- which.max(eigc$vector)    # who's got the max eig centr value?
max_eigc_s <- which.max(eigc_s$vector)
# Betweenness centrality
bet <- betweenness(g1, directed=F, weights=NA)
bet_s <- betweenness(g2, directed=F, weights=NA)
max_bet <- which.max(bet)
max_betscaste <- vil32[max_bet,2]
max_bet_s <- which.max(bet_s)
max_bet_s_scaste <- vil32[max_bet_s,2]


# ---------------------- Summary table ------------------------------------
Village <- c(vil,vil)
State <- c("Pre-shock", "Post-shock")
df_final <- data.frame(Village,State)
df_final$Accuracy <- c(accuracy,accuracy)
df_final$Diameter <- c(diam,diam_s)
df_final$Density <- c(dens,dens_s)
df_final$AvPathLength <- c(avlp,avlp_s)
df_final$Max_degree <- c(max(deg),max(deg_s))
df_final$Max_degree_node <- c(which.max(deg),which.max(deg_s))
df_final$Max_degreesc <- c(vil32[max_deg,2],vil32[max_deg_s,2])
df_final$MaxEigcentr <- c(max(eigc$vector),max(eigc_s$vector))
df_final$MaxEigcentr_node <- c(which.max(eigc$vector),which.max(eigc_s$vector))
df_final$MaxEigcentrsc <- c(vil32[max_eigc,2],vil32[max_eigc_s,2])
df_final$MaxBetw <- c(max(bet),max(bet_s))
df_final$MaxBetw_node <- c(which.max(bet),which.max(bet_s))
df_final$MaxBetwsc <- c(vil32[max_bet,2],vil32[max_bet_s,2])
df_final$EB_communities <- c(com1,com2)
df_final$GREEDY_communities <- c(com_greedy1,com_greedy2)
df_final$SPECTRAL_communities <- c(com_sp1,com_sp2)

nodes_ind1 <- nodes_ind2

# ======================= FOR LOOP FOR ALL VILLAGES ============================

for (i in 2:l_vil) {
  # Doing it for the "vil"th village
  vil <- villages[i]
  
  # Keep track of nodes' indices
  nodes_ind2 <- nodes_ind1 + sum(hh[,1]==vil)
  
  print(paste("----------------------------------------------------------------------------------------"))
  print(paste("-------","Running for village ",vil,"-------"))
  
  nodes_ind1 <- 0
  nodes_ind2 <- sum(hh[,1]==vil)
  vil32 <- final_data[(nodes_ind1+1):nodes_ind2,]
  
  l_vil <- dim(vil32)[1]
  c_vil <- dim(vil32)[2]
  
  nodes_ind2 <- nodes_ind1
  
  # --------------------------- FOR THE 32TH VILLAGE ---------------------------
  #============================ Summary Statistics ===============================
  # Caste ratios
  r_sc <- sum(vil32[,3]=="SCHEDULE CASTE")/l_vil
  r_obc <- sum(vil32[,3]=="OBC")/l_vil
  r_g <- sum(vil32[,3]=="GENERAL")/l_vil
  
  #============================ Logit Regression ===============================
  
  # Read adjacency matrix
  file <- paste("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/1. Network Data/Adjacency Matrices/adj_allVillageRelationships_HH_vilno_",vil,".csv",sep="")
  data_adj <- read.csv(file,header=FALSE)
  colnames(data_adj)<-c(1:dim(data_adj)[1])
  adjm <- as.matrix(data_adj)
  # -- Create graph from adjacency matrix --
  g <- graph.adjacency(adjm,mode = c("undirected"))
  d <- as.data.frame(distances(g))
  
  # Construct \Delta_{ij}, d
  Delta <- integer(length=(l_vil*(l_vil-1))/2)
  d_c <- integer(length=(l_vil*(l_vil-1))/2)
  d_sc <- integer(length=(l_vil*(l_vil-1))/2)
  # Construct amount of common neighbours
  com_n <- integer(length=(l_vil*(l_vil-1))/2)
  
  ind <- 1
  for (i in 1:(l_vil-1)) {
    for (j in (i+1):l_vil) {
      Delta[ind] <- adjm[i,j]
      if(vil32[i,3]==vil32[j,3]){ d_c[ind] <- 1 }
      if(vil32[i,2]==vil32[j,2]){ d_sc[ind] <- 1 }
      n1 <- neighbors(g, i, mode = c("all"))
      n2 <- neighbors(g, j, mode = c("all"))
      com_n[ind] <- length(n1[is.element(n1,n2)])
      ind <- ind+1
    }
  } 
  
  # Combine columns as matrix
  mydata <- as.data.frame(cbind(Delta,d_c,d_sc,com_n)) 
  mydata$d_c <- factor(mydata$d_c)
  mydata$d_sc <- factor(mydata$d_sc)
  mylogit <- glm(Delta ~ d_c + d_sc + com_n, data = mydata, family = "binomial")
  #summary(mylogit)
  
  # Odds ratios only
  #exp(coef(mylogit))
  # Odds ratios and 95% CI
  #exp(cbind(OR = coef(mylogit), confint(mylogit)))
  # Print for Latex
  #stargazer(mylogit,title="Results", align=TRUE)
  
  # ================================ Calibration =================================
  
  # Create link matrix
  l <- matrix(0,l_vil,l_vil)
  # Fix delta
  delta <- 0.5
  set.seed(1234)
  xi <- 1/rPareto(l_vil, 5, 2.7)
  est <- coef(mylogit)[-1]
  for (i in 1:(l_vil-1)) {
    for (j in (i+1):l_vil) {
      n1 <- neighbors(g, i, mode = c("all"))
      n2 <- neighbors(g, j, mode = c("all"))
      lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
              +est[3]*length(n1[is.element(n1,n2)]))*xi[i]
      if(lhs>delta){
        if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
          l[i,j] <- 1
          l[j,i] <- 1
        }else{
          p <- 1/abs((xi[i]-xi[j])*100)
          link <- sample(c(0,1),1,prob=c(1-p,p))
          l[i,j] <- link
          l[j,i] <- link 
        }
      }
    }
  }
  
  for (i in 2:(l_vil)) {
    for (j in 1:(i-1)) {
      n1 <- neighbors(g, i, mode = c("all"))
      n2 <- neighbors(g, j, mode = c("all"))
      lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
              +est[3]*length(n1[is.element(n1,n2)]))*xi[i]
      if(lhs>delta){
        if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
          l[i,j] <- 1
          l[j,i] <- 1
        }else{
          p <- 1/abs((xi[i]-xi[j])*100)
          link <- sample(c(0,1),1,prob=c(1-p,p))
          l[i,j] <- link
          l[j,i] <- link 
        }
      }
    }
  }
  
  l <- as.data.frame(l)
  A <- as.data.frame(adjm)
  accuracy <- sum(l==A)/(l_vil*l_vil)*100
  
  if(accuracy<0.85){
    print(paste("WARNING: Accuracy is: ", accuracy,"%"))
  }else{
    print(paste("Accuracy is: ", accuracy, "%"))
  }
  print(paste("# links in the original network: ",sum(adjm)))
  print(paste("# links in the calibrated network: ",sum(l)))
  
  # ================================ Shock Design ================================
  
  # ------------------ Changing empathy value --------------------
  alt <- 0.15
  xi_s <- xi*(1+alt)
  A_s_e <- adjm
  
  
  for (i in 1:(l_vil-1)) {
    for (j in (i+1):l_vil) {
      n1 <- neighbors(g, i, mode = c("all"))
      n2 <- neighbors(g, j, mode = c("all"))
      lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
              +est[3]*length(n1[is.element(n1,n2)]))*xi_s[i]
      if(lhs>delta){
        if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
          A_s_e[i,j] <- 1
          A_s_e[j,i] <- 1
        }else{
          p <- 1/abs((xi[i]-xi[j])*100)
          link <- sample(c(0,1),1,prob=c(1-p,p))
          A_s_e[i,j] <- link
          A_s_e[j,i] <- link 
        }
      }
    }
  }
  
  
  for (i in 2:(l_vil)) {
    for (j in 1:(i-1)) {
      n1 <- neighbors(g, i, mode = c("all"))
      n2 <- neighbors(g, j, mode = c("all"))
      lhs <- (est[1]*as.numeric(vil32[i,3]==vil32[j,3]) + est[2]*as.numeric(vil32[i,2]==vil32[j,2])
              +est[3]*length(n1[is.element(n1,n2)]))*xi_s[i]
      if(lhs>delta){
        if (round(xi[i],digits=1)<=round(xi[j],digits=1)|(abs(xi[i]-xi[j])<0.05)){
          A_s_e[i,j] <- 1
          A_s_e[j,i] <- 1
        }else{
          p <- 1/abs((xi[i]-xi[j])*100)
          link <- sample(c(0,1),1,prob=c(1-p,p))
          A_s_e[i,j] <- link
          A_s_e[j,i] <- link 
        }
      }
    }
  }
  
  new_l <- (sum(A_s_e)-sum(adjm))/2
  print(paste(new_l, " new links created after the shock if the empathy is altered by",alt*100,"%"))
  
  
  # ----------------------------------------------------
  # ========================== GRAPHS PRE/POST ==========================
  # ----------------------------------------------------
  # -- PRE --
  g1 <- graph.adjacency(adjm,mode = c("undirected"))
  Isolated <- which(degree(g1)==0)
  g1 <- delete.vertices(g1, Isolated)
  nodes <- vil32[,1:2]
  
  # -- POST --
  adjm_e <- as.matrix(A_s_e)
  data_adj <- as.data.frame(adjm_e)
  colnames(data_adj)<-c(1:dim(data_adj)[1])
  adjm_e <- as.matrix(data_adj)
  g2 <- graph.adjacency(adjm_e,mode = c("undirected"))
  Isolated <- which(degree(g2)==0)
  g2 <- delete.vertices(g2, Isolated)
  
  
  # ----------------------------------------------------
  # ========================== COMMUNITY DETECTION ==========================
  # ----------------------------------------------------
  
  # --------------- G-N COMMUNITY DETECTION  ---------------
  
  # ---- Pre-shock -----
  eb <- cluster_edge_betweenness(g1)
  com1 <- length(unique(eb$membership))
  # ---- Post-shock -----
  eb_shock <- cluster_edge_betweenness(g2)
  com2 <- length(unique(eb_shock$membership))
  
  
  
  #  --------------- GREEDY COMMUNITY DETECTION  ---------------
  # PRE
  c1 = cluster_fast_greedy(g1)
  com_greedy1 <- length(unique(c1$membership))
  # POST
  c2 = cluster_fast_greedy(g2)
  com_greedy2 <- length(unique(c2$membership))
  
  
  #  --------------- SPECTRAL COMMUNITY DETECTION  ---------------
  # PRE
  c_sp1 = cluster_leading_eigen(g1)
  com_sp1 <- length(unique(c_sp1$membership))
  # POST
  c_sp2= cluster_leading_eigen(g2)
  com_sp2 <- length(unique(c_sp2$membership))
  
  
  
  # ========================== Properties ============================
  
  # Diameter
  diam <- diameter(g1,directed=FALSE)
  diam_s <- diameter(g2,directed=FALSE)
  # Density
  dens <- ecount(g1)/(vcount(g1)*(vcount(g1)-1))
  dens_s <- ecount(g2)/(vcount(g2)*(vcount(g2)-1))
  # Average path length
  avlp <- mean_distance(g1, directed=FALSE)
  avlp_s <- mean_distance(g2, directed=FALSE)
  # Clustering coefficient
  cls_c <- transitivity(g1, type="global")
  cls_c_s <- transitivity(g2, type="global")
  # Degree centrality
  deg <- degree(g1) 
  deg_s <- degree(g2) 
  max_deg <- which.max(deg)     # who's got the max degree?
  max_deg_s <- which.max(deg_s)
  degc <- centr_degree(g1, normalized=T)
  degc_s <- centr_degree(g2, normalized=T)
  # Eigen centrality
  eigc <- eigen_centrality(g1, directed=F, weights=NA)
  eigc_s <- eigen_centrality(g2, directed=F, weights=NA)
  max_eigc <- which.max(eigc$vector)    # who's got the max eig centr value?
  max_eigc_s <- which.max(eigc_s$vector)
  # Betweenness centrality
  bet <- betweenness(g1, directed=F, weights=NA)
  bet_s <- betweenness(g2, directed=F, weights=NA)
  max_bet <- which.max(bet)
  max_betscaste <- vil32[max_bet,2]
  max_bet_s <- which.max(bet_s)
  max_bet_s_scaste <- vil32[max_bet_s,2]
  
  
  # ---------------------- Summary table ------------------------------------
  Village <- c(vil,vil)
  State <- c("Pre-shock", "Post-shock")
  df <- data.frame(Village,State)
  df$Accuracy <- c(accuracy,accuracy)
  df$Diameter <- c(diam,diam_s)
  df$Density <- c(dens,dens_s)
  df$AvPathLength <- c(avlp,avlp_s)
  df$Max_degree <- c(max(deg),max(deg_s))
  df$Max_degree_node <- c(which.max(deg),which.max(deg_s))
  df$Max_degreesc <- c(vil32[max_deg,2],vil32[max_deg_s,2])
  df$MaxEigcentr <- c(max(eigc$vector),max(eigc_s$vector))
  df$MaxEigcentr_node <- c(which.max(eigc$vector),which.max(eigc_s$vector))
  df$MaxEigcentrsc <- c(vil32[max_eigc,2],vil32[max_eigc_s,2])
  df$MaxBetw <- c(max(bet),max(bet_s))
  df$MaxBetw_node <- c(which.max(bet),which.max(bet_s))
  df$MaxBetwsc <- c(vil32[max_bet,2],vil32[max_bet_s,2])
  df$EB_communities <- c(com1,com2)
  df$GREEDY_communities <- c(com_greedy1,com_greedy2)
  df$SPECTRAL_communities <- c(com_sp1,com_sp2)
  
  df_final <- rbind(df_final,df)
  nodes_ind1 <- nodes_ind2
}

setwd("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Outcomes/")

save(df_final, file="df_final_emp15.Rdata")

write.csv(df_final, file = paste("data_summary_emp15.csv"))


dat <- df_final
toDelete <- seq(1, nrow(dat), 2)
dat <- dat[ toDelete ,]

# ---------------------- Pre-statistics ------------------------------------
# Degree
rt_sc <- ((sum(dat[,9]=="ADIKARNATAKA")+sum(dat[,9]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,9]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)
# Eigenvector
rt_sc <- ((sum(dat[,12]=="ADIKARNATAKA")+sum(dat[,12]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,12]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)
# Betweenness
rt_sc <- ((sum(dat[,15]=="ADIKARNATAKA")+sum(dat[,15]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,15]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)

# ---------------------- Post-statistics ------------------------------------
dat <- df_final
toDelete <- seq(2, nrow(dat), 2)
dat <- dat[ toDelete ,]

# Degree
rt_sc <- ((sum(dat[,9]=="ADIKARNATAKA")+sum(dat[,9]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,9]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)
# Eigenvector
rt_sc <- ((sum(dat[,12]=="ADIKARNATAKA")+sum(dat[,12]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,12]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)
# Betweenness
rt_sc <- ((sum(dat[,15]=="ADIKARNATAKA")+sum(dat[,15]=="BHOVI")))/dim(dat)[1]
rt_ge <- (sum(dat[,15]=="LINGAYATH"))/dim(dat)[1]
rt_obc <- (1-rt_sc-rt_ge)