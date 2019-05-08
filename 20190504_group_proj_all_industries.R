###############################################################################
# 0. LOADING LIBRARIES
###############################################################################
if (!require("igraph")) install.packages("igraph"); library(igraph)
if (!require("NetData")) install.packages("NetData"); library(NetData)
if (!require("cluster")) install.packages("cluster"); library(cluster)
if (!require("sna")) install.packages("sna"); library(sna)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("animation")) install.packages("animation", repos = "http://cran.cnr.berkeley.edu/", dependencies = TRUE); library(readxl)
if (!require("readxl")) install.packages("readxl"); library(readxl)

setwd("C:/Users/eviriyakovithya/Documents/GitHub/SNA/Data")

data <- read_excel("Supply_2012_DET_orig.xlsx", sheet = "2012")
names(data)
head(data)
data[is.na(data)] <- 0
industry_ids <- unique(data$Code)
industry_names <- unique(data$'Commodity Description')

combined_df <- data
names(combined_df)
combined_df$Code <- combined_df$'Commodity Description'<-NULL
combined_df[combined_df!=0] <- 1

mat_data <- as.matrix(combined_df)

rownames(mat_data) <- industry_ids
colnames(mat_data) <- industry_ids

par(mfrow=c(1,1))
g_data <- igraph::graph_from_adjacency_matrix(mat_data, mode = "undirected")
plot(g_data)

#-------------------------------------------------
# Walktrap algorithm
#-------------------------------------------------
g_data_wt <- igraph::walktrap.community(g_data, steps=2000,modularity=TRUE)
plot(g_data_wt, g_data)

#-------------------------------------------------
# Modularity maximization algorithm
#-------------------------------------------------

(g_data_m <- igraph::fastgreedy.community(g_data, modularity=TRUE))
plot(g_data_m, g_data)

#-------------------------------------------------
# Edge Betweenness algorithm
#-------------------------------------------------

(g_data_E <- igraph::edge.betweenness.community(g_data, modularity=TRUE))
plot(g_data_E, g_data)

#-------------------------------------------------
# Comparison
#-------------------------------------------------

par(mfrow=c(1,3))
plot(g_data_wt, g_data)
plot(g_data_m, g_data)
plot(g_data_E,g_data)


### n cluster function
n<-nrow(mat_data)
n.cluster <- function(g, type){
  
  AM <- igraph::get.adjacency(g)
  
  ideal_observed_cors = vector()
  ideal_observed_modul = vector()
  
  if(type == 1){
    print('WALKTRAP METHOD')
    community.structure <- igraph::walktrap.community(g, steps = 200)
  }else if(type == 2){
    print('MODULARITY OPTIMIZATION METHOD')
    community.structure <- igraph::fastgreedy.community(g)
  }else if(type == 3){
    print('EDGE BETWEENNESS METHOD')
    community.structure <- edge.betweenness.community(g)
  }else{
    return('NO CLUSTERING METHOD')
  }
  
  for (h in 1:n) {
    num_comms = (n - h + 1)
    community <- cutat(community.structure, no = h)
    idealized_comm_mat <- matrix(0, nrow=n, ncol=n)
    
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        if (community[i] == community[j]) {
          idealized_comm_mat[i,j] = 1 # idealized community matrix -> element_ij = 1 if i is in the same community with j
        } else {
          idealized_comm_mat[i,j] = 0
        }
      }
    }
    
    if (num_comms > 1 & num_comms < n) {
      ideal_observed_cors <- append(ideal_observed_cors, 
                                    cor(as.vector(idealized_comm_mat), as.vector(as.matrix(AM)))
      )
      ideal_observed_modul <- append(ideal_observed_modul, 
                                     modularity(g, community))
      
    } else {
      ideal_observed_cors <- append(ideal_observed_cors, 0)
      ideal_observed_modul <- append(ideal_observed_modul, 
                                     modularity(g, community))
    }
    cat('number of communities: ', num_comms, '\t', 'correlation: ', round(ideal_observed_cors[h],3),'\t', 'modularity: ', round(ideal_observed_modul [h],3),'\n')
  }
  par(mfrow=c(1,3))
  plot(ideal_observed_modul, type = 'o')
  plot(ideal_observed_cors, type = 'o')
  
  cs=community.structure
  my_best_comm = which.max(ideal_observed_cors)
  community = cutat(cs, no =my_best_comm)
  cs.membership = community
  plot(cs,g)
  
  return(c(cs,which.max(ideal_observed_modul), which.max(ideal_observed_cors)))
}

# [1] "WALKTRAP METHOD"
best_WALKTRAP <- n.cluster(g_data,1)
best_WALKTRAP_group <- cbind(industry_ids,best_WALKTRAP$membership)
write.csv(best_WALKTRAP_group, "best_WALKTRAP_group_405.csv",row.names=FALSE)

# [1] "MODULARITY OPTIMIZATION METHOD"
best_MODULARITY <- n.cluster(g_data,2)
best_MODULARITY_group <- cbind(industry_ids,best_MODULARITY$membership)
write.csv(best_MODULARITY_group, "best_MODULARITY_group_405.csv",row.names=FALSE)

# [1] "EDGE BETWEENNESS METHOD"
best_EDGE_BTW <- n.cluster(g_data,3)
best_EDGE_BTW$membership
best_EDGE_BTW_group <- cbind(industry_ids,best_EDGE_BTW$membership)
write.csv(best_EDGE_BTW_group, "best_EDGE_BTW_group_405.csv",row.names=FALSE)

#Performance Evaluation between communities

#Walktrap and modularity optimisation
performance = compare(best_WALKTRAP$membership,best_MODULARITY$membership,method = "rand")
performance_vi = compare(best_WALKTRAP$membership,best_MODULARITY$membership,method = "vi")
performance_adjrand = compare(best_WALKTRAP$membership,best_MODULARITY$membership,method = "adjusted.rand")

#Modularity optimisation and Edge Betweeness
performance1 = compare(best_EDGE_BTW$membership,best_MODULARITY$membership,method = "rand")
performance_vi1 = compare(best_EDGE_BTW$membership,best_MODULARITY$membership,method = "vi")
performance_adjrand1 = compare(best_EDGE_BTW$membership,best_MODULARITY$membership,method = "adjusted.rand")


#walktrap and Edge Betweeness
performance2 = compare(best_WALKTRAP$membership,best_EDGE_BTW$membership,method = "rand")
performance_vi2 = compare(best_WALKTRAP$membership,best_EDGE_BTW$membership,method = "vi")
performance_adjrand2 = compare(best_WALKTRAP$membership,best_EDGE_BTW$membership,method = "adjusted.rand")















