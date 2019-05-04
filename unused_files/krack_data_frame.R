#==============================================================================
###############################################################################
#
# Course in Social Network Analysis                  
#                           
###############################################################################
#==============================================================================



#==============================================================================
###############################################################################
#
# LAB 2: DESCRIPTIVE STATISTICS
#                                   
###############################################################################
#==============================================================================

###############################################################################
# 0. LOADING LIBRARIES
###############################################################################

install.packages("NetData")
install.packages("sna")
install.packages("animation", repos = "http://cran.cnr.berkeley.edu/", dependencies = TRUE)

library(igraph)
library(NetData)
library('cluster')
library('sna')



###############################################################################
# 1. LOADING DATA
###############################################################################
#
# Data Background: These are data collected from the managers of a high-tec 
# company. The company manufactured high-tech equipment on the west coast of 
# the United States and had just over 100 employees with 21 managers. Each 
# manager was asked to whom do you go to for advice and who is your friend, to 
# whom do you report was taken from company documents. In addition attribute 
# information was collected. This consisted of the managers age (in years), 
# length of service or tenure (in years), level in the corporate hierarchy 
# (coded 1,2 and 3; 1=CEO, 2 = Vice President, 3 = manager) and department 
# (coded 1,2,3,4 with the CEO in department 0 ie not in a department). This 
# data is used by Wasserman and Faust in their network analysis book.
#
###############################################################################

setwd("C:/Users/eviriyakovithya/Documents/Social_Network_Analysis/General Network Data Sets-20190425")

advice_data_frame <- read.table('Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('Krack-High-Tec-edgelist-ReportsTo.txt')

attributes <- read.csv('Krack-High-Tec-Attributes.csv', header=T)

colnames(advice_data_frame) <- c('ego', 'alter', 'advice_tie')
head(advice_data_frame)

colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')
head(friendship_data_frame)

colnames(reports_to_data_frame) <- c('ego', 'alter', 'reports_to_tie')
head(reports_to_data_frame)

#------------------------------------------------------------------------------
# combine thr data sets into a single data frame.
#------------------------------------------------------------------------------

krack_full_data_frame <- cbind(advice_data_frame, 
                               friendship_data_frame$friendship_tie, 
                               reports_to_data_frame$reports_to_tie)

#------------------------------------------------------------------------------
# Change the names of each column
#------------------------------------------------------------------------------

names(krack_full_data_frame)[4:5] <- c("friendship_tie", "reports_to_tie")  
head(krack_full_data_frame)

#------------------------------------------------------------------------------
# Reduce to non-zero edges so that the edge list only contains
# actual ties of some type.
#------------------------------------------------------------------------------

krack_full_nonzero_edges <- subset(krack_full_data_frame, 
                                   (advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
head(krack_full_nonzero_edges)

#------------------------------------------------------------------------------
# Import the data set into a "graph" object 
#------------------------------------------------------------------------------

krack_full <- graph.data.frame(krack_full_nonzero_edges) 
summary(krack_full)

#------------------------------------------------------------------------------

# Create sub-graphs based on edge attributes
krack_advice <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "advice_tie")==0])
summary(krack_advice)

krack_friendship <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "friendship_tie")==0])
summary(krack_friendship)

krack_reports_to <- igraph::delete.edges(krack_full, E(krack_full)[igraph::get.edge.attribute(krack_full,name = "reports_to_tie")==0])
summary(krack_reports_to)

#------------------------------------------------------------------------------
# Add vertex attribute to a graph object
#------------------------------------------------------------------------------

attributes <- cbind(1:length(attributes[,1]), attributes)
krack_full <- graph.data.frame(d = krack_full_nonzero_edges, vertices = attributes) 

summary(krack_full)

n <- gorder(krack_full)




###############################################################################
#
# 3. RECIPROCITY TRANSITIVITY MEASURES
#
###############################################################################

# Reciprocity
reciprocity(krack_full)
reciprocity(krack_advice)
reciprocity(krack_friendship)
reciprocity(krack_reports_to)

# Transitivity
transitivity(krack_full)
transitivity(krack_advice)
transitivity(krack_friendship)
transitivity(krack_reports_to)

#------------------------------------------------------------------------------
# Triad census. Here we'll first build a vector of labels for the different 
# triad types. Then we'll combine this vector with the triad censuses for the 
# different networks, which we'll export as a CSV.
#------------------------------------------------------------------------------

census_labels = c('003',
                  '012',
                  '102',
                  '021D',
                  '021U',
                  '021C',
                  '111D',
                  '111U',
                  '030T',
                  '030C',
                  '201',
                  '120D',
                  '120U',
                  '120C',
                  '210',
                  '300')

tc_full <- igraph::triad.census(krack_full)
tc_advice <- igraph::triad.census(krack_advice)
tc_friendship <- igraph::triad.census(krack_friendship)
tc_reports_to <- igraph::triad.census(krack_reports_to)

triad_df <- data.frame(census_labels,
                       tc_full, 
                       tc_advice, 
                       tc_friendship,
                       tc_reports_to)
triad_df



#------------------------------------------------------------------------------
# Transitivity 
# (global: ratio of the triangles and the connected triples in the graph)
#------------------------------------------------------------------------------

tr_full <- transitivity(krack_full, type = "global")
tr_advice <- transitivity(krack_advice, type = "global")
tr_friendship <- transitivity(krack_friendship, type = "global")
tr_reports_to <- transitivity(krack_reports_to, type = "global")

transit_df <- data.frame(tr_full, 
                         tr_advice, 
                         tr_friendship,
                         tr_reports_to)
transit_df


#------------------------------------------------------------------------------
# Transitivity 
# (local: ratio of the triangles connected to a vertex and the triples 
# centered on the same vertex)
#------------------------------------------------------------------------------

tr_full <- transitivity(krack_full, type = "local")
tr_advice <- transitivity(krack_advice, type = "local")
tr_friendship <- transitivity(krack_friendship, type = "local")
tr_reports_to <- transitivity(krack_reports_to, type = "local")

transit_df <- data.frame(tr_full, 
                         tr_advice, 
                         tr_friendship,
                         tr_reports_to)
transit_df

apply(transit_df, 2, mean)





###############################################################################
#
# 4. COMMUNITY STRUCTURE
#
###############################################################################
#
# There are many different ways to detect communities. In this 
# lab, we'll use three: 
#
# - WALKTRAP
# - GREEDY MODULARITY OPTIMIZATION
# - EDGE BETWEENNESS METHOD
# - K-CORE
#
# We can visualize the clusters generated by these algorithms as a dendrogram 
# Here, the y-axis reflects the distance metric used by the algorithm. 
#
###############################################################################
#
# Useful online material:
#
# - https://arxiv.org/pdf/cond-mat/0408187v2.pdf
# - https://arxiv.org/pdf/physics/0512106v1.pdf
# - https://arxiv.org/pdf/cond-mat/0308217v1.pdf
#
###############################################################################

#------------------------------------------------------------------------------
# Visualize the networks
#------------------------------------------------------------------------------

layout.0 <- layout.fruchterman.reingold(krack_full)
plot(krack_full, layout=layout.0, edge.arrow.size=.5)

layout.0 <- layout.fruchterman.reingold(krack_advice)
plot(krack_advice, layout=layout.0, edge.arrow.size=.5)

layout.0 <- layout.fruchterman.reingold(krack_friendship)
plot(krack_friendship, layout=layout.0, edge.arrow.size=.5)

layout.0 <- layout.fruchterman.reingold(krack_reports_to)
plot(krack_reports_to, layout=layout.0, edge.arrow.size=.5)


#------------------------------------------------------------------------------
# Preprocessing
#------------------------------------------------------------------------------
# We'll use the friend sub-graph as the basis for our community 
# detection methods. For clarity and simplicity, we'll set the
# network to undirected and remove isolated vertices. Comparing
# m182_friend before and after these operations, you'll notice
# that the number of edges decreases as reciprocated directed ties
# are consolidated into single undirected ties, and the number of
# vertices decreases as isolates are removed.
#------------------------------------------------------------------------------

krack_full_und <- as.undirected(krack_full, mode='collapse')
krack_full_no_iso <- igraph::delete.vertices(krack_full_und, V(krack_full_und)[igraph::degree(krack_full_und)==0])
summary(krack_full_und)
summary(krack_full_no_iso)

krack_advice_und <- as.undirected(krack_advice, mode='collapse')
krack_advice_no_iso <- igraph::delete.vertices(krack_advice_und, V(krack_advice_und)[igraph::degree(krack_advice_und)==0])
summary(krack_advice_und)
summary(krack_advice_no_iso)

krack_friendship_und <- as.undirected(krack_friendship, mode='collapse')
krack_friendship_no_iso <- igraph::delete.vertices(krack_friendship_und, V(krack_friendship_und)[igraph::degree(krack_friendship_und)==0])
summary(krack_friendship_und)
summary(krack_friendship_no_iso)

krack_reports_to_und <- as.undirected(krack_reports_to, mode='collapse')
krack_reports_to_no_iso <- igraph::delete.vertices(krack_reports_to_und, V(krack_reports_to_und)[igraph::degree(krack_reports_to_und)==0])
summary(krack_reports_to_und)
summary(krack_reports_to_no_iso)




#------------------------------------------------------------------------------
# WALKTRAP
#------------------------------------------------------------------------------
# The Walktrap algorithm detects communities through a series of short random
# walks. The idea is that the vertices encountered on any given random walk
# are more likely to be within a community. It initially treats all nodes as 
# separated communities, then iteratively merges them into larger communities. 
# In each step a new community is created from two other communities. The
# Walktrap algorithm calls upon the user to specify the length of random walks, 
# and Pons and Latapy (2005) recommend walks of 4 or 5 steps. 
#------------------------------------------------------------------------------

(krack_full_comm_wt <- walktrap.community(krack_full_no_iso, steps=200,modularity=TRUE))
plot(krack_full_comm_wt, krack_full_no_iso)
krack_full_comm_dend <- as.dendrogram(krack_full_comm_wt, use.modularity=TRUE)
plot(krack_full_comm_dend)
modularity(krack_full_comm_wt, krack_full_comm_wt$membership)

(krack_advice_comm_wt <- walktrap.community(krack_advice_no_iso, steps=200,modularity=TRUE))
plot(krack_advice_comm_wt, krack_advice_no_iso)
krack_advice_comm_dend <- as.dendrogram(krack_advice_comm_wt, use.modularity=TRUE)
plot(krack_advice_comm_dend)
modularity(krack_advice_comm_wt, krack_advice_comm_wt$membership)

(krack_friendship_comm_wt <- walktrap.community(krack_friendship_no_iso, steps=200,modularity=TRUE))
plot(krack_friendship_comm_wt, krack_friendship_no_iso)
krack_friendship_comm_dend <- as.dendrogram(krack_friendship_comm_wt, use.modularity=TRUE)
plot(krack_friendship_comm_dend)
modularity(krack_friendship_comm_wt, krack_friendship_comm_wt$membership)

(krack_reports_to_comm_wt <- walktrap.community(krack_reports_to_no_iso, steps=200,modularity=TRUE))
plot(krack_reports_to_comm_wt, krack_reports_to_no_iso)
krack_reports_to_comm_dend <- as.dendrogram(krack_reports_to_comm_wt, use.modularity=TRUE)
plot(krack_reports_to_comm_dend)
modularity(krack_reports_to_comm_wt, krack_reports_to_comm_wt$membership)



#------------------------------------------------------------------------------
# GREEDY MODULARITY OPTIMIZATION
#------------------------------------------------------------------------------
# The greedy monularity optimization algorithm detects communities by iteratively 
# including and removing nodes, in order to maximize the modularity.
#------------------------------------------------------------------------------

(krack_full_comm_fg <- fastgreedy.community(krack_full_no_iso, modularity=TRUE))
plot(krack_full_comm_fg, krack_full_no_iso)
krack_full_comm_dend <- as.dendrogram(krack_full_comm_fg, use.modularity=TRUE)
plot(krack_full_comm_dend)
modularity(krack_full_comm_fg, krack_full_comm_fg$membership)

(krack_advice_comm_fg <- fastgreedy.community(krack_advice_no_iso, modularity=TRUE))
plot(krack_advice_comm_fg, krack_advice_no_iso)
krack_advice_comm_dend <- as.dendrogram(krack_advice_comm_fg, use.modularity=TRUE)
plot(krack_advice_comm_dend)
modularity(krack_advice_comm_fg, krack_advice_comm_fg$membership)

(krack_friendship_comm_fg <- fastgreedy.community(krack_friendship_no_iso, modularity=TRUE))
plot(krack_friendship_comm_fg, krack_friendship_no_iso)
krack_friendship_comm_dend <- as.dendrogram(krack_friendship_comm_fg, use.modularity=TRUE)
plot(krack_friendship_comm_dend)
modularity(krack_friendship_comm_fg, krack_friendship_comm_fg$membership)

krack_reports_to_comm_fg <- fastgreedy.community(krack_reports_to_no_iso, modularity=TRUE)
plot(krack_reports_to_comm_fg, krack_reports_to_no_iso)
krack_reports_to_comm_dend <- as.dendrogram(krack_reports_to_comm_fg, use.modularity=TRUE)
plot(krack_reports_to_comm_dend)
modularity(krack_reports_to_comm_fg, krack_reports_to_comm_fg$membership)


#------------------------------------------------------------------------------
# EDGE BETWEENNESS METHOD
#------------------------------------------------------------------------------
# The edge-betweenness score of an edge measures the number of shortest paths 
# from one vertex to another that go through it. The idea of the edge-betweenness 
# based community structure detection is that it is likely that edges connecting 
# separate cluster have high edge-betweenness, as all the shortest paths from one 
# cluster to another must traverse through them. So if we iteratively remove the 
# edge with the highest edge-betweenness score we will get a hierarchical map of 
# the communities in the graph. The idea of the edge betweenness based community 
# structure detection is that is is likely that edges connecting separate modules 
# have high edge betweenness as all the shortest paths from one module to another 
# must traverse through them. So if we gradually remove the edge with the highest 
# edge betweenness score we will get a hierarchical map, a rooted tree, called a 
# dendrogram of the graph. The leafs of the tree are the individual vertices and 
# the root of the tree represents the whole graph.
# The Dendrogram we made seems to show values on the left for the average 
# number of inter-community edges per vertex. Since the algorithm takes out the 
# most between edges first, it forms cliques and leaves fewer and fewer inter-
# community edges in place. 
#------------------------------------------------------------------------------

(krack_full_comm_eb <- edge.betweenness.community(krack_full_no_iso, modularity=TRUE))
plot(krack_full_comm_eb, krack_full_no_iso)
krack_full_comm_dend <- as.dendrogram(krack_full_comm_eb, use.modularity=TRUE)
plot(krack_full_comm_dend)
modularity(krack_full_no_iso, krack_full_comm_eb$membership)

(krack_advice_comm_eb <- edge.betweenness.community(krack_advice_no_iso, modularity=TRUE))
plot(krack_advice_comm_eb, krack_advice_no_iso)
krack_advice_comm_dend <- as.dendrogram(krack_advice_comm_eb, use.modularity=TRUE)
plot(krack_advice_comm_dend)
modularity(krack_advice_no_iso, krack_advice_comm_eb$membership)

(krack_friendship_comm_eb <- edge.betweenness.community(krack_friendship_no_iso, modularity=TRUE))
plot(krack_friendship_comm_eb, krack_friendship_no_iso)
krack_friendship_comm_dend <- as.dendrogram(krack_friendship_comm_eb, use.modularity=TRUE)
plot(krack_friendship_comm_dend)
modularity(krack_friendship_no_iso, krack_friendship_comm_eb$membership)

(krack_reports_to_comm_eb <- edge.betweenness.community(krack_reports_to_no_iso, modularity=TRUE))
plot(krack_reports_to_comm_eb, krack_reports_to_no_iso)
krack_reports_to_comm_dend <- as.dendrogram(krack_reports_to_comm_eb, use.modularity=TRUE)
plot(krack_reports_to_comm_dend)
modularity(krack_reports_to_no_iso, krack_reports_to_comm_eb$membership)



#------------------------------------------------------------------------------
# Which is the optimal number of communities?
#------------------------------------------------------------------------------
# Now we'll decide on an optimum number of communities for this
# network. All of the community methods above allow us to iterate
# through different numbers of communities and judge how well
# correlated a given idealized community structure is with the
# observed network. 
#
# We'll start by getting an adjacency matrix based on the network 
# with isolates removed.
#
# Next, we'll derive a set of idealized community structures 
# at each possible number of communities. We can correlate each 
# of these structures with the observed community and store 
# the correlation statistics in a vector.
#
# This code loops through each possible number of communities that 
# can be derived using the edge-betweenness method, but it could 
# readily be applied to the other clustering methods above. For each 
# number of communities, it prints out an adjacency matrix with 0s
# indicating that the row and column vertices do not share a 
# community and 1s indicating that they do.
# 
# Thus, the first iteration returns 14 communities, one for each
# vertex, and an adjacency matrix of all 0s. The last iteration 
# returns a single community comprised of all vertices, and an 
# adjacency matrix of all 1s (except on the diagonal).
#
# Meanwhile, we can correlate each community structure with the 
# observed community to generate a list of ideal-observed
# correlations.
# We can then plot the list of correlations to determine the 
# optimum number of communities for this particular network.
#------------------------------------------------------------------------------

n.cluster <- function(g, type){
  
  AM <- get.adjacency(g)
  ideal_observed_cors = vector()
  ideal_observed_modul = vector()
  
  if(type == 1){
    print('WALKTRAP METHOD')
    community.structure <- walktrap.community(g, steps = 200)
  }else if(type == 2){
    print('MODULARITY OPTIMIZATION METHOD')
    community.structure <- fastgreedy.community(g)
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
          idealized_comm_mat[i,j] = 1
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
  par(mfrow=c(1,2))
  plot(ideal_observed_modul, type = 'o')
  plot(ideal_observed_cors, type = 'o')
  return(c(which.max(ideal_observed_modul), which.max(ideal_observed_cors)))
}

n.cluster(krack_full_no_iso,1)
n.cluster(krack_advice_no_iso,1)
n.cluster(krack_friendship_no_iso,1)
n.cluster(krack_reports_to_no_iso,1)

n.cluster(krack_full_no_iso,2)
n.cluster(krack_advice_no_iso,2)
n.cluster(krack_friendship_no_iso,2)
n.cluster(krack_reports_to_no_iso,2)

n.cluster(krack_full_no_iso,3)
n.cluster(krack_advice_no_iso,3)
n.cluster(krack_friendship_no_iso,3)
n.cluster(krack_reports_to_no_iso,3)




#------------------------------------------------------------------------------
# K-CORES
#------------------------------------------------------------------------------
# The k-core of graph is a maximal subgraph in which each vertex has at least degree k. 
# The coreness of a vertex is k if it belongs to the k-core but not to the (k+1)-core.
# The graph.coreness() function in igraph returns a vector containing 
# the degree of the highest-degree k-core to which each vertex 
# belongs. 
#------------------------------------------------------------------------------

coreness = graph.coreness(m182_task)
coreness

# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores. 
#
# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
  lay1 <- layout.fruchterman.reingold(g)
  plot(g, 
       vertex.color = graph.coreness(g), 
       layout=lay1, 
       edge.arrow.size = .5)
} 

make_k_core_plot(m182_friend)
make_k_core_plot(m182_social)
make_k_core_plot(m182_task)

# Here's an artificial example showing two separate 3-cores:
g1 <- graph.ring(10)
g1 <- add.edges(g1, c(0,2, 1,3, 0,3, 5,7, 5,8, 6,8))
make_k_core_plot(g1)

# Question #6 - What's the difference between K-cores and cliques? 
# Why might one want to use one over the other?



