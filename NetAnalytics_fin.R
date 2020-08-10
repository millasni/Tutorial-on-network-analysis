# # 5. Data analysis example

#####################################################################
# ## 5.1 Reading in and preprocessing data
#####################################################################

# Please make sure to uncomment and install all packages you don't have yet

# install.packages("igraph")
# install.packages("ggplot2")
# install.packages("RColorBrewer")
# install.packages("ggraph")
# install.packages("tidygraph")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("visNetwork")
# install.packages("png") 
# install.packages("grid")

library(igraph)
library(ggplot2)
library(RColorBrewer)
library(ggraph)
library(visNetwork)
library(tidygraph)
library(tidyverse)
library(dplyr)
library(corrplot)

# Next, we read in the data and quickly explore it. 
book1 = read.csv("book1.csv")
nrow(book1) # show the number of rows in the dataset
summary(book1) 
head(book1,3) # show top-3 lines from the dataset

# remove columns "Type" and "book" since they are of no use to us. 
# replace hyphens in the character names with spaces, using the `gsub()` command for it. 
book1 = book1[,c(1:2,4)]
book1$Source = gsub("-"," ", book1$Source)
book1$Target = gsub("-"," ", book1$Target)
head(book1,3)

#####################################################################
# ## 5.2 Creating a network and exploring its basic statistics
#####################################################################

GoT = graph_from_data_frame(d = book1, directed = FALSE)
summary(GoT)

# Functions `V(graph)` and `E(graph)` extract vertices and edges from an `igraph` network 
head(V(GoT)) 
head(E(GoT),3)

# a way to get size of a network is to count node and tie numbers with the following functions:
vcount(GoT) # get the number of nodes
ecount(GoT) # get the number of ties

# explore network density
edge_density(GoT)

# transitivity measures the proportion of closed triangles among existing closed and open ones. 
# It takes values from 0 to 1. 
# The `igraph` package allows calculation of two types of transitivity: 
# a *global* one, for the whole network, and a *local* one, for each node. 
transitivity(GoT, type = "global") # removing type = "global" yields the same result
head(transitivity(GoT, type = "local")) # transitivity for the first 6 people 
head(count_triangles(GoT)) # number of triangles for the first 6 people

# Network diameter
# # If a network is weighted, by default, the call of `diameter()` will account for weights, 
# but the result will be hard to interpret. 
# We specify `weights = NA` in it to get a more intuitive result
diameter(GoT, weights = NA) # disable weights
get_diameter(GoT, weights = NA) # explore the longest path between two nodes
farthest_vertices(GoT, weights = NA)


# Calculate **degree** of each node, i.e. the number of nodes it is connected to
deg = degree(GoT) # produces a named vector of degrees
head(sort(deg, decreasing = TRUE), 10) # top-10 characters by degree

# save *degree* as a vertex attribute for later use
V(GoT)$degree = degree(GoT)
V(GoT)[head(order(V(GoT)$degree, decreasing = TRUE),10)] # top-10 characters by degree

# see what pairs of characters have the strongest connections 
# (the largest weights associated with their ties). 
E(GoT)[head(order(E(GoT)$weight, decreasing = TRUE),5)] # top-5 pairs of characters

# Explore how a number of connections per character and their strengths are distributed. According to @gilles, in most complex networks, a few nodes will have a high number of connections while the majority of nodes - a small number. 
par(mfrow=c(1,2))
plot(table(V(GoT)$degree), type="l", main = "Distribution of node degrees",
     cex.main = 0.9, xlab = "Node degree (number of connections)", 
     ylab = "Number of nodes", cex.lab = 0.75, cex.axis = 0.75)
plot(table(E(GoT)$weight), type="l", main = "Distribution of edge weights",
     cex.main = 0.9, xlab = "Edge weight (strength of a connection)", 
     ylab = "Number of edges", cex.lab = 0.75, cex.axis = 0.75)
par(mfrow=c(1,1))

 
# Have a glimpse into our adjacency matrix showing weights of our edges 
GoT[21:25,21:25]

#####################################################################
# ## 5.3 Plotting a network
#####################################################################

# ### 5.3.1 Static plots
# For explanations how to produce this plot, please see the report.
# Due to technical reasons, we use 'theme_graph()' in static plots in this code
# but use 'theme_void' in Rmd code. Please see report for details.
set.seed(55555) # For reproducibility
ggraph(GoT, layout = "with_dh") +
  geom_edge_link(aes(alpha = weight, edge_colour = 1), show.legend = FALSE) + 
  geom_node_point(aes(alpha = degree, size = degree, colour = "red"), 
                  show.legend = FALSE) +
  geom_node_text(aes(label = name, filter = degree>=15, size=degree/3),
                 repel=TRUE, show.legend = FALSE) +
  labs(title = "GoT network plot (1): using node degree and tie weight", 
       subtitle = "More connected characters and stronger connections are shown bigger/darker") +
  theme_graph() # this is replaced by "theme_void" in Rmd file. 

# Get a list of possible layouts for a network plot
ggraph:::igraphlayouts
# It is possible to read about each layout, for example,with `?layout_with_kk` in the console. 

# explore node and edge attributes in the network
vertex_attr_names(GoT)
edge_attr_names(GoT)

# ### 5.3.2 Interactive plots
# 
# Using `visNetwork` package to build an interactive plot: 

#  * Option 1: use its core function `visNetwork()`. 
#    It takes as arguments data frames with nodes and edges. 
#  * Option 2: use `visIgraph()` command passing it the `igraph` object. 
#    The function will do the conversion behind the scenes and make a plot.

# Option 1 and Option 2 will produce identical interactive plots.
# Feel free to zoom in/out, click on nodes, drag them around, use a filter. 
# Option 1
g1 = toVisNetworkData(GoT)
visNetwork(nodes = g1$nodes, edges = g1$edges) %>%
  visIgraphLayout(layout = "layout_with_kk") %>% # Select Kamada-Kawai layout
  # Create a filter to select nodes by character names 
  # and highlight neighbours of the selected node
  visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE)

# Option 2
visIgraph(GoT) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE)

#####################################################################
# ## 5.4 Network centrality measures
#####################################################################

# Finding the most prominent/influential nodes. 
# **Strength** of a node in a weighted network is the sum of weights
# on all edges adjacent to this node 
str = strength(GoT)
head(sort(str, decreasing = TRUE), 10) # top-10 characters by strength
V(GoT)$strength = strength(GoT) # save as a node attribute

# **Closeness** centrality measures how many steps, on average, 
# separate one node from any other node. 
cls = closeness(GoT)
head(sort(cls, decreasing = TRUE), 10) # top-10 characters by centrality
V(GoT)$closeness = closeness(GoT) # save as a node attribute


# **Betweenness** of a node shows how often it lies on the shortest 
# paths between other pairs of nodes.  
btw = betweenness(GoT)
head(sort(btw, decreasing = TRUE), 10) # top-10 characters by betweenness
V(GoT)$betweenness = betweenness(GoT) # save as a node attribute

# **Eigen-centrality** measures the importance of a node by taking 
# into account the importance of its neighbours.
# the output of eigen_centrality is a list, need to add $vector
eig = eigen_centrality(GoT)$vector 
head(sort(eig, decreasing = TRUE), 10) # top-10 characters by eigen-centrality
V(GoT)$eigen = eigen_centrality(GoT)$vector

# Combining all the centrality measures in a data frame 
# and exploring correlations between them. 
options(digits = 3) # decrease the number of digits in the output
centrality = as.data.frame(cbind(deg, cls, btw, str, eig))
cor(centrality)

# Calculate **betweenness of ties**. 
# Using `edge_betweenness()` is a bit tricky. 
# If edges have weights, the function will use them by default, 
# however, it will treat them as **distances** though they represent 
# the **strength of a connection**. To adjust for this, we need to 
# specify `weights` argument and pass to it a reciprocal of edge 
# `weight` attribute. Otherwise, we will get strange results.
E(GoT)$btw.e.w = edge_betweenness(GoT, weights = 1/E(GoT)$weight) # weighted
E(GoT)[head(order(E(GoT)$btw.e.w, decreasing = TRUE),10)] # top-10 connections
head(sort(E(GoT)$btw.e.w, decreasing = TRUE),10) # their tie betweenness values

# We can also calculate non-weighted edge betweenness
# and its correlation with weighted betweenness
E(GoT)$btw.e.nw = edge_betweenness(GoT, weights = NA) # use NA to allow non-weighted
cor(E(GoT)$btw.e.w, E(GoT)$btw.e.nw)

# We plot our network once more, highlighting nodes and edges with high betweenness.
set.seed(55555)
ggraph(GoT, layout = "with_dh") +
  geom_edge_link(aes(alpha = btw.e.w, edge_colour = 1), show.legend = FALSE) + 
  geom_node_point(aes(alpha = betweenness, size=betweenness, colour = "red"),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name, filter = betweenness>=300,size=betweenness/3),
                 repel=TRUE, show.legend = FALSE) +
  labs(title = "GoT network plot (2): using node and edge betweenness", 
       subtitle = "Characters and ties with higher betweenness are shown darker") +
  theme_graph() # this is replaced by "theme_void" in Rmd file.

#####################################################################
# ## 5.5 Community detection
#####################################################################

# We will apply all algorithms for comm.detection from  igraph package 
# except `cluster_optimal()` to our data set. This algorithm represents 
# an NP-complete problem and runs in exponential time. 

ca.eb = cluster_edge_betweenness(GoT, weights = 1 / E(GoT)$weight); sizes(ca.eb)

ca.fg = cluster_fast_greedy(GoT); sizes(ca.fg)

ca.lp = cluster_label_prop(GoT); sizes(ca.lp)

ca.le <- cluster_leading_eigen(GoT); sizes(ca.le)

ca.lou <- cluster_louvain(GoT); sizes(ca.lou)

ca.wt <- cluster_walktrap(GoT); sizes(ca.wt)

ca.sg <- cluster_spinglass(GoT); sizes(ca.sg)

ca.info <- cluster_infomap(GoT); sizes(ca.info)

# The following code chunk will calculate `length` and `modularity` 
# for all tried algorithms and join results in a data frame. 
communities = list(ca.lou, ca.fg, ca.sg, ca.wt, ca.info, ca.eb, ca.lp, ca.le)
alg = c("Louvain","Fast-greedy","Spinglass","Walktrap","InfoMAP","Edge betw.",
"Label.prop.","Lead.eigen.") 
len = c(); modul = c()
# loop over communities to calculate their length and modularity
for (i in communities) {
  len = c(len, length(i)) # calculate the number of communities
  modul = c(modul, round(modularity(i),3)) # calculate modularity
}
# create a neat data frame to compare different algorithms
as.data.frame(cbind(alg,len,modul)) %>%
  arrange(desc(modul)) # sort by modularity in descending order

# Compare how similar communities are using Rand index 
RI = matrix(nrow=8,ncol=8) # initialize a matrix to store the Rand index
for (i in 1:8) {
  for (j in 1:8) {
  RI[i,j] = compare(communities[[i]], communities[[j]], method = "adjusted.rand")
  }
}
rownames(RI) = alg; colnames(RI) = alg; diag(RI) = 0 # set row/col names and diagonal to 0
corrplot(RI, method = "number", is.corr = FALSE, tl.col = "tomato2", tl.cex = 0.8,
         number.cex = 0.8)

# save and visualize communities detected by *Louvain* algorithm. 
# This solution has the highest modularity, produces a reasonable 
# number of communities and is also similar to several other solutions. 
# We will plot communities and edges between them with different colors. 

# TO COLOR EDGES WITH CLUSTER COLORS
# create node data frame with numeric node IDs useful for matching nodes and edges
from = book1 %>% distinct(Source)
to = book1 %>% distinct(Target)
nodes = full_join(from, to, by=c("Source"="Target"))
names(nodes) = "label" # rename the column
nodes = rowid_to_column(nodes, var = "id") # create a column with numeric node IDs
# create a data frame with edges
edges = book1 %>%
  left_join(nodes, by = c("Source" = "label")) %>%  # join "id" for "Source"
  rename(from = id) %>%  # rename "id" that we joined into "from"
  left_join(nodes, by = c("Target" = "label")) %>%  # join "id" for "Target"
  rename(to = id) %>% # rename "id" that we joined into "to" 
  select(from, to, weight)
V(GoT)$community = membership(ca.lou) # save louvain community membership 
# create edge color attribute: edges within the same community colored as nodes,
# edges between communities having color 9999
# this function requires numeric IDs, does not work with character names
E(GoT)$color <- apply(edges, 1, 
                function(x) ifelse(V(GoT)$community[x[1]] == V(GoT)$community[x[2]],
                                   V(GoT)$community[x[1]], 9999))

# Make a plot
V(GoT)$community = membership(ca.lou)
set.seed(55555)
ggraph(GoT, layout = "with_dh") +
  # we use colour, fill and scale_fill_identity() to color communities in different colors
  # we don't show links between communities with filter=color!=9999
  geom_edge_link(aes(alpha = btw.e.w, color = factor(color), filter=color!=9999),
                 show.legend = FALSE) +
  geom_node_point(aes(alpha = 0.8, size=betweenness, colour = factor(community), 
                      fill = factor(community)), show.legend = FALSE) +
  scale_fill_identity() + 
  geom_node_text(aes(label = name, filter = betweenness>=500, size=betweenness/3),
                 repel=TRUE, show.legend = FALSE) +
  labs(title = "Eight GoT communities found by louvain algorithm", 
       subtitle = "Characters and ties with higher betweenness are shown bigger/darker;
       only ties within communities are shown") +
  theme_graph() # this is replaced by "theme_void" in Rmd file.

# make an interactive plot with communities detected.
V(GoT)$color = V(GoT)$community # create color attribute equal to a cluster number
set.seed(333)
visIgraph(GoT) %>%
  visIgraphLayout(layout = "layout_with_dh") %>%
  # selectedBy = "group" will create a filter with cluster numbers
  visOptions(highlightNearest = TRUE, selectedBy = "group")

