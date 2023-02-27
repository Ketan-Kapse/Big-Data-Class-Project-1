library(igraph)

df = read.table("soc-Epinions1_adj.tsv")
head(df)
tail(df)
summary(df)

# since all the edges are 1, we will drop the V3 column

df = df[, -3]

### loading the dataFrame into a graph object and plotting it

epinions <- graph_from_data_frame(df)
str(epinions)
epinions
# plot pre-simplification
plot(epinions, vertex.label.cex = 0.5)

epinions.adj <- igraph::get.adjacency(epinions)
epinions.adj


### Calculating the degrees pre-simplification

degrees <- degree(epinions)
mean(degrees)    #21.38879
median(degrees) # 4
max(degrees)    # 6088
min(degrees)    # 2

num_node <- sum(degrees)
num_node



# degree distribution

deg_dist <- degree.distribution(epinions)
plot(deg_dist, type = "h", lwd = 3, xlab = "Degree", ylab = "Frequency", main = "Degree distribution")


### Simplifying the graph

wf <- walktrap.community(epinions, steps = 5)
E(epinions)$weight <- 1
V(epinions)$weight <- 1
epinions_simple <- contract.vertices(epinions, wf$membership, 
                  vertex.attr.comb = list(weight = "sum", 
                  name = function(x)x[1], "ignore"))

epinions_simple <- simplify(epinions_simple,
                            edge.attr.comb = list(weight = "sum", function(x)length(x)))

epinions_reduced <- induced.subgraph(epinions_simple, V(epinions_simple)$weight > 25)
V(epinions_reduced)$degree <- unname(degree(epinions_reduced))
epinions_reduced

plot(epinions_reduced, layout = layout.fruchterman.reingold, vertex.label.cex = 0.4, vertex.size = 6)
plot(epinions_reduced, layout = layout.davidson.harel, vertex.label.cex = 0.4)
plot(epinions_reduced, layout = layout.circle, vertex.label.cex = 0.4, vertex.size = 11)



### Centrality measures after simplifying

## Degree Centrality

# print the top nodes with the highest degrees in the data
degree_centrality <- degree(epinions_reduced, mode = "in")
top_nodes <- names(sort(degree_centrality, decreasing = TRUE)[1:10])
top_nodes

## Betweenness Centrality

betweennes_centrality <- betweenness(epinions_reduced)
min(betweennes_centrality)    #0
max(betweennes_centrality)    # 3136.794
mean(betweennes_centrality)   # 253.6977
median(betweennes_centrality) # 103.9995
betweennes_centrality


## Closeness Centrality

closeness_centrality <- centr_clo(epinions_reduced)
closeness_centrality

### Largest Clique Size

largest_clique <- clique_num(epinions_reduced)
largest_clique

largest_clique <- largest_cliques(epinions_reduced)
clique_plot <- induced_subgraph(epinions_reduced, largest_clique[[1]])
plot(clique_plot, vertex.label.cex = 0.5, layout = layout_as_star)


### Longest Path
# Get the vertices in the longest path
path <- get.diameter(epinions_reduced, directed = FALSE, weights = NULL)
path

path <- induced_subgraph(epinions_reduced, get_diameter(epinions_reduced))
plot(path, vertex.label.cex = 0.5, vertex.size = 25)

### Power centrality

pc <- power_centrality(epinions_reduced)
pc

### EGO
ego.graph <- igraph::ego(epinions_reduced)
ego.graph


### center between

epinions_reduced.between <- igraph::centr_betw(epinions_reduced)
epinions_reduced.between

### Shortest paths
epinions_reduced.shortestPaths <- igraph::shortest.paths(epinions_reduced)
epinions_reduced.shortestPaths


### Geodist 

epinions_reduced.edgeList <- igraph::get.edgelist(epinions_reduced)
geoDist <- geodist(epinions_reduced.edgeList)
geoDist


### Degree histogram

hist(igraph::degree(epinions_reduced), breaks = 150)

### Edge density

edge_dense <- edge_density(epinions_reduced, loops = TRUE)
edge_dense
nodes <- vcount(epinions_reduced)
edges <- ecount(epinions_reduced)
node_density <- 2*edges/(nodes*(nodes-1))
node_density


### Central Node
V(epinions_reduced)$central_degree <- centr_degree(epinions_reduced)$res
V(epinions_reduced)$name[V(epinions_reduced)$central_degree==max(centr_degree(epinions_reduced)$res)]
