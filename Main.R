library(igraph)

df = read.table("soc-Epinions1_adj.tsv")
head(df)
tail(df)
summary(df)

# since all the edges are 1, we will drop the V3 column

df = df[, -3]

# loading the dataFrame into a graph object and plotting it

epinions <- graph_from_data_frame(df)
str(epinions)
plot(epinions)

epinions.adj <- igraph::get.adjacency(epinions)


### Centrality measures

## Degree Centrality

# print the top nodes with the highest degrees in the data
degree_centrality <- degree(epinions, mode = "in")
top_nodes <- names(sort(degree_centrality, decreasing = TRUE)[1:10])
top_nodes

## Betweenness Centrality

betweennes_CL_STR <- betweenness(epinions.adj)
min(betweennes_CL_STR)    #0
max(betweennes_CL_STR)    # 415836125
mean(betweennes_CL_STR)   # 250987.2
median(betweennes_CL_STR) # 0


## Closeness Centrality

closeness_centrality <- centr_clo(epinions)
closeness_centrality

### Largest Clique Size

largest_clique <- clique_num(epinions)
largest_clique

### Power centrality

power_c <- power_centrality(epinions)

