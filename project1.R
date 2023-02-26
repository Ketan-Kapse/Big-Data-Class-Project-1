library(igraph)

df = read.table("soc-Epinions1_adj.tsv")
df
head(df)
tail(df)
summary(df)
# colSums(is.na(df))
# there are no na values in the dataset

# since the column V3 has all edges 1, we remove it.
df = df[, -3]
tail(df)

optab <-as.matrix(df)
optab

v1 <- optab[1:811480, 1]
v2 <- optab[1:811480, 2]

relations <- data.frame(from=v1,to=v2)
g <- graph.data.frame(relations, directed = TRUE)
plot(g)
# as we can see from the generated plot, it is basically unreadable
# and we require simplification for better analysis

# figuring out what node and edge densities tell us about this problem

epinions <- graph_from_data_frame(df)

node = edge_density(epinions)
x <- simplify(g)
plot(x)


# centrality measures:

# a) degree centrality

degree_centrality <- degree(epinions, mode = "in")
top_nodes <- names(sort(degree_centrality, decreasing = TRUE)[1:10])
top_nodes
top_central_degree <- degree(epinions, 8886)
top_central_degree
top_central_degree <- degree(epinions, 8886, mode = "in")
top_central_degree


# calculating the betweenness centrality

betweenness_centrality <- betweenness(epinions, directed = TRUE)
top10 <- names(sort(betweenness_centrality, decreasing = TRUE)[1:10])
top10
nodes <- c(8886,73248, 4778,60550,59328,37775,68216,73548,47021,70771)
bc_nodes <- betweenness_centrality[as.character(nodes)]
bc_nodes


# calculating the eigenvector centrality

eigenv_centrality <- eigen_centrality(epinions, directed = TRUE)$vector
top10 <- names(sort(eigenv_centrality, decreasing = TRUE)[1:10])
top10
nodes <- c(8886,60550,59328,37775,18886,33443,4778,70771,26664,1989)
ev_nodes <- eigenv_centrality[as.character(nodes)]
ev_nodes







# some simplification

# checking if there are any isolated nodes present
degrees <- degree(epinions)
isolated_nodes <- sum(degrees >= 12)
isolated_nodes # 0 No isolated nodes present!

#finding the mean of the degrees
mean_degree <- mean(degrees)
mean_degree
max_degree <- max(degrees)
max_degree
min_degree <- min(degrees)
min_degree

# finding the distribution of the degrees for nodes

distribution <- degree_distribution(epinions)
plot(distribution, main = "Distribution", xlab = "Degree", ylab = "Frequency", log ="xy")

hist(igraph::degree(epinions))

epinions_reduced <- delete.vertices(epinions, V(epinions)[degree(epinions) < 8])
isolated_nodes <- which(degree(epinions_reduced) == 0)
epinions_reduced <- delete.vertices(epinions_reduced, isolated_nodes)
epinions_simplified <- simplify(epinions_reduced)

wf <- walktrap.community(epinions_simplified, steps = 5)
plot(wf,epinions_simplified, vertex.size=0.5, layout=layout.fruchterman.reingold, vertex.label.cex = 0.5)


cluster <- clusters(epinions_simplified)
cluster

subgraphs <- decompose(epinions_simplified)

keep_graphs <- subgraphs[cluster$membership == 1]


