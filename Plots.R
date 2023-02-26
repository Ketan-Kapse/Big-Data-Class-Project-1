library(igraph)

df = read.table("soc-Epinions1_adj.tsv")
df
head(df)
tail(df)
summary(df)

# dropping the edges
df = df[, -3]
tail(df)


epinions <- graph_from_data_frame(df)

epinions_density <- graph.density(epinions, loops = TRUE)
epinions_density

# centrality measures

degrees <- degree(epinions)
min(degrees) ### 2
max(degrees) ### 6088
mean(degrees)### 21.38879
median(degrees) ### 4
top_nodes <- names(sort(degrees, decreasing = TRUE)[1:10])
top_nodes
top_central_degree <- degree(epinions, 8886)
top_central_degree # 72

# in-degree
degree_in <- degree(epinions, mode = "in")
min(degree_in)   ### 1
max(degree_in)   ### 3044
mean(degree_in)  ### 10.6944
median(degree_in)### 2
top_nodes <- names(sort(degree_in, decreasing = TRUE)[1:10])
top_nodes
top_central_degree <- degree(epinions, 8886, mode = "in")
top_central_degree  # 36

# out-degree
degree_out <- degree(epinions, mode = "out")
min(degree_out)   ### 1
max(degree_out)   ### 3044
mean(degree_out)  ### 10.6944
median(degree_out)### 2
top_nodes <- names(sort(degree_out, decreasing = TRUE)[1:10])
top_nodes
top_central_degree <- degree(epinions, 8886, mode = "out")
top_central_degree # 36


node <- which.max(degree(epinions))
print(node)

degrees <- degree(epinions, 8886, mode = "all")
degrees
