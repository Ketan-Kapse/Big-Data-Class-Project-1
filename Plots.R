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


clu <- clusters(epinions, mode = "weak")
max(clu$csize)    #75877
min(clu$csize)    #2
clu$no            #2

# so, performing weak clustering on the data shows us that 
# only two clusters are formed out of which one cluster only 
# has 2 members and the rest of the nodes are in the other one

which(clu$csize == max(clu$csize))
# cluster 1 is the largest cluster

which(clu$membership==2)
# nodes 72044 and 68604 belong to the smallest cluster


cl_str = clusters(epinions,mode = "strong")
cl_str$no     #2
max(cl_str$csize) #75877
min(cl_str$csize) #2
which(cl_str$csize == max(cl_str$csize))  #2
which(cl_str$membership==1)

### both strong and weak clustering result in the same
#  clusters being formed. This means that the network has
# a clear and distinct structure.




# calculating the betweenness centrality of the largest cluster

betweennes_CL_STR <- betweenness(epinions, v=cl_str$membership==2)
min(betweennes_CL_STR)    #0
max(betweennes_CL_STR)    # 415836125
mean(betweennes_CL_STR)   # 250987.2
median(betweennes_CL_STR) # 0

hist(betweennes_CL_STR, breaks = 2)


cl <- walktrap.community(epinions, steps = 2)

E(epinions)$weight <- 1
V(epinions)$weight <- 1
mygraph_simple <- contract.vertices(epinions, cl$membership, vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))



# Simplify edges
mygraph_simple <- simplify(mygraph_simple, edge.attr.comb = list(weight = "sum", function(x)length(x)))

mygraph_Simplified <- induced.subgraph(mygraph_simple, V(mygraph_simple)$weight > 12)
V(mygraph_Simplified)$degree <- unname(degree(mygraph_Simplified))
plot(mygraph_Simplified, layout = layout.circle, vertex.label.cex = 0.5)

V(mygraph_Simplified)                                       
