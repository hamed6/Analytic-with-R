Hamed_Matrix <-matrix(
  c(0,1,1,0,0,0,1,0,0,0,0,0,
    1,0,0,1,0,0,0,1,0,0,0,0,
    1,0,0,0,1,0,0,0,0,0,1,0,
    0,1,0,0,0,1,0,1,0,0,0,1,
    0,0,1,0,0,1,1,0,0,0,0,0,
    0,0,0,1,1,0,0,1,0,0,0,0,
    1,0,0,0,1,0,0,0,1,0,0,0,
    0,1,0,0,0,1,0,0,0,1,0,0,
    0,0,0,0,0,0,1,0,0,1,0,1,
    0,0,0,0,0,0,0,1,1,0,1,0,
    0,0,1,0,0,0,0,0,0,1,0,1,
    0,0,0,1,0,0,0,0,1,0,1,0
  ), # the data elements
  nrow=12, 
  ncol=12, 
  byrow = TRUE) # fill matrix by rows

Hamed_graph <- graph.adjacency(Hamed_Matrix, mode="undirected")

tkplot(Hamed_graph, vertex.color = "red")

summary(Hamed_Matrix)
-----------------------------------------------------------------

graph_example_2 <−graph.famous("Meredith")
tkplot(graph_example_2, vertex.color = "yellow")


centrality_a <- degree(graph_example_2, loops = FALSE)
sort(centrality_a, decreasing = TRUE)


closeness_a <- closeness(graph_example_2)
sort(closeness_a, decreasing = TRUE)

betweenness_a <- betweenness(graph_example_2)
sort(betweenness_a, decreasing = TRUE)

pagerank_a <- page.rank(graph_example_2)[[1]]
sort(pagerank_a, decreasing = TRUE)

dfB <- data.frame(centrality_a,closeness_a,betweenness_a,pagerank_a)
cor(dfB)



pairs(~centrality_a + closeness_a + betweenness_a + pagerank_a , data = dfB, main = "Scatterplot Matrix")

-----------------------------------------------------------------

network_data <- read.csv("network_data.csv", sep = ";", header = TRUE)
graph_example_3 <- graph.adjacency(as.matrix(network_data),mode="undirected")


walktrap <- walktrap.community(graph_example_3)
membership(walktrap)
plot(walktrap,graph_example_3)
tkplot(graph_example_3, vertex.color=membership(walktrap))


edge_between <- edge.betweenness.community(graph_example_3)
plot(edge_between, graph_example_3)

greedy <- fastgreedy.community(graph_example_3)
plot(greedy, graph_example_3)

aggregate(sngame,by=list(sngame$gender),FUN=mean)
aggregate(sngame,by=list(sngame$age),FUN=mean)


-----------------------------------------------------------------
distance_mat <- dist(sngame, method = "euclidean")
hclust_sngame <- hclust(d, method="ward.D")
plot(hclust_sngame)


