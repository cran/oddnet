## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(oddnet)
library(igraph)

## ----readdata-----------------------------------------------------------------
set.seed(1)
networks <- list()
p.or.m.seq <- seq(from = 0.01, to = 0.2, length.out = 100)
p.or.m.seq[50] <- p.or.m.seq[50] + 0.2  # anomalous network
 for(i in 1:100){
  gr <- igraph::erdos.renyi.game(100, p.or.m = p.or.m.seq[i])
  networks[[i]] <- igraph::as_adjacency_matrix(gr)
}

## ----plotnetworks-------------------------------------------------------------

# Plotting
network_1 <- networks[[1]]
gr <- igraph::graph_from_adjacency_matrix(network_1)
plot(gr, 
     layout = layout_with_fr, 
     vertex.size=3, 
     vertex.label=NA,
     edge.arrow.size=0.2, 
     main = "Network 1")



network_30 <- networks[[30]]
gr <- igraph::graph_from_adjacency_matrix(network_30)
plot(gr, 
     layout = layout_with_fr, 
     vertex.size=3, 
     vertex.label=NA,
     edge.arrow.size=0.2, 
     main = "Network 30")


network_80 <- networks[[80]]
gr <- igraph::graph_from_adjacency_matrix(network_80)
plot(gr, 
     layout = layout_with_fr, 
     vertex.size=3, 
     vertex.label=NA,
     edge.arrow.size=0.2, 
     main = "Network 80")

## ----plotanomaly--------------------------------------------------------------

# Plotting
network_a <- networks[[50]]
gr <- igraph::graph_from_adjacency_matrix(network_a)
plot(gr, 
     layout = layout_with_fr, 
     vertex.size=3, 
     vertex.label=NA,
     edge.arrow.size=0.2, 
     main = "Network 50 - Anomaly")

## ----degreedist---------------------------------------------------------------
network_80 <- networks[[80]]
gr <- igraph::graph_from_adjacency_matrix(network_80)
hist(degree(gr))

network_a <- networks[[50]]
gr <- igraph::graph_from_adjacency_matrix(network_a)
hist(degree(gr))


## ----avgdeg-------------------------------------------------------------------
avg_deg <- rep(0, 100)
max_deg <- rep(0, 100)
for(i in 1:100){
  network <- networks[[i]]
  gr <- igraph::graph_from_adjacency_matrix(network)
  avg_deg[i] <- mean(degree(gr))
  max_deg[i] <- max(degree(gr))
}
plot(avg_deg, type = "l")
plot(max_deg, type = "l")

## ----anomalous----------------------------------------------------------------
anomalous_networks(networks)

