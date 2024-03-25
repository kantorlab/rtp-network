rm(list=ls())


# Load libraries ---------------------------

library(here)
library(dplyr)
library(network)
library(sna)


# Setup ---------------------------

par(mar=c(1,1,1,1)) #for figures
ls()

### 


# Set seed ---------------------------

set.seed(1234567)


# Load Objects ---------------------------

## rds object
eda_env <- readRDS("eda_objects.rds")
names(eda_env)

## networks
ct_net_unique <- eda_env$ct_net_unique
phylo_net <- eda_env$phylo_net

## edge lists
ct_el <- eda_env$ct_el 
ct_el_unique <- eda_env$ct_el_unique 
phylo_el <- eda_env$phylo_el 

## common edge lists
common_links <- eda_env$common_links

## query objects
ct_net_unique
phylo_net

head(ct_el_unique)
head(phylo_el)


# Plot the two networks ---------------------------

#gplot(ct_net_unique, usearrows = TRUE)
#gplot(phylo_net, usearrows = FALSE)


# Compute overlap between nodes present in both networks ---------------------------

ct_net_ids <- ct_net_unique %v% "vertex.names"
phylo_net_ids <- phylo_net %v% "vertex.names"

common_node_ids <- intersect(ct_net_ids, phylo_net_ids)
length(common_node_ids)

unique_ct_net_ids <- setdiff(ct_net_ids, phylo_net_ids)
length(unique_ct_net_ids)

unique_phylo_net_ids <- setdiff(phylo_net_ids, ct_net_ids)
length(unique_phylo_net_ids)


# Load overlap between nodes present in both networks ---------------------------

el_ct_net_unique <- as.data.frame(as.edgelist(ct_net_unique))
el_ct_net_unique_named <- apply(el_ct_net_unique, 2, function(x) ct_net_ids[x])

el_phylo_net <- as.data.frame(as.edgelist(phylo_net))
el_phylo_net_named <- apply(el_phylo_net, 2, function(x) phylo_net_ids[x])

head(el_ct_net_unique_named)
head(el_phylo_net_named)
common_links

## common_links' represents edges present in both networks as "node1-node2"
## convert 'common_links' into a data frame similar to 'ct_el' and 'phylo_el'

common_edges_df <- data.frame(
  V1 = sapply(strsplit(common_links, "-"), `[`, 1),
  V2 = sapply(strsplit(common_links, "-"), `[`, 2)
)


# Add a common edge indicator to your edge lists
ct_el_unique$common = with(ct_el_unique, paste(V1, V2, sep = "-") %in% common_links)
phylo_el$common = with(phylo_el, paste(V1, V2, sep = "-") %in% common_links)

# Plot contact tracing network with common elements -------------------------------------------------------------------------

ct_net_unique

# function to assign colors based on whether an edge is common or not
get_edge_colors <- function(network, common_edges) {
  apply(network, 1, function(edge) {
    if (paste(edge[1], edge[2], sep = "-") %in% common_edges || paste(edge[2], edge[1], sep = "-") %in% common_edges) {
      return("green")  # color for common edges
    } else {
      return("red")    # color for unique edges
    }
  })
}

# get the colors for the edges
ct_edge_colors <- get_edge_colors(ct_el_unique, common_links)
phylo_edge_colors <- get_edge_colors(phylo_el, common_links)

# Get the colors for the nodes
ct_node_colors <- ifelse(ct_net_ids %in% common_node_ids, "green", "red")
phylo_node_colors <- ifelse(phylo_net_ids %in% common_node_ids, "green", "red")

# Convert the network objects to adjacency matrices

# Now plot with gplot
# gplot(ct_net_matrix, gmode="graph", displaylabels = FALSE, 
#       vertex.col=ct_node_colors, edge.col=ct_edge_colors, 
#       vertex.cex=0.5, edge.lwd=2)

table(ct_node_colors)

## increase node size
node_size <- 1.5  # Adjust as necessary

## define bright colors for nodes
bright_node_colors <- ifelse(ct_net_ids %in% common_node_ids, "limegreen", "orange")

## plot contact tracing network 
set.seed(1234567)
gplot(ct_net_unique, gmode="graph", displaylabels = FALSE, 
      vertex.col=bright_node_colors, edge.col=ct_edge_colors, 
      vertex.cex=node_size, edge.lwd=4)


## plot phylo network 
phylo_net


bright_node_colors_phylo <- ifelse(phylo_net_ids %in% common_node_ids, "limegreen", "orange")

set.seed(1234567)
gplot(phylo_net, 
      gmode="graph", 
      displaylabels = FALSE, 
      vertex.col=bright_node_colors_phylo, 
      edge.col=phylo_edge_colors, 
      vertex.cex=node_size, 
      edge.lwd=4)


