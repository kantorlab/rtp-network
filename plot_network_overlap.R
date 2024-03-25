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

#gplot(ct_net_unique, usearrows = FALSE)
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

# Assuming 'common_links' represents edges present in both networks as "node1-node2"
# Convert 'common_links' into a data frame similar to 'ct_el' and 'phylo_el'
common_edges_df <- data.frame(
  V1 = sapply(strsplit(common_links, "-"), `[`, 1),
  V2 = sapply(strsplit(common_links, "-"), `[`, 2)
)

# Assuming 'ct_el' and 'phylo_el' are your edge lists
# Assuming 'common_edges_df' is formatted correctly as described earlier

# Add a common edge indicator to your edge lists
ct_el$common = with(ct_el, paste(V1, V2, sep = "-") %in% common_links)
phylo_el$common = with(phylo_el, paste(V1, V2, sep = "-") %in% common_links)

# Convert to igraph with the updated edge lists
ct_net_igraph <- graph_from_data_frame(d = ct_el)
ct_net_unique_igraph <- graph_from_data_frame(d = ct_el_unique)
phylo_net_igraph <- graph_from_data_frame(d = phylo_el)

# Use igraph to calculate layout
layout <- create_layout(ct_net_igraph, layout = 'igraph', algorithm = 'fr')

# Plot with updated logic
ggraph(layout) +
  geom_edge_link(aes(color = as.factor(common)), alpha = 0.5) +
  geom_node_point(aes(color = as.factor(name %in% common_node_ids)), size = 3) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "green")) +
  ggtitle("CT Network with Common Elements Highlighted") +
  theme_void()

# Alternate ct_net -------------------------------------------------------------------------

## igraph and ggraph
ct_net_igraph

# Create a vector with edge names from ct_net_igraph
ct_net_edges <- apply(as_edgelist(ct_net_igraph), 1, paste, collapse = "-")

# Check if these edges are in the common edges list
ct_net_common_edges <- ct_net_edges %in% common_links

# Add this information as an edge attribute to the igraph object
E(ct_net_igraph)$common <- ct_net_common_edges

# Plot using the new edge attribute for coloring
ggraph(layout) +
  geom_edge_link(aes(color = common), alpha = 0.8, width = 1) +  # Increase alpha and width
  geom_node_point(aes(color = name %in% common_node_ids), size = 3) +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"))  # Change background color


## with gplot

library(sna)

# Create a function to assign colors based on whether an edge is common or not
get_edge_colors <- function(network, common_edges) {
  apply(network, 1, function(edge) {
    if (paste(edge[1], edge[2], sep = "-") %in% common_edges || paste(edge[2], edge[1], sep = "-") %in% common_edges) {
      return("green")  # color for common edges
    } else {
      return("red")    # color for unique edges
    }
  })
}

# Get the colors for the edges
ct_edge_colors <- get_edge_colors(ct_el, common_links)
phylo_edge_colors <- get_edge_colors(phylo_el, common_links)

# Get the colors for the nodes
ct_node_colors <- ifelse(ct_net_ids %in% common_node_ids, "green", "red")
phylo_node_colors <- ifelse(phylo_net_ids %in% common_node_ids, "green", "red")

# Convert the igraph objects to adjacency matrices
ct_net_matrix <- as.matrix(as_adjacency_matrix(ct_net_igraph))
phylo_net_matrix <- as.matrix(as_adjacency_matrix(phylo_net_igraph))

# Now plot with gplot
gplot(ct_net_matrix, gmode="graph", displaylabels = FALSE, vertex.col=ct_node_colors, edge.col=ct_edge_colors, vertex.cex=0.5, edge.lwd=2)

table(ct_node_colors)



# Alternate using ct_net_unique -------------------------------------------------------------------------

## igraph+gggraph
ct_net_unique_igraph

# Create a vector with edge names from ct_net_unique_igraph
ct_net_unique_edges <- apply(as_edgelist(ct_net_unique_igraph), 1, paste, collapse = "-")

# Check if these edges are in the common edges list
ct_net_unique_common_edges <- ct_net_unique_edges %in% common_links

# Add this information as an edge attribute to the igraph object
E(ct_net_unique_igraph)$common <- ct_net_unique_common_edges

# Plot using the new edge attribute for coloring
ggraph(layout) +
  geom_edge_link(aes(color = common), alpha = 0.8, width = 1) +  # Increase alpha and width
  geom_node_point(aes(color = name %in% common_node_ids), size = 3) +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"))  # Change background color
