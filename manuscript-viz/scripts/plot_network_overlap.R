rm(list=ls())

# Load libraries
library(here)
library(dplyr)
library(network)
library(sna)
library(png)
library(grid)
library(ggpubr)
library(gridExtra)
library(gridGraphics)

# Setup
par(mar=c(1,1,1,1)) #for figures
set.seed(1234567)

# Load Objects
eda_env <- readRDS(here("derived_data", "eda_objects.rds"))
ct_net_unique <- eda_env$ct_net_unique
phylo_net <- eda_env$phylo_net
ct_el <- eda_env$ct_el 
ct_el_unique <- eda_env$ct_el_unique 
phylo_el <- eda_env$phylo_el 
common_links <- eda_env$common_links

# Compute overlap between nodes present in both networks
ct_net_ids <- ct_net_unique %v% "vertex.names"
phylo_net_ids <- phylo_net %v% "vertex.names"
common_node_ids <- intersect(ct_net_ids, phylo_net_ids)
unique_ct_net_ids <- setdiff(ct_net_ids, phylo_net_ids)
unique_phylo_net_ids <- setdiff(phylo_net_ids, ct_net_ids)

# Create edge lists with names
el_ct_net_unique <- as.data.frame(as.edgelist(ct_net_unique))
el_ct_net_unique_named <- apply(el_ct_net_unique, 2, function(x) ct_net_ids[x])
el_phylo_net <- as.data.frame(as.edgelist(phylo_net))
el_phylo_net_named <- apply(el_phylo_net, 2, function(x) phylo_net_ids[x])

# Convert common links to data frame
common_edges_df <- data.frame(
  V1 = sapply(strsplit(common_links, "-"), `[`, 1),
  V2 = sapply(strsplit(common_links, "-"), `[`, 2)
)

# Add a common edge indicator to edge lists
ct_el_unique$common <- with(ct_el_unique, paste(V1, V2, sep = "-") %in% common_links)
phylo_el$common <- with(phylo_el, paste(V1, V2, sep = "-") %in% common_links)

# Function to assign colors based on common edges
get_edge_colors <- function(network, common_edges) {
  apply(network, 1, function(edge) {
    if (paste(edge[1], edge[2], sep = "-") %in% common_edges || paste(edge[2], edge[1], sep = "-") %in% common_edges) {
      return("green")  # color for common edges
    } else {
      return("red")    # color for unique edges
    }
  })
}

# Get colors for the edges
ct_edge_colors <- get_edge_colors(ct_el_unique, common_links)
phylo_edge_colors <- get_edge_colors(phylo_el, common_links)

# Get colors for the nodes
ct_node_colors <- ifelse(ct_net_ids %in% common_node_ids, "limegreen", "orange")
phylo_node_colors <- ifelse(phylo_net_ids %in% common_node_ids, "limegreen", "orange")

# Node size
node_size <- 2  # Adjust as necessary

# Plot contact tracing network
png(here("manuscript-viz", "out", "ct_net_plot.png"), width=800, height=800)
gplot(ct_net_unique, gmode="graph", displaylabels = FALSE, 
      vertex.col=ct_node_colors, edge.col=ct_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()

# Plot phylogenetic network
png(here("manuscript-viz", "out", "phylo_net_plot.png"), width=800, height=800)
gplot(phylo_net, gmode="graph", displaylabels = FALSE, 
      vertex.col=phylo_node_colors, edge.col=phylo_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()

# Plot side by side
png(here("manuscript-viz", "out", "side_by_side_plots.png"), width=1600, height=800)
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
gplot(ct_net_unique, gmode="graph", displaylabels = FALSE, 
      vertex.col=ct_node_colors, edge.col=ct_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
gplot(phylo_net, gmode="graph", displaylabels = FALSE, 
      vertex.col=phylo_node_colors, edge.col=phylo_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()


# Plot just the overlapping nodes and edges ---------

# Extract the overlapping nodes
common_node_ids <- intersect(ct_net_ids, phylo_net_ids)

# Extract the overlapping edges
common_edges_df <- data.frame(
  V1 = sapply(strsplit(common_links, "-"), `[`, 1),
  V2 = sapply(strsplit(common_links, "-"), `[`, 2)
)

# Create sub-networks with only overlapping nodes and edges
ct_net_overlap <- network(ct_el_unique[ct_el_unique$common == TRUE, 1:2], directed = FALSE)
phylo_net_overlap <- network(phylo_el[phylo_el$common == TRUE, 1:2], directed = FALSE)

# Set vertex names for overlapping networks
network.vertex.names(ct_net_overlap) <- common_node_ids
network.vertex.names(phylo_net_overlap) <- common_node_ids

# Set colors for nodes and edges
overlap_node_colors <- "limegreen"
overlap_edge_colors <- "green"
node_size <- 2  # Adjust as necessary

# Plot overlapping contact tracing network
png(here("manuscript-viz", "out", "ct_net_overlap_plot.png"), width=800, height=800)
gplot(ct_net_overlap, gmode="graph", displaylabels = FALSE, 
      vertex.col=overlap_node_colors, edge.col=overlap_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()

# Plot overlapping phylogenetic network
png(here("manuscript-viz", "out", "phylo_net_overlap_plot.png"), width=800, height=800)
gplot(phylo_net_overlap, gmode="graph", displaylabels = FALSE, 
      vertex.col=overlap_node_colors, edge.col=overlap_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()

# Plot side by side overlapping networks
png(here("manuscript-viz", "out", "side_by_side_overlap_plots.png"), width=1600, height=800)
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
gplot(ct_net_overlap, gmode="graph", displaylabels = FALSE, 
      vertex.col=overlap_node_colors, edge.col=overlap_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
gplot(phylo_net_overlap, gmode="graph", displaylabels = FALSE, 
      vertex.col=overlap_node_colors, edge.col=overlap_edge_colors, 
      vertex.cex=node_size, edge.lwd=3)
dev.off()



####
# Integrated Plot with Data Flow Diagram
####

# Create a custom divider with a label
create_divider_with_label <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = label, size = 8, fontface = "bold", hjust = 0.5, vjust = 0.5) +
    theme_void()
}

# Load the Data Flow Diagram 
img <- readPNG(here("manuscript-viz", "out", "Data Flow Diagram.png"))
#img <- readPNG(here("manuscript-viz", "out", "Data Flow Diagram high res.png"))
plot_A <- ggplot() +
  annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"))) +
  theme_void()+
  ggtitle("A: Data Flow Diagram")+
  theme(plot.title = element_text(face = "bold", size = 16))  
ggsave(here("manuscript-viz", "out", "Fig2A.png"), plot = plot_A, width = 6, height = 4, dpi = 300)

# Read the contact tracing network plot image
ct_net_img <- readPNG(here("manuscript-viz", "out", "ct_net_plot.png"))
plot_B <- ggplot() +
  annotation_custom(rasterGrob(ct_net_img, width = unit(1,"npc"), height = unit(1,"npc"))) +
  theme_void()+
  ggtitle("B: Contact Tracing Network")+
  theme(plot.title = element_text(face = "bold", size = 16))  
ggsave(here("manuscript-viz", "out", "Fig2B.png"), plot = plot_B, dpi = 300)

# Read the phylogenetic network plot image
phylo_net_img <- readPNG(here("manuscript-viz", "out", "phylo_net_plot.png"))
plot_C <- ggplot() +
  annotation_custom(rasterGrob(phylo_net_img, width = unit(1,"npc"), height = unit(1,"npc"))) +
  theme_void()+
  ggtitle("C: Phylogenetic Network")+
  theme(plot.title = element_text(face = "bold", size = 16))  
ggsave(here("manuscript-viz", "out", "Fig2C.png"), plot = plot_C, dpi = 300)


# Read the correct overlapping networks plot image
overlap_net_img <- readPNG(here("manuscript-viz", "out", "ct_net_overlap_plot.png"))  # Ensure this is the correct overlap plot
plot_D <- ggplot() +
  annotation_custom(rasterGrob(overlap_net_img, width = unit(1,"npc"), height = unit(1,"npc"))) +
  theme_void()+
  ggtitle("D: Overlapping Network")+
  theme(plot.title = element_text(face = "bold", size = 16))  
ggsave(here("manuscript-viz", "out", "Fig2D.png"), plot = plot_D, dpi = 300)


# Create dividers with labels
divider_A <- create_divider_with_label("A")
divider_B <- create_divider_with_label("B")
divider_C <- create_divider_with_label("C")
divider_D <- create_divider_with_label("D")

# Arrange the plots with dividers in a layout
combined_plot <- ggarrange(
  divider_A, plot_A,
  divider_B, plot_B,
  divider_C, plot_C,
  divider_D, plot_D,
  ncol = 1, nrow = 8,  # 4 dividers + 4 plots
  heights = c(0.1, 2, 0.2, 1, 0.2, 1, 0.2, 1)  # Adjust heights (increase divider size or plot size as necessary)
)

# Save the combined plot as a PNG
ggsave(here("manuscript-viz", "out", "combined_4x1_with_dividers.png"), combined_plot, width = 10, height = 25)


# Experiment with a different layout

## Arrange the plots in a 2x2 grid layout
combined_plot <- ggarrange(
  plot_A + ggtitle("A: Data Flow Diagram"),
  plot_B + ggtitle("B: Contact Tracing Network"),
  plot_C + ggtitle("C: Phylogenetic Network"),
  plot_D + ggtitle("D: Overlapping Network"),
  ncol = 2, nrow = 2,
  #labels = c("A", "B", "C", "D"),
  heights = c(1, 1), widths = c(1, 1)
)

# Save the combined plot as a PNG
ggsave(here("manuscript-viz", "out", "combined_2x2_with_titles.png"), combined_plot, width = 20, height = 15)



# Load the Flowchart (High Resolution PNG)
flowchart_img <- img
flowchart_plot <- ggplot() +
  annotation_custom(rasterGrob(flowchart_img, width = unit(1, "npc"), height = unit(1, "npc"))) +
  theme_void() +
  ggtitle("A: Data Flow Diagram") +
  theme(plot.title = element_text(size = 20))

# Arrange Layout
combined_plot <- ggarrange(
  flowchart_plot,
  ggarrange(plot_B, plot_C, plot_D, ncol = 3,
  labels = c("B: Contact Tracing Network", "C: Phylogenetic Network", "D: Overlapping Network")
  ),
  ncol = 1, nrow = 2,
  heights = c(2, 1) # Flowchart takes twice the height of the bottom row
)

# Save the Combined Plot
ggsave(here("manuscript-viz", "out", "combined_plot_1x3.png"), combined_plot, width = 20, height = 18, dpi = 300)

## Figure Alignment ----------

# Create the plot with a title (2A)
flowchart_plot <- ggplot() +
  annotation_custom(rasterGrob(flowchart_img, width = unit(1, "npc"), height = unit(1, "npc"))) +
  theme_void() +
  ggtitle("A: Data Flow Diagram") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5, vjust = -1))

# Save the plot as Figure 2A
ggsave(here("manuscript-viz", "out", "Figure_2A.png"), flowchart_plot, width = 10, height = 10, dpi = 300)


## Figures 2B, C and D:
combined_network_plot <- ggarrange(
  ggarrange(plot_B, plot_C, plot_D, ncol = 1, nrow=3,
  labels = c("B: Contact Tracing Network", "C: Phylogenetic Network", "D: Overlapping Network")
  ),
  heights = c(1, 1, 1) 
)

# Save the combined network plots as Figure 2B-D
ggsave(here("manuscript-viz", "out", "Figure_2B-D.png"), combined_network_plot, width = 10, height = 30, dpi = 300)
