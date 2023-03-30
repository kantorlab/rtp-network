# Set the library path
.libPaths("/gpfs/home/akhann16/code/rtp-network/renv/library/R-4.2/x86_64-pc-linux-gnu")


library(igraph)
library(stringr)

# Read the dependencies_v2.txt file
lines <- readLines("dependencies.txt")

# Extract filenames and their dependencies
filenames <- lines[grepl("File:", lines)]
filenames <- gsub("File: ", "", filenames)
filenames <- gsub("../", "", filenames)
package_deps <- lines[grepl("Package Dependencies:", lines)]
file_deps <- lines[grepl("File Dependencies:", lines)]

# Remove label lines
package_deps <- package_deps[-grep("Package Dependencies:", package_deps)]
file_deps <- file_deps[-grep("File Dependencies:", file_deps)]

# Create a graph from the dependencies
g <- graph.empty(n = length(filenames), directed = TRUE)

# Set vertex attributes
V(g)$name <- filenames
V(g)$type <- "file"

# Add edges for package dependencies
for (i in seq_along(package_deps)) {
  pkgs <- unlist(str_split(package_deps[i], ","))
  pkgs <- str_trim(pkgs)
  if (length(pkgs) > 0 && pkgs[1] != "") {
    for (pkg in pkgs) {
      if (!pkg %in% V(g)$name) {
        g <- g + vertices(pkg, type = "package")
      }
      g <- g + edge(V(g)[filenames[i]], V(g)[pkg])
    }
  }
}

# Add edges for file dependencies
for (i in seq_along(file_deps)) {
  files <- unlist(str_split(file_deps[i], ","))
  files <- str_trim(files)
  if (length(files) > 0 && files[1] != "") {
    for (dep_file in files) {
      g <- g + edge(V(g)[filenames[i]], V(g)[dep_file])
    }
  }
}

# Plot the graph
# png("dependency-graph.png")
# plot(g,
#      vertex.color = ifelse(V(g)$type == "file", "lightblue", "gold"),
#      vertex.size = ifelse(V(g)$type == "file", 30, 20),
#      vertex.shape = ifelse(V(g)$type == "file", "square", "circle"),
#      edge.arrow.size = 0.5,
#      layout = layout.fruchterman.reingold(g),
#      main = "Dependency Graph")
# dev.off()

# ... rest of the script

# png(
#   "dependency_graph.png",
#   width = 800,
#   height = 600,
#   res = 100
# )
# plot(
#   g,
#   layout = layout_nicely(g),
#   vertex.color = "lightblue",
#   vertex.size = 20,
#   vertex.label.dist = 0.5,
#   edge.arrow.size = 1,
#   edge.arrow.width = 1, 
#   edge.color = "black"
# )
# dev.off()

#pdf(file="dependency_graph.pdf", width = 11, height = 8.5)
ggraph(g, layout = "nicely") +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm'), color = "blue") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 1, size = 3.5) +
  theme_void()
#dev.off()

#ggsave("dependency_graph_ggraph.png", width = 10, height = 8, dpi = 100)
ggsave("dependency_graph.pdf", width = 11, height = 8.5)
