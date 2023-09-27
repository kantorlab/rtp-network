rm(list=ls())

library(VennDiagram)

# Define the set counts
total_in_both <- 904
only_in_SGDB <- 1649
only_in_CTDB <- 1514

# Draw the Venn diagram
venn.plot <- draw.pairwise.venn(
  area1 = total_in_both + only_in_SGDB,
  area2 = total_in_both + only_in_CTDB,
  cross.area = total_in_both,
  category = c("SGDB", "CTDB"),
  lty = "blank",
  fill = c("red", "blue"),
  alpha = 0.50,
  label.col = c("black", "black", "black"),
  cex = 1.5,
  fontface = "bold",
  fontfamily = "sans",
  cat.default.pos = "text",
  cat.pos = c(0, 0),
  cat.dist = c(0.22, 0.22),
  cat.fontfamily = "sans",
  rotation.degree = 180,
  rotation.centre = c(1.2, 1.2)
)

# Show the plot
grid.draw(venn.plot)
