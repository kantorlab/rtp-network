rm(list=ls())

library(VennDiagram)

# Define the set counts
total_in_both <- 904
only_in_SGDB <- 1649
only_in_CTDB <- 1514

# Draw the Venn diagram with white font colors and enhanced category labels
venn.plot <- draw.pairwise.venn(
  area1 = total_in_both + only_in_SGDB,
  area2 = total_in_both + only_in_CTDB,
  cross.area = total_in_both,
  category = c("Genomic DB", "Partner DB"),
  lty = "blank",
  fill = c("red", "blue"),
  alpha = 0.50,
  label.col = c("white", "white", "white"),
  label = c(NULL, NULL, ""),
  cex = 1.5,
  fontface = "bold",
  fontfamily = "sans",
  cat.default.pos = "text",
  cat.pos = c(2.5, 4.5),
  cat.dist = c(0.05, 0.05),
  cat.fontfamily = "sans",
  cat.cex = 2,  # Increase this to make category labels larger
  cat.fontface = "bold", # Make category labels bold
  cat.col = "white",
  rotation.degree = 180,
  rotation.centre = c(1.2, 1.2)
)

# Show the plot
grid.draw(venn.plot)

# Manually add custom intersection labels
library(grid)
grid.text("497", x = 0.49, y = 0.44, gp = gpar(fontsize = 18, fontface = "bold", col = "white")) 
grid.text("154", x = 0.49, y = 0.38, gp = gpar(fontsize = 18, fontface = "bold", col = "white"))
