rm(list=ls())

library(VennDiagram)
library(grid)

# Define the set counts
total_in_both <- 904
only_in_SGDB <- 1649
only_in_CTDB <- 1514

# Draw the Venn diagram with default intersection label
venn.plot <- draw.pairwise.venn(
  area1 = total_in_both + only_in_SGDB,
  area2 = total_in_both + only_in_CTDB,
  cross.area = total_in_both,
  category = c("Genomic DB", "Partner DB"),
  lty = "blank",
  fill = c("red", "blue"),
  alpha = 0.50,
  label.col = c("white", "white", "white"),
  cex = 1.5,
  fontface = "bold",
  fontfamily = "sans",
  cat.default.pos = "text",
  cat.pos = c(2.5, 4.5),
  cat.dist = c(0.05, 0.05),
  cat.fontfamily = "sans",
  cat.cex = 2,
  cat.fontface = "bold",
  cat.col = "white",
  rotation.degree = 180,
  rotation.centre = c(1.2, 1.2)
)

# Overlay black rectangles behind all numbers
grid.rect(x = 0.49, y = 0.50, width=0.08, height=0.05, gp = gpar(fill = "black", col = NA))
grid.rect(x = 0.49, y = 0.42, width=0.08, height=0.05, gp = gpar(fill = "black", col = NA))
grid.rect(x = 0.49, y = 0.34, width=0.08, height=0.05, gp = gpar(fill = "black", col = NA))

# Manually add custom intersection labels with increased font size
grid.text("904", x = 0.49, y = 0.50, gp = gpar(fontsize = 24, fontface = "bold", col = "white")) 
grid.text("497", x = 0.49, y = 0.42, gp = gpar(fontsize = 24, fontface = "bold", col = "white")) 
grid.text("154", x = 0.49, y = 0.34, gp = gpar(fontsize = 24, fontface = "bold", col = "white"))

# Draw downward arrows with increased length
arrow_spec <- arrow(type="closed", length = unit(0.15, "inches"))
grid.lines(x = c(0.49, 0.49), y = c(0.48, 0.44), arrow = arrow_spec, gp = gpar(lwd = 2))
grid.lines(x = c(0.49, 0.49), y = c(0.40, 0.36), arrow = arrow_spec, gp = gpar(lwd = 2))

