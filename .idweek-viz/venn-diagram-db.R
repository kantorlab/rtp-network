rm(list=ls())

rm(list=ls())

library(VennDiagram)
library(grid)

# Define the set counts
total_in_both <- 904
only_in_SGDB <- 1649
only_in_CTDB <- 1514

# Draw the Venn diagram
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
