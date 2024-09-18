rm(list=ls())
library(eulerr)

# Define the sets and their intersections
counts <- c(
  "Partner DB" = 1514, 
  "Genomic DB" = 1649, 
  "Genomic DB&Partner DB" = 904)

# Draw the Euler diagram with enhanced quantities
euler_plot <-
  plot(euler(counts),
     quantities = TRUE,
     fills = c("red", "blue"),
     ptext.cex = 3.0,        # Increase font size
     ptext.col = "black",   # Change font color to black
     ptext.fontface = "bold" # Make text bold
)

print(euler_plot)