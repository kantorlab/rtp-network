rm(list=ls())

# Load necessary libraries
library(ggplot2)
library(patchwork)
library(here)

# Read the saved RDS file (which contains both p_overall and labels_n)
plot_data <- readRDS(here("manuscript-viz", "out", "p_overall.rds"))
p_combined <- readRDS(here("manuscript-viz", "out", "p_combined_manual_linebreaks.rds"))

overall_labels <- readRDS(here("manuscript-viz", "out", "overall_labels.rds"))
overlapping_labels <- readRDS(here("manuscript-viz", "out", "overlapping_labels.rds"))


# Extract the plot and labels from the list
p_overall <- plot_data$p_overall
labels_n <- plot_data$labels_n

# Add the labels to the plot `p_overall`
p_overall <- p_overall + 
  geom_text(aes(label = labels_n), vjust = -0.5, fontface = "bold") +
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 1.5))  # Background color for top panel

# Read the second plot
p_split_cascade <- readRDS(here("manuscript-viz", "out", "p_split_cascade.rds"))

# Customize background for the second plot
p_split_cascade <- p_split_cascade +
  theme(
    panel.background = element_rect(fill = "gray95", color = "black", linewidth = 1.5)   # Light gray background for bottom panel
  )

# Add a horizontal line separator between panels
divider <- plot_spacer() + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), 
               color = "black", linewidth = 0.5, linetype = "solid")

# Combine the two plots with a divider and extra space
combined_plot <- 
p_overall / divider / p_split_cascade + 
#p_combined / divider / p_split_cascade + 
  plot_layout(ncol = 1, heights = c(1, 0.1, 2)) +  # Adjust the heights for spacer
  plot_annotation(tag_levels = "A") &  
  theme(
    plot.tag = element_text(size = 24, face = "bold"),  # Make the tags larger and bold
    plot.tag.position = c(0.48, 1.02),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

# Display the combined plot
print(combined_plot)

# Combine the new plot `p_combined` with a divider and extra space for testing
new_combined_plot <- p_combined / divider / p_split_cascade + 
  plot_layout(ncol = 1, heights = c(1, 0.1, 2)) +  # Adjust the heights for spacer
  plot_annotation(tag_levels = "A") &  
  theme(
    plot.tag = element_text(size = 24, face = "bold"),  # Make the tags larger and bold
    plot.tag.position = c(0.48, 1.02),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )
new_combined_plot

# Save the combined plot
loc_to_save_combined <- here("manuscript-viz", "out", "combined_cascade_clear.pdf")
ggsave(loc_to_save_combined, plot = combined_plot, 
       width = 12, height = 12, dpi = 300)


loc_to_save_combined <- here("manuscript-viz", "out", "new_combined_cascade_clear.png")
ggsave(loc_to_save_combined, plot = new_combined_plot, 
       width = 14, height = 14, dpi = 300)
