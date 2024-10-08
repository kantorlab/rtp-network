rm(list=ls())

# Loading necessary libraries
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(here)

# Structuring the data with the average values
overall_data <- data.frame(
  Category = c("Named_Mean", "Contact_Attempted_Mean", "Reached_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
  Value = c(1342, 880, 807, 570, 152, 152)
)

# Adjusting the factor levels and labels for the Category column
overall_data$Category <- factor(overall_data$Category, 
                                levels = c("Named_Mean", 
                                           "Contact_Attempted_Mean",
                                           "Reached_Mean",
                                           "Tested_Mean",
                                           "Diagnosed_Mean",
                                           "Sequenced_Mean"),
                                labels = c("Named", 
                                           "Attempted",
                                           "Reached",
                                           "Newly Tested", 
                                           "Diagnosed", 
                                           "Sequenced"))

# Color settings
set1_colors <- brewer.pal(6, "Set1")
levels_to_colors <- c("Named", "Attempted", "Reached", "Newly Tested", "Diagnosed", "Sequenced")
my_colors <- setNames(set1_colors, levels_to_colors)


# Create the labels
labels_n = c("1342", "880 (66%)", "807(92%)", 
            "570 (71%)", "152 (27%)", "152 (100%)")

# Updated plot code
p_overall <- ggplot(overall_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=labels_n), vjust=-0.5, 
            #size=6, 
            fontface="bold") +
  labs(title = "",
       y = "Total Partners",
       x = "Cascade Categories") +
  scale_fill_manual(values=my_colors) +  # Setting colors manually
  scale_y_continuous(limits = c(0, 1500)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold", color = "blue"),
    axis.title.y = element_text(size = 18, face = "bold", color = "blue"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "none"   # This will remove the legend
  )

print(p_overall)

# Save plot
loc_to_save <- here("manuscript-viz", "out", "overall_cascade.pdf")
ggsave(loc_to_save, plot = p_overall, 
      width = 8, height = 6, dpi = 300)

# RDS for multipanel plot
saveRDS(list(p_overall=p_overall, 
             labels_n=labels_n),
        file = here("manuscript-viz", "out", "p_overall.rds"))

###
## Add bars to show cascade among overlapping persons as well
###

# The same as before
overall_data <- data.frame(
  Category = c("Named_Mean", "Contact_Attempted_Mean", "Reached_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
  Value = c(1342, 880, 807, 570, 152, 152)
)

overlapping_data <- data.frame(
  Category = c("Named_Mean", "Contact_Attempted_Mean", "Reached_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
  Value = c(1056, 687, 631, 446, 121, 121)
)

# Explicitly add "\n" for manual line breaks
overall_labels <- c("1342", "880\n(66%)", "807\n(92%)", "570\n(71%)", "152\n(27%)", "152\n(100%)")
overlapping_labels <- c("1056", "687\n(65%)", "631\n(92%)", "446\n(71%)", "121\n(27%)", "121\n(100%)")

# Combine both datasets
combined_data <- rbind(
  cbind(overall_data, Group = "Overall"),
  cbind(overlapping_data, Group = "Overlapping")
)

# Adjust the factor levels and labels for better ordering in the plot
combined_data$Category <- factor(combined_data$Category, 
                                 levels = c("Named_Mean", 
                                            "Contact_Attempted_Mean",
                                            "Reached_Mean",
                                            "Tested_Mean",
                                            "Diagnosed_Mean",
                                            "Sequenced_Mean"),
                                 labels = c("Named", 
                                            "Attempted",
                                            "Reached",
                                            "Newly Tested", 
                                            "Diagnosed", 
                                            "Sequenced"))

                                            
# Updated plot with explicit line breaks
p_combined <- ggplot(combined_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(aes(alpha = Group), stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = ifelse(Group == "Overall", overall_labels, overlapping_labels)), 
            vjust = -0.5, fontface = "bold", size = 3.5,  # Adjust size for better fit
            hjust = ifelse(combined_data$Group == "Overall", 1.2, -0.2), 
            position = position_dodge(width = 0.8)) +  
  labs(title = "",
       y = "Total Partners",
       x = "Cascade Categories") +
  scale_fill_manual(values = set1_colors) +  # Keep Set1 colors
  scale_alpha_manual(values = c(1, 0.6)) +   # Use alpha to differentiate between Overall and Overlapping
  scale_y_continuous(limits = c(0, 1500)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold", color = "blue"),
    axis.title.y = element_text(size = 18, face = "bold", color = "blue"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "none"   # Remove legend since the Group is represented with alpha
  )

print(p_combined)

# Save updated plot
loc_to_save_combined <- here("manuscript-viz", "out", "combined_cascade_manual_linebreaks.pdf")
ggsave(loc_to_save_combined, plot = p_combined, 
       width = 8, height = 6, dpi = 300)

saveRDS(p_combined, file = here("manuscript-viz", "out", "p_combined_manual_linebreaks.rds"))
saveRDS(overall_labels, file = here("manuscript-viz", "out", "overall_labels.rds"))
saveRDS(overlapping_labels, file = here("manuscript-viz", "out", "overlapping_labels.rds"))
