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
                                           "Tested", 
                                           "Diagnosed", 
                                           "Sequenced"))

# Color settings
set1_colors <- brewer.pal(6, "Set1")
levels_to_colors <- c("Named", "Attempted", "Reached", "Tested", "Diagnosed", "Sequenced")
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
loc_to_save <- here("manuscript-viz", "overall_cascade.pdf")
ggsave(loc_to_save, plot = p_overall, 
      width = 8, height = 6, dpi = 300)

# RDS for multipanel plot
saveRDS(list(p_overall=p_overall, 
             labels_n=labels_n),
        file = here("manuscript-viz", "p_overall.rds"))

