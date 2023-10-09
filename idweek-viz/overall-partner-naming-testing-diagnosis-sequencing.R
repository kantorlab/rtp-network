rm(list=ls())

# Loading necessary libraries
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Structuring the data with the average values
overall_data <- data.frame(
  Category = c("Named_Mean", "Contacted_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
  Value = c(1342/497, 880/497, 338/497, 152/497, 152/497)
)

# Adjusting the factor levels and labels for the Category column
overall_data$Category <- factor(overall_data$Category, 
                                levels = c("Named_Mean", 
                                           "Contacted_Mean",
                                           "Tested_Mean",
                                           "Diagnosed_Mean",
                                           "Sequenced_Mean"),
                                labels = c("Named", 
                                           "Contacted",
                                           "Tested", 
                                           "Diagnosed", 
                                           "Sequenced"))

# Color settings
set1_colors <- brewer.pal(8, "Set1")
my_colors <- c("Named" = set1_colors[1], 
               "Contacted" = set1_colors[2],
               "Tested" = set1_colors[3],     
               "Diagnosed" = set1_colors[4],  
               "Sequenced" = set1_colors[5]) 


# Create the labels
labels_n = c("n=1342", "n=880", "n=338", "n=152", "n=152")

# Updated plot code
p_overall <- ggplot(overall_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=labels_n), vjust=-0.5, size=6, fontface="bold") +
  labs(title = "",
       y = "Mean Number of Partners per Index Case",
       x = "") +
  scale_fill_manual(values=my_colors) +  # Setting colors manually
  scale_y_continuous(limits = c(0, 3)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18, face = "bold", color = "black"),
    axis.text.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 18, face = "bold", color = "black"),
    legend.position = "none"   # This will remove the legend
  )

print(p_overall)
