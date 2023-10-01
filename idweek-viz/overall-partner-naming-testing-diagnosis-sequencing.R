# Loading necessary libraries
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Structuring the data with the average values
overall_data <- data.frame(
  Category = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
  Value = c(2.7, 338/497, 154/497, 152/497)
)

# Setting the order of the factor levels for Category
overall_data$Category <- factor(overall_data$Category, 
                                levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))

# Plot for overall data
# ... (previous code)

# Plot for overall data
# p_overall <- ggplot(overall_data, aes(x = Category, y = Value, fill = Category)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Mean Number of Partners Overall",
#        y = "Mean Number of Partners",
#        x = "") +
#   scale_fill_brewer(palette="Set1", 
#                     breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
#                     labels=c("Named", "Tested", "Diagnosed", "Sequenced"),
#                     name="Category") +
#   scale_y_continuous(limits = c(0, 3)) + # Replace 0 and 1 with the range of the race/ethnicity/behavior plot
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 18, face = "bold"),
#     axis.title.x = element_blank(),
#     axis.title.y = element_text(size = 16),
#     axis.text.x = element_text(size = 14, face = "bold"),  # Removed the blue color
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.title = element_text(size = 16)
#   )
# 
# print(p_overall)

# ... (previous code)

# Plot for overall data
p_overall <- ggplot(overall_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       y = "Mean Number of Partners",
       x = "") +
  scale_fill_brewer(palette="Set1", 
                    breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
                    labels=c("Named", "Tested", "Diagnosed", "Sequenced"),
                    name="Category") +
  scale_y_continuous(limits = c(0, 3)) + # Replace 0 and 1 with the range of the race/ethnicity/behavior plot
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_blank(),   # This will remove the x-axis labels
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

print(p_overall)
