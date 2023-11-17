rm(list=ls())
library(ggplot2)
library(tidyr)

# Sample data in long format
data <- data.frame(
  Subcategory = c("Male", "Female", "Non-Binary"),
  Persons_in_SDB = c(71.4, 26.1, 0.9),
  Interviewed_Persons = c(78.8, 20.0, 1.2),
  Named_Partners = c(78.3, 21.1, 0.6),
  Partners_Characteristics = c(84.4, 14.3, 1.3)
)

data_long <- tidyr::pivot_longer(data, cols = -Subcategory, names_to = "variable", values_to = "value")

ggplot(data_long, aes(fill=Subcategory, y=value, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Gender Identity Across Various Metrics",
       x="Metric", y="Percentage") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1")


