library(tidyr)
library(ggplot2)


# Behavior

# Adjusted sample data based on the information provided
df <- data.frame(
  Grouping = c("MSM", "PWID", "HRH"),
  Named_Mean = c(794/275, 70/26, 318/160),  # Mean number of named partners
  Tested_Mean = c(262/275, 16/26, 134/160),  # Mean number of tested partners
  Diagnosed_Mean = c(81/275, 12/26, 31/160),  # Mean number of diagnosed partners in SDB
  Sequenced_Mean = c(79/275, 12/26, 31/160)  # Mean number of sequenced partners in SDB
)


df_long$Category <- factor(df_long$Category, 
                           levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))




# Plot
ggplot(df_long, aes(x = factor(Grouping, levels=c("MSM", "PWID", "HRH")), 
                    y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Number of Partners by Index Cases Providing Partner Data",
       y = "Mean Number of Partners",
       x = "Behavior Grouping") +
  scale_fill_brewer(palette="Set1", 
                    breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
                    labels=c("Named", "Tested", "Diagnosed", "Sequenced")) +
  theme_minimal()


# Race

df_race <- data.frame(
  Grouping = c("White", "Black", "Asian", "Other"),
  Named_Mean = c(861/313, 273/133, 23/16, 72/28),
  Tested_Mean = c(311/313, 93/133, 8/16, 23/28),
  Diagnosed_Mean = c(88/313, 34/133, 3/16, 7/28),
  Sequenced_Mean = c(86/313, 34/133, 3/16, 7/28)
)

df_race_long <- df_race %>%
  gather(Category, Value, -Grouping)

df_race_long$Category <- factor(df_race_long$Category, 
                                levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))



ggplot(df_race_long, aes(x = Grouping, 
                         y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Number of Partners by Race",
       y = "Mean Number of Partners",
       x = "Race") +
  scale_fill_brewer(palette="Set1", 
                    breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
                    labels=c("Named", "Tested", "Diagnosed", "Sequenced")) +
  theme_minimal()



## Ethnicity


df_ethnicity <- data.frame(
  Grouping = c("Hispanic", "Not Hispanic"),
  Named_Mean = c(322/140, 928/357),
  Tested_Mean = c(99/140, 338/357),
  Diagnosed_Mean = c(40/140, 91/357),
  Sequenced_Mean = c(40/140, 89/357)
)

df_ethnicity_long <- df_ethnicity %>%
  gather(Category, Value, -Grouping)

df_ethnicity_long$Category <- factor(df_ethnicity_long$Category, 
                                     levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))

# Plot for Ethnicity
ggplot(df_ethnicity_long, aes(x = Grouping, 
                              y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Number of Partners by Ethnicity",
       y = "Mean Number of Partners",
       x = "Ethnicity") +
  scale_fill_brewer(palette="Set1", 
                    breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"),
                    labels=c("Named", "Tested", "Diagnosed", "Sequenced")) +
  theme_minimal()


