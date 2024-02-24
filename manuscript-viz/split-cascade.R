library(tidyr)
library(ggplot2)

# Behavior Data
df <- data.frame(
  Grouping = c("MSM", "PWID", "HRH"),
  Named_Mean = c(794/275, 70/26, 318/160),
  Tested_Mean = c(262/275, 16/26, 134/160),
  Diagnosed_Mean = c(81/275, 12/26, 31/160),
  Sequenced_Mean = c(79/275, 12/26, 31/160),
  N = c("(n=275)", "(n=26)", "(n=160)")
)
df_long <- df %>%
  gather(Category, Value, -Grouping, -N)
df_long$Category <- factor(df_long$Category, levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))

# Race Data
df_race <- data.frame(
  Grouping = c("White", "Black", "Asian", "Other"),
  Named_Mean = c(861/313, 273/133, 23/16, 72/28),
  Tested_Mean = c(311/313, 93/133, 8/16, 23/28),
  Diagnosed_Mean = c(88/313, 34/133, 3/16, 7/28),
  Sequenced_Mean = c(86/313, 34/133, 3/16, 7/28),
  N = c("(n=313)", "(n=133)", "(n=16)", "(n=28)")
)
df_race_long <- df_race %>%
  gather(Category, Value, -Grouping, -N)
df_race_long$Category <- factor(df_race_long$Category, levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))

# Ethnicity Data
df_ethnicity <- data.frame(
  Grouping = c("Hispanic", "Not Hispanic"),
  Named_Mean = c(322/140, 928/357),
  Tested_Mean = c(99/140, 338/357),
  Diagnosed_Mean = c(40/140, 91/357),
  Sequenced_Mean = c(40/140, 89/357),
  N = c("(n=140)", "(n=357)")
)
df_ethnicity_long <- df_ethnicity %>%
  gather(Category, Value, -Grouping, -N)
df_ethnicity_long$Category <- factor(df_ethnicity_long$Category, levels = c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"))

# Combine All Data
df_long$Type <- "Behavior"
df_race_long$Type <- "Race"
df_ethnicity_long$Type <- "Ethnicity"
combined_df <- rbind(df_long, df_race_long, df_ethnicity_long)

# Plot
p <- ggplot(combined_df, aes(x = Grouping, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = N, y = 0), vjust = 1.5, hjust = 0.5, size = 8, fontface="bold", color = "black") +
  labs(y = "Mean Number of Partners per Index Case") +
  #scale_fill_brewer(palette="Set1", breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"), labels=c("Named", "Tested", "Diagnosed", "Sequenced"), name="Category") +
  scale_fill_brewer(palette="Set1", breaks=c("Named_Mean", "Tested_Mean", "Diagnosed_Mean", "Sequenced_Mean"), labels=c("Named", "Tested", "Diagnosed", "Sequenced"), name=NULL)+
  theme_minimal() +
  theme(
    #plot.title = element_text(size = 18, face = "bold"),
    plot.title = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_blank(),
    #axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    legend.text = element_text(size = 12, face = "bold"),
    #legend.title = element_text(size = 16),
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 1)
  ) +
  facet_wrap(~Type, scales = "free_x", ncol = 3) +
  ylim(0, max(combined_df$Value) + 0.1*max(combined_df$Value))

#pdf("cascade-breakdown.pdf", width=28, height=12)
print(p)
#dev.off()
