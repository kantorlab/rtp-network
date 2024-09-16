rm(list=ls())

library(tidyr)
library(ggplot2)
library(here)

# Behavior Data
msm_denom <- 274
pwid_denom <- 26
hrh_denom <- 159

df <- data.frame(
  Grouping = c("MSM", "PWID", "HRH"),
  Named_Mean = c(666/msm_denom, 52/pwid_denom, 279/hrh_denom),
  Contact_Attempted_Mean = c(420/msm_denom, 27/pwid_denom, 196/hrh_denom),
  Reached_Mean = c(383/msm_denom, 26/pwid_denom, 181/hrh_denom),
  Tested_Mean = c(262/msm_denom, 16/pwid_denom, 133/hrh_denom),
  Diagnosed_Mean = c(79/msm_denom, 12/pwid_denom, 31/hrh_denom),
  Sequenced_Mean = c(79/msm_denom, 12/pwid_denom, 31/hrh_denom),
  N = c(paste0("(n=", msm_denom, ")"), paste0("(n=", pwid_denom, ")"), paste0("(n=", hrh_denom, ")"))
)

df_long <- df %>%
  gather(Category, Value, -Grouping, -N)

levels = c("Named_Mean", 
              "Contact_Attempted_Mean",
              "Reached_Mean",
              "Tested_Mean", 
              "Diagnosed_Mean", 
              "Sequenced_Mean"
              )
              

df_long$Category <- 
  factor(df_long$Category, 
    levels = levels
  )

# Race Data
white_denom <- 312
black_denom <- 132
asian_denom <- 15
other_denom <- 28

df_race <- data.frame(
  Grouping = c("White", "Black", "Asian", "Other"),
  Named_Mean = c(740/white_denom, 234/black_denom, 22/asian_denom, 61/other_denom),
  Contact_Attempted_Mean = c(478/white_denom, 142/black_denom, 10/asian_denom, 33/other_denom),
  Reached_Mean = c(439/white_denom, 131/black_denom, 10/asian_denom, 30/other_denom), 
  Tested_Mean = c(311/white_denom, 93/black_denom, 7/asian_denom, 23/other_denom),
  Diagnosed_Mean = c(85/white_denom, 34/black_denom, 3/asian_denom, 7/other_denom),
  Sequenced_Mean = c(85/white_denom, 34/black_denom, 3/asian_denom, 7/other_denom),
  N = c(paste0("(n=", white_denom, ")"), paste0("(n=", black_denom, ")"), paste0("(n=", asian_denom, ")"), paste0("(n=", other_denom, ")"))
)

df_race_long <- df_race %>%
  gather(Category, Value, -Grouping, -N)
df_race_long$Category <- factor(df_race_long$Category, 
  levels = levels)

# Ethnicity Data
hispanic_denom <- 140
nonhispanic_denom <- 354

df_ethnicity <- data.frame(
  Grouping = c("Hispanic", "Not Hispanic"),
  Named_Mean = c(266/hispanic_denom, 806/nonhispanic_denom),
  Contact_Attempted_Mean = c(156/hispanic_denom, 522/nonhispanic_denom),
  Reached_Mean = c(141/hispanic_denom, 481/nonhispanic_denom),
  Tested_Mean = c(99/hispanic_denom, 337/nonhispanic_denom),
  Diagnosed_Mean = c(40/hispanic_denom, 88/nonhispanic_denom),
  Sequenced_Mean = c(40/hispanic_denom, 88/nonhispanic_denom),
  N = c(paste0("(n=", hispanic_denom, ")"), paste0("(n=", nonhispanic_denom, ")"))
)
df_ethnicity_long <- df_ethnicity %>%
  gather(Category, Value, -Grouping, -N)

df_ethnicity_long$Category <- 
  factor(df_ethnicity_long$Category, 
    levels = levels
    )

# Combine All Data
df_long$Type <- "Behavior"
df_race_long$Type <- "Race"
df_ethnicity_long$Type <- "Ethnicity"
combined_df <- rbind(df_long, df_race_long, df_ethnicity_long)

# Plot
p <- ggplot(combined_df, aes(x = Grouping, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = N, y = 0), vjust = 1.5, hjust = 0.5, size = 3.5, fontface="plain", color = "black") +
  labs(y = "Mean Number of Partners per Index Case") +
  scale_fill_brewer(
    palette="Set1", 
    breaks=levels, 
    labels=c("Named", "Attempted", "Reached", 
            "Tested", "Diagnosed", "Sequenced"), name="Cascade Categories")+
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold", size=18, color="blue"),
    axis.title.x = element_text(face="bold", size=18, color="blue"),
    axis.text.y = element_text(),
    legend.text = element_text(size=14),
    legend.title = element_text(size = 16, face = "bold", color = "blue"), 
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 1)
  ) +
  facet_wrap(~Type, scales = "free_x", ncol = 3) +
  ylim(0, max(combined_df$Value) + 0.1*max(combined_df$Value))

loc_to_save <- here("manuscript-viz", "split_cascade.pdf")
print(p)
ggsave(loc_to_save, plot = p, 
      width = 12, height = 6, dpi = 300)

p_split <- p

saveRDS(p_split, file = here("manuscript-viz", "p_split_cascade.rds"))
